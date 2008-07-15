::xo::library doc {
  XoWiki Workflow - main library classes and objects

  @author Gustaf Neumann
  @creation-date 2008-03-05
  @cvs-id $Id $
}

# Todo:
# - validation of template and form_constraints DONE
# - plain wiki pages DONE
#
# - after import, references are not updated 
#   (same for plain references); after_import methods?
#
# - long-term issue: import with categories 
#   (import/export of category tree 
#    + category mappings, which have to be remapped to the new IDs)
#
# - Roles
# - assignment
# - workflow-assingnment includelet (over multiple workflows and 
#   package instances)

::xo::db::require package xowiki

namespace eval ::xowf {
  ::xo::PackageMgr create ::xowf::Package \
      -package_key "xowf" -pretty_name "XoWiki Workflow" \
      -superclass ::xowiki::Package
  
  Package ad_instproc initialize {} {
    mixin ::xowf::WorkflowPage to every FormPage
  } {
    #my msg ""
    # called, whenever package is initialized 
    next
    ::xowiki::FormPage instmixin add ::xowf::WorkflowPage
  }
  Package ad_instproc destroy {} {
    remove mixin
  } {
    ::xowiki::FormPage instmixin delete ::xowf::WorkflowPage
    next
  }

  #
  # Workflow Context
  #
  
  Class Context -parameter {
    {current_state "[self]::initial"} 
    workflow_definition
    object
    {all_roles false}
    in_role
  }
  
  # forward property to the workflow object
  Context instforward property {%my object} %proc
  Context instforward set_property {%my object} %proc
  Context instforward get_property {%my object} %proc

  # forward form_constraints, view_method and for the to current state object
  Context instforward get_form_constraints {%my current_state} form_constraints
  Context instforward get_view_method      {%my current_state} view_method
  Context instforward form                 {%my current_state} form
  Context instforward form_loader          {%my current_state} form_loader

  Context instproc set_current_state {value} {
    my current_state [self]::$value
  }
  Context instproc get_current_state {} {
    namespace tail [my current_state]
  }

  Context instproc get_actions {} {
    set actions [list]
    #my msg "for [my current_state] actions '[[my current_state] get_actions]'"
    foreach action [[my current_state] get_actions] {
      lappend actions [self]::$action
    }
    return $actions
  }
  Context instproc defined {what} {
    set result [list]
    foreach c [my info children] {if {[$c istype $what]} {lappend result $c}}
    return $result
  }

  Context instproc resolve_form_name {name parent_id} {
    set form_id [::xo::db::CrClass lookup -name $name -parent_id $parent_id]
    if {!$form_id} {
      set lang ""; set stripped_name $name
      regexp {^(..):(.*)$} $name _ lang stripped_name
      set package_id [[my object] package_id]
      if {$lang eq ""} {
        # if no lang is given, use default_locale.
        # This allows for multi-language workflow instances.
        set lang [$package_id default_language]
        set form_id [::xo::db::CrClass lookup -name ${lang}:${stripped_name} -parent_id $parent_id]
        if {!$form_id} {
          # Maybe we are configured to use the connection locale. 
          # In this case, use the system locale as last ressort
          set system_locale [lang::system::locale -package_id $package_id]
          set system_lang [string range $system_locale 0 1]
          if {$system_lang ne $lang} {
            set lang $system_lang
            set form_id [::xo::db::CrClass lookup -name ${lang}:${stripped_name} -parent_id $parent_id]
          }
        }
      }
      set name ${lang}:${stripped_name} 
    }
    #my msg result=[list form_id $form_id name $name]
    return [list form_id $form_id name $name]
  }

  Context instproc form_id {parent_id} {
    # After this method is activated, the form object of the form of
    # the current state is created and the instance variable form_id
    # is set.
    #
    # Load the actual form only once for this context.  We cache the
    # form_id in the context.
    #
    if {[my exists form_id]} {return [my set form_id]}
    # 
    # We have to load the form, maybe via form loader.  If the
    # form_loader is set and the method exists, then use the form
    # loader instead of the plain lookup. In case the form_loader
    # fails, it is supposed to return 0.
    #
    set loader [my form_loader]
    if {$loader eq "" || [my info methods $loader] eq ""} {
      array set "" [my resolve_form_name [my form] $parent_id]
      set form_id $(form_id)
    } else {
      #my msg "using loader for [my form]"
      set form_id [my $loader [my form]]
      #my msg form_id=$form_id
    }
    if {$form_id == 0} {
      #  todo: handle case, where form does not exist
      my msg "cannot fetch form '[my form]' from folder $parent_id"
    }
    my set form_id $form_id
  }

  Context instproc init {} {
    my destroy_on_cleanup
    my array set handled_roles {}
    #
    # We define the classes Action, State and Property per workflow
    # instance.  This has the advantage that we can provide instprocs
    # or parameters per workflow definition without the danger to
    # interfere with other Workflows
    #
    regsub -all \r\n [my workflow_definition] \n workflow_definition
    my contains "
       Class create Action   -superclass  ::xowf::Action
       Class create State    -superclass  ::xowf::State
       Class create Condition -superclass ::xowf::Condition
       Class create Property -superclass  ::xowf::Property -set abstract 1
       $workflow_definition"
    if {[my all_roles]} {
      #my msg want.to.create=[my array names handled_roles]
      foreach role [my array names handled_roles] {
        Context create [self]-$role -workflow_definition $workflow_definition \
            -in_role $role -object [my object]
      }
    }
  }

  Context proc require {obj} {
    set name $obj-wfctx
    if {![my isobject ::$name]} {
      [self] create $name -destroy_on_cleanup -object $obj \
          -workflow_definition [$obj wf_property workflow_definition]
      set state [$obj state]
      if {$state eq ""} {set state initial}
      $name set_current_state $state
    }
    return ::$name
  }

  Context instproc draw_arc {from_state next_state action label style} {
    if {$next_state eq ""} {set next_state $from_state}
    set key transition($from_state,$next_state,$action)
    if {[my exists $key]} {return ""}
    my set $key 1
    return "  state_$from_state -> state_$next_state \[label=\"$label\"$style\];\n"
  }
  Context instproc draw_transition {from action role} {
    #my msg "[self args]"
    set cond_values [$action get_cond_values [$action next_state]]
    set result ""
    if {[llength $cond_values]>2} {
      # we have conditional values
      set c cond_[$from name]_[my incr condition_count]
      set arc_style {,style="setlinewidth(1)",color=gray}
      append result "  state_$c \[shape=diamond, fixedsize=1, width=0.2, height=0.2, fixedsize=1,style=solid,color=gray,label=\"\"\];\n"
      append result [my draw_arc [$from name] $c [$action name]-1 $role[$action label] ""]
      foreach {cond value} $cond_values {
	if {$cond ne ""} {set prefix "$cond"} {set prefix "else"}
	append result [my draw_arc $c $value [$action name] \[$prefix\] $arc_style] 
      }
    } else {
      set prefix ""
      append result [my draw_arc [$from name] [lindex $cond_values 1] [$action name] $role$prefix[$action label] ""]
    }
    return $result
  }
  
  Context instproc as_graph {{-current_state ""} {-visited ""} {-dpi 96} {-style "width:100%"}} {
    set dot ""
    catch {set dot [::util::which dot]}
    # final ressort for cases, where ::util::which is not available
    if {$dot eq "" && [file executable /usr/bin/dot]} {set dot /usr/bin/dot}
    if {$dot eq ""} {return "<font color='red'>Program 'dot' is not available! No graph displayed.</font>"}
    set obj_id [namespace tail [my object]]
    set result [subst {digraph workflow_$obj_id \{
      dpi = $dpi;
      node \[shape=doublecircle, margin=0.001, fontsize=8, fixedsize=1, width=0.4, style=filled\]; start;
      node \[shape=ellipse, fontname="Courier", color=lightblue2, style=filled, 
	fixedsize=0, fontsize=10, margin=0.06\];
      edge \[fontname="Courier", fontsize=9\];
    }]
    foreach s [my defined State] {
      if {[$s name] eq $current_state} {
        set color ",color=orange"
      } elseif {[lsearch -exact $visited [$s name]] > -1} {
        set color ",color=yellow"
      } else {
        set color ""
      }
      append result "  state_[$s name] \[label=\"[$s label]\"$color\];\n"
    }
    append result "start->state_initial;"
    my set condition_count 0
    foreach s [my defined State] {
      foreach a [$s get_actions] {
	append result [my draw_transition $s [self]::$a ""]
      }
      foreach role [$s set handled_roles] {
        set role_ctx [self]-$role
        #my msg exists?role=$role->[self]-$role->[info command [self]-$role]
	if {[info command ${role_ctx}::[$s name]] ne ""} {
	  foreach a [${role_ctx}::[$s name] get_actions] {
	    append result [my draw_transition $s ${role_ctx}::$a "$role:"]
	  }
        }
      }
    }

    append result "\}\n"
    set path [acs_package_root_dir xowf]/www/
    set fn $path/g.dot
    set ofn dot-$obj_id.png
    set f [open $fn w]; fconfigure $f -encoding utf-8; puts $f $result; close $f
    if {[catch {exec $dot -Tpng $fn -o $path/$ofn} errorMsg]} {
      my msg "Error during execution of $dot: $errorMsg"
    }
    file delete $fn
    return "<img style='$style' src='[[[my object] package_id] package_url]/$ofn'>\n"
  }

  Context instproc check {} {
    foreach s [my defined State]     {set state([$s name])  $s}
    foreach a [my defined Action]    {set action([$a name]) $a}
    foreach a [my defined Condition] {set condition([$a name]) $a}
    array set condition {else 1 true 1 default 1}
    foreach a [my defined Action] {
      # Are some "next_states" undefined?
      foreach {cond value} [$a get_cond_values [$a next_state]] {
        if {$cond ne "" && ![info exists condition($cond)]} {
          return [list rc 1 errorMsg "Error in action [$a name]: no such condition '$cond' defined \
		(valid: [lsort [array names condition]])"]
        }
        if {$value ne "" && ![info exists state($value)]} {
          return [list rc 1 errorMsg "Error in action [$a name]: no such state '$value' defined \
		(valid: [lsort [array names state]])"]
        }
      }
    }
    foreach s [my defined State] {
      # Are some "actions" undefined?
      foreach {cond actions} [$s get_cond_values [$s actions]] {
        foreach a $actions {
          if {![info exists action($a)]} {
            return [list rc 1 errorMsg "Error in state [$s name]: no such action '$a' defined \
		(valid: [lsort [array names action]])"]
          }
        }
      }
      if {[$s form_loader] eq ""} {
        my set forms([$s form]) 1
      }
    }
    foreach p [my defined ::xowiki::formfield::FormField] {
      if {[$p exists parampage]} {my set parampages([$p set parampage]) 1}
    }

    #my msg "forms=[my array names forms], parampages=[my array names parampages] in-role [my exists in_role] [my array names handled_roles]"
    
    if {![my exists in_role]} {
      foreach role [my array names handled_roles] {
        set role_ctx [self]-$role
        if {[my isobject $role_ctx]} {
          array set "" [$role_ctx check]
          if {$(rc) == 1} {return [array get ""]}
          my array set forms [$role_ctx array get forms]
          my array set parampage [$role_ctx array get parampage]
        }
      }
      #my msg "forms=[my array names forms], parampages=[my array names parampages]"
      set page [my object]
      $page set unresolved_references 0
      $page set __unresolved_references [list]
      foreach {type pages} [list wf_form [my array names forms] wf_parampage [my array names parampages]] {
        foreach p $pages {
          array set "" [my resolve_form_name $p [$page parent_id]]
          set l [::xowiki::Link new -volatile -page $page -type $type -name $(name)]
          # render does the optional fetch of the names, and maintains the
          # variable references of the page object (similar to render).
          set link_text [$l render]
        }
      }
      #my msg "-- link_text=$link_text"
      if {[$page exists references]} {
        #my msg "updating references refs=[$page set references]"
        $page update_references [$page item_id] [lsort -unique [$page set references]]
	$page unset references
      }
      if {[llength [$page set __unresolved_references]] > 0} {
	# TODO: we should provide a link to create the missing forms. maybe we 
	# change unresolved_references to a list..., or maybe we write these into the DB.
	my msg "Missing forms: [join [$page set __unresolved_references] {, }]"
      }
    }
    return [list rc 0]
  }
}

namespace eval ::xowf {
  #
  # WorkflowConstruct, the base class for workflow definitions
  #
  Class WorkflowConstruct -parameter {
    {handled_roles [list]}
    {label "[namespace tail [self]]"}
    {name  "[namespace tail [self]]"}
  }
  #WorkflowConstruct ad_instforward property     {get property} {%[my info parent] object} %proc
  #WorkflowConstruct ad_instforward set_property {set property} {%[my info parent] object} %proc

  WorkflowConstruct instforward property     {%[my info parent] object} %proc
  WorkflowConstruct instforward set_property {%[my info parent] object} %proc

  WorkflowConstruct instproc in_role {role configuration} {
    set ctx [my info parent]
    set obj [$ctx object]
    #my msg parent=$obj,cl=[$obj info class],name=[$obj name]
    if {[$ctx exists in_role]} {
      set success [expr {[$ctx in_role] eq $role}]
    } else {
      set success [$obj check_role $role]
    }
    #my msg role-$role->$success
    my lappend handled_roles $role
    $ctx set handled_roles($role) 1
    if {$success} {
      eval my configure $configuration
    }
  }
  WorkflowConstruct instproc get_condition {conditional_entry} {
    set e [split $conditional_entry :?]
    if {[llength $e]==2} {return [list cond [lindex $e 0] value [lindex $e 1]]}
    return [list cond "" value $conditional_entry]
  }

  WorkflowConstruct instproc get_cond_values {values} {
    if {[lindex $values 0] eq "?"} {
      return [lrange $values 1 end]
    } elseif {$values eq ""} {
      return ""
    } else {
      if {[regexp {^(.+):([^ ]+) } $values _ cond value]} {
        my msg "switch '$values' to new syntax: ? $cond $value ..."
      }
      return [list "" $values]
    }
  }
  WorkflowConstruct instproc get_value {values} {
    foreach {cond value} [my get_cond_values $values] {
      if {$cond eq "" || $cond eq "default" || $cond eq "else" || 
          $cond eq "true"} {
        return $value
      } elseif {[my $cond]} {
        return $value
      }
    }
  }
}

namespace eval ::xowf {
  WorkflowConstruct instforward true set "" 1
  WorkflowConstruct instforward false set "" 0

  proc ? {cmd expected {msg ""}} {
    ::xo::Timestamp t1
    set r [uplevel $cmd]
    if {$msg eq ""} {set msg $cmd}
    if {$r ne $expected} {
      regsub -all \# $r "" r
      append ::_ "Error: $msg returned \n'$r' ne \n'$expected'\n"
    } else {
      append ::_ "$msg - passed ([t1 diff] ms)\n"
    }
  }

  #
  # some test cases
  #
  WorkflowConstruct x
  set ::_ ""
  ? {x get_value ""} "" 
  ? {x get_value a} a 
  ? {x get_value {a b}} {a b}
  ? {x get_value "a b"} {a b}
  ? {x get_value "? true a default b"} {a}
  ? {x get_value "? false a default b"} {b}
  ? {x get_value "? true {a b} default {b c}"} {a b}
  ? {x get_value "? false {a b} default {b c}"} {b c}
  ns_log notice "--Test returns $::_"
}


namespace eval ::xowf {

  Class State -superclass WorkflowConstruct -parameter {
    {actions ""}
    {view_method ""}
    {form ""}
    {form_loader ""}
    {form_constraints ""}
    {assigned_to}
  }

  State instproc get_actions {} {
    return [my get_value [my actions]]
  }

  Class Condition -superclass WorkflowConstruct -parameter expr
  Condition instproc init {} {
    [my info parent]::Action instforward [namespace tail [self]] [self]
    [my info parent]::State  instforward [namespace tail [self]] [self]
  }
  Condition instproc defaultmethod {} {
    [my info parent] instvar {object obj}
    expr [my expr]
  }

  #{label "#xowf.form-button-[namespace tail [self]]#"}
  Class Action -superclass WorkflowConstruct -parameter {
    {next_state ""}
    {roles all}
  }
  Action instproc activate {obj} {;}
  Action instproc get_next_state {} {
    return [my get_value [my next_state]]
  }

  Class Property \
      -superclass ::xowiki::formfield::FormField -parameter {{name "[namespace tail [self]]"}} \
      -parameter {{allow_query_parameter false}}
  Property set abstract 1

  Property instproc get_default_from {page} {
    my set parampage $page
    my default [[my info parent] get_property -source $page -name [my name] -default ""]
  }
  #namespace export State Action Property



  #
  # MixinClass for implementing the workflow definition and workflow instance
  #
  Class WorkflowPage

  WorkflowPage ad_instproc is_wf {} {
    Check, if the current page is a workflow page (page, defining a workflow)
  } {
    if {[my exists __wf(workflow_definition)]} {return 1}
    if {[my property workflow_definition] ne ""} {
      my array set __wf [my instance_attributes]
      return 1
    }
    return 0
  }

  WorkflowPage ad_instproc is_wf_instance {} {
    Check, if the current page is a workflow instance (page, refering to a workflow)
  } {
    set pt [my page_template]
    if {[my state] ne "" && [$pt istype ::xowiki::FormPage]} {
      my array set __wfi [$pt instance_attributes]
      return 1
    }
    return 0
  }

  WorkflowPage instproc check_role {role} {
    if {[::xo::cc info methods role=$role] eq ""} {
      my msg "ignoring unknown role '$role'"
      return 0
    }
    if {$role eq "creator"} {
      # hmm, requires additional attibute
      return [::xo::cc role=$role \
                  -object [self] \
                  -user_id [::xo::cc user_id] \
                  -package_id [my package_id]]
    } else {
      return [::xo::cc role=$role \
                  -user_id [::xo::cc user_id] \
                  -package_id [my package_id]]
    }
  }

  WorkflowPage ad_instproc render_form_action_buttons {{-CSSclass ""}} {
    Render the defined actions in the current state with submit buttons
  } {
    if {[my is_wf_instance]} {
      ::html::div -class form-button {
        set ctx [::xowf::Context require [self]]
        foreach action [$ctx get_actions] {
          set success 0
          foreach role [$action roles] {
            set success [my check_role $role]
            if {$success} break
          }
          if {$success} {
            set f [::xowiki::formfield::submit_button new -destroy_on_cleanup \
                       -name __action_[namespace tail $action] -CSSclass $CSSclass]
            #my msg action=$action
            $f value [$action label]
            $f render_input
          }
        }
      }
    } else {
      next
    }
  }
  WorkflowPage ad_instproc post_process_edit_fields {dom_root form_field} {
    post-process form in edit mode to provide feedback in feedback mode
  } {
    # In feedback mode, we set the CSS class to correct or incorrect
    if {[my exists __feedback_mode]} {
      my unset __feedback_mode
      ::xo::Page requireCSS /resources/xowf/feedback.css
      set form [lindex [$dom_root selectNodes "//form"] 0]
      $form setAttribute class "[$form getAttribute class] feedback"
      foreach f $form_field {
        if {![$f exists answer]} continue
        if {[$f answer] eq [$f value]} {
          set feedback "correct"
          if {[$f exists feedback_answer_correct]} {set feedback [$f feedback_answer_correct]}
        } else {
          set feedback "incorrect"
          if {[$f exists feedback_answer_incorrect]} {set feedback [$f feedback_answer_incorrect]}
        }
        $f form-widget-CSSclass $feedback
        $f help_text [$f form-widget-CSSclass]
        foreach n [$dom_root selectNodes "//form//*\[@name='[$f name]'\]"] {
          set oldCSSClass [expr {[$n hasAttribute class] ? [$n getAttribute class] : ""}]
          $n setAttribute class [string trim "$oldCSSClass [$f form-widget-CSSclass]"]
        }
      }
      if {[my answer_is_correct]} {
        set feedback [my get_from_template feedback_correct]
      } else {
        set feedback [my get_from_template feedback_incorrect]
      }
      if {$feedback ne ""} {
        $dom_root appendFromScript {
          html::div -class feedback {
            html::t $feedback
          }
        }
      }
    }
  }
  WorkflowPage ad_instproc view {{content ""}} {
    Provide additional view modes:
       - edit: instead of viewing a page, it is opened in edit mode
       - view_user_input: show user the provided input
       - view_user_input_with_feedback: show user the provided input with feedback
  } {
    # The edit method calls view with an HTML content as argument.
    # To avoid a loop, when "view" is redirected to "edit",
    # we make sure that we only check the redirect on views
    # without content.
    if {[my is_wf_instance] && $content eq ""} {
      set ctx [::xowf::Context require [self]]
      set method [$ctx get_view_method]
      if {$method ne "" && $method ne "view"} {
        my instvar package_id
        #my msg "view redirects to $method in state [$ctx get_current_state]"
        switch $method {
          view_user_input {
            return [$package_id call [self] edit [list -disable_input_fields 1]]
          }
          view_user_input_with_feedback {
            my set __feedback_mode 1
            return [$package_id call [self] edit [list -disable_input_fields 1]]
          }
          default {return [$package_id invoke -method $method]}
        }
      } 
    }
    next
  }

  WorkflowPage instproc get_assignee {assigned_to} {
    return [my assignee]
  }

  WorkflowPage instproc send_to_assignee {
    -subject 
    -from 
    -body 
    {-mime_type text/plain}
    {-with_ical:boolean false}
  } {
    my instvar package_id page_template
    set wf_name [$page_template name]

    if {![info exists subject]} {
      set subject "\[$wf_name\] [my title] ([my state])"
    }
    if {![my exists from]} {set from [my creation_user]}
    acs_user::get -user_id [my assignee] -array to_info
    acs_user::get -user_id $from -array from_info

    set message_id [mime::uniqueID]
    set message_date [acs_mail_lite::utils::build_date]
    #set tokens [acs_mail_lite::utils::build_body -mime_type text/html $body]
    set tokens [mime::initialize \
                    -canonical $mime_type \
                    -encoding "quoted-printable" -string $body]

    if {$with_ical} {
      set items [::xo::OrderedComposite new -destroy_on_cleanup -mixin ::xo::ical::VCALENDAR]
      # hmm, mozilla just supports VEVENT, but changing the VTODO below into a VEVENT
      # does not seem to help either....
      $items add [::xo::ical::VTODO new \
                      -creation_date [my set creation_date] \
                      -last_modified [my last_modified] \
                      -uid $package_id-[my revision_id] \
                      -url [$package_id pretty_link -absolute true [my name]] \
                      -summary $subject \
                      -description "Workflow instance of workflow $wf_name [my description]"]
      $items configure -prodid "-//WU Wien//NONSGML XoWiki Content Flow//EN" -method request
      set ical [$items as_ical]
       lappend tokens [mime::initialize \
                           -canonical text/calendar \
                           -param [list method request] \
                           -param [list charset UTF-8] \
                           -header [list "Content-Disposition" "attachment; filename=\"todo.vcs\""] \
                           -encoding "quoted-printable" -string $ical]
      lappend tokens [mime::initialize \
                          -canonical application/ics -param [list name "invite.ics"] \
                           -header [list "Content-Disposition" "attachment; filename=\"todo.ics\""] \
                          -encoding "quoted-printable" -string $ical]
      #lappend tokens [acs_mail_lite::utils::build_body -mime_type {application/ics; name="invite.ics"} $ical]
    }
    
    if {[llength $tokens]>1} {
      set tokens [mime::initialize -canonical "multipart/mixed" -parts $tokens]
    }

    set headers_list [list]
    lappend headers_list \
	[list From $from_info(email)] \
	[list To $to_info(email)] \
	[list Subject $subject]

    acs_mail_lite::smtp -multi_token $tokens -headers $headers_list
    mime::finalize $tokens -subordinates all
    
  }

  WorkflowPage instproc get_form_data args {
    if {[my is_wf_instance]} {
      foreach {validation_errors category_ids} [next] break
      if {$validation_errors == 0} {
        #my msg "validation ok"
        foreach {name value} [::xo::cc get_all_form_parameter] {
          if {[regexp {^__action_(.+)$} $name _ action]} {
            set ctx [::xowf::Context require [self]]
            if {[catch {${ctx}::$action activate [self]} errorMsg]} {
	      set error "error in action '$action' of workflow instance [my name]\
		of workflow [[my page_template] name]:"
	      if {[[my package_id] exists __batch_mode]} {
		[my package_id] set __evaluation_error "$error\n\n$::errorInfo"
		incr validation_errors
	      } else {
		my msg -html 1 "$error <PRE>$::errorInfo</PRE>"
	      }
              my log "--- evaluation error $error\n$::errorInfo"
            } else {
              set next_state [${ctx}::$action get_next_state]
              #my msg "next_state=$next_state, current_state=[$ctx get_current_state]"
              if {$next_state ne ""} {
                if {[${ctx}::$next_state exists assigned_to]} {
                  my assignee [my get_assignee [${ctx}::$next_state assigned_to]]
                }
                $ctx set_current_state $next_state
              }
            }
            break
          }
        }
      }
      return [list $validation_errors $category_ids]
    } else {
      next
    }
  }

  WorkflowPage instproc instantiated_form_fields {} {
    # Helper method to
    #  - obtain the field_names from the current form, to
    #  - create form_field instances from that and to
    #  - provide the values from the instance attributes into it.
    foreach {_ field_names} [my field_names_from_form] break
    set form_fields [my create_form_fields $field_names]
    my load_values_into_form_fields $form_fields
    return $form_fields
  }
  WorkflowPage ad_instproc solution_set {} {
    Compute solution set in form of attribute=value pairs
    based on "answer" attribute of form fields.
  } {
    set solutions [list]
    foreach f [my instantiated_form_fields] {
      if {![$f exists answer]} continue
      lappend solutions [$f name]=[$f answer]
    }
    return [join [lsort $solutions] ", "]
  }
  WorkflowPage ad_instproc answer_is_correct {} {
    Check, if answer is correct based on "answer" attribute of form fields
    and provided user input.
  } {
    set correct 1
    foreach f [my instantiated_form_fields] {
      if {![$f exists answer]} continue
      if {[$f value] ne [$f answer]} {
        set correct 0
        break
      }
    }
    return $correct
  }
  WorkflowPage instproc unset_temporary_instance_variables {} {
    # never save/cache the following variables
    my array unset __wfi
    my array unset __wf
    next
  }
  WorkflowPage instproc save_data args {
    if {[my is_wf_instance]} {
      array set __ia [my instance_attributes]
      set ctx [::xowf::Context require [self]] 
      my state [$ctx get_current_state]
      #my msg "saving ia: [array get __ia]"
      my instance_attributes [array get __ia]
      #
      # we have to flag currently storing in hstore here, since
      # saving removes the temporary variables for properties
      #
      if {[::xo::db::has_hstore] && [[my package_id] get_parameter use_hstore 0]} {set save_in_hstore 1}
    } elseif {[my is_wf]} {
      if {[::xo::db::has_hstore] && [[my package_id] get_parameter use_hstore 0]} {set save_in_hstore 1}
    }
    next
    #my msg "save_in_hstore=[info exists save_in_hstore]"
    if {[info exists save_in_hstore]} {
      # "next" sets the revision_id, by not e.g. page_instance_id
      my set page_instance_id [my revision_id]
      my save_in_hstore
    }
  }

  WorkflowPage instproc double_quote {value} {
    if {[regexp {[ ,\"\\=>]} $value]} {
      set value \"[string map [list \" \\\\\" \\ \\\\ ' ''] $value]\"
    }
    return $value
  }
  WorkflowPage instproc save_in_hstore {} {
    # experimental code for testing with hstore
    # to use it, do for now something like:
    #
    # /usr/local/pg820/bin/psql -U nsadmin -d dotlrn-test5 < /usr/local/pg820/share/postgresql/contrib/hstore.sql
    # alter table xowiki_page_instance add column hkey hstore;
    # CREATE INDEX hidx ON xowiki_page_instance using GIST(hkey);
    #
    set keys [list]
    foreach {key value} [my instance_attributes] {
      set v [my double_quote $value]
      if {$v eq ""} continue
      if {$key eq "workflow_definition"} continue
      lappend keys [my double_quote $key]=>$v
    }
    #my msg "hkey='[join $keys ,]'"
    db_dml dbqd..update_hstore "update xowiki_page_instance \
                set hkey = '[join $keys ,]'
                where page_instance_id = [my revision_id]"
  }
  WorkflowPage instproc wf_property {name} {
    if {[my exists __wf]} {set key __wf($name)} else {set key __wfi($name)}
    if {[my exists $key]} { return [my set $key] }
    return ""
  }
  WorkflowPage instproc get_form_id {} {
    my instvar page_template
    if {[my is_wf_instance]} {
      set key __wfi(wf_form_id)
      if {![my exists $key]} {
	set ctx [::xowf::Context require [self]]
	my set $key [$ctx form_id [my parent_id]]
      }
      # TODO handle case, when form_id == 0
      set form_id [my set $key]

      # be sure, to instantiate as well page_template, some procs assume this
      if {![my isobject ::$page_template]} {
        ::xo::db::CrClass get_instance_from_db -item_id $page_template
      }
      return $form_id
    } else {
      return [next]
    }
  }
  WorkflowPage instproc default_instance_attributes {} {
    # Provide the default list of instance attributes to derived 
    # FormPages.
    if {[my is_wf]} {
      #
      # we have a workflow page
      # get the initial state from the workflow
      #
      set ctx [::xowf::Context require [self]]
      foreach p [$ctx defined ::xowiki::formfield::FormField] {
        set __ia([$p name]) [$p default]
        set f([$p name]) $p
      }
      foreach {qp_name value} [::xo::cc get_all_query_parameter] {
        if {[regexp {^p.(.+)$} $qp_name _ name] 
            && [info exists f($name)] 
            && [$f($name) exists allow_query_parameter]
          } {
          set __ia($name) $value
        }
      }
      # set always the current state to the value from the ctx (initial)
      my state [$ctx get_current_state]
      #my msg "setting initial state to '[my state]'"
      set instance_attributes [array get __ia]
      #my msg ia=$instance_attributes,props=[$ctx defined Property]
      return $instance_attributes
    } else {
      next
    }
  }
  WorkflowPage instproc constraints_as_array {c} {
    array set __c ""
    foreach name_and_spec $c {
      regexp {^([^:]+):(.*)$} $name_and_spec _ spec_name short_spec
      set __c($spec_name) $short_spec
    }
    return [array get __c]
  }
  WorkflowPage instproc merge_constraints {c1 args} {
    # Load into the base_constraints c1 the constraints from the argument list.
    # The first constraints have the lowest priority
    array set __c1 [my constraints_as_array $c1]
    foreach c2 $args {
      foreach {att value} [my constraints_as_array $c2] {
        set key __c1($att)
        if {[info exists $key]} {append $key ",$value"} else {set $key $value}
      }
    }
    set result [list]
    foreach {att value} [array get __c1] {lappend result $att:$value}
    return $result
  }
  WorkflowPage instproc wfi_merged_form_constraints {constraints_from_form} {
    set ctx [::xowf::Context require [self]]
    set wf_specific_constraints [[my page_template] property form_constraints]
    set m [my merge_constraints $wf_specific_constraints \
               $constraints_from_form [$ctx get_form_constraints]]
    #my msg merged:$m
    return $m
  }
  WorkflowPage instproc wf_merged_form_constraints {constraints_from_form} {
    return [my merge_constraints $constraints_from_form [my property form_constraints]]
  }
  WorkflowPage instproc get_form_constraints {} {
    if {[my istype ::xowiki::FormPage] && [my is_wf]} {
      return [::xo::cc cache [list [self] wf_merged_form_constraints [next]]]
    } elseif {[my istype ::xowiki::FormPage] && [my is_wf_instance]} {
      return [::xo::cc cache [list [self] wfi_merged_form_constraints [next]]]
    } else {
      next
    }
  }
  WorkflowPage instproc visited_states {} {
    my instvar item_id
    foreach state [db_list [my qn history] {
      select DISTINCT state from xowiki_form_page p, cr_items i, cr_revisions r 
      where i.item_id = :item_id and r.item_id = i.item_id and xowiki_form_page_id = r.revision_id}] {
      set visited($state) 1
    }
    #my msg "visisted states of item $item_id = [array names visited]"
    return [array names visited]
  }

  WorkflowPage ad_instproc footer {} {
    Provide a tailored footer for workflow definition pages and 
    workflow instance pages containing controls for instantiating
    forms or providing links to the workflow definition.
  } {
    if {[my exists __no_form_page_footer]} {
      next
    } else {
      my instvar package_id
      set form_item_id [my page_template]
      #my msg "is wf page [my is_wf], is wf instance page [my is_wf_instance]"
      if {[my is_wf]} {
        #
        # page containing a work flow definition
        #
	#set ctx [::xowf::Context require [self]]
        set work_flow_form [::xo::db::CrClass get_instance_from_db -item_id $form_item_id]
        set work_flow_base [$package_id pretty_link [$work_flow_form name]]

        set wf [self]
        set wf_base [$package_id pretty_link [$wf name]]
        set button_objs [list]

        # create new workflow instance button with start form
        set link [$package_id make_link -link $wf_base $wf create-new return_url]
        lappend button_objs [::xowiki::includelet::form-menu-button-new new -volatile \
                                 -form $wf -link $link]

        # list workflow instances button
        set obj [::xowiki::includelet::form-menu-button-wf-instances new -volatile \
                     -package_id $package_id \
                     -base $wf_base -form $wf]
        if {[info exists return_url]} {$obj return_url $return_url}
        lappend button_objs $obj

        # work flow definition button 
        set obj [::xowiki::includelet::form-menu-button-form new -volatile \
                     -package_id $package_id \
                     -base $work_flow_base -form $work_flow_form]
        if {[info exists return_url]} {$obj return_url $return_url}
        lappend button_objs $obj

        # make menu
        return [my include [list form-menu -button_objs $button_objs]]

      } elseif {[my is_wf_instance]} {
        #
        # work flow instance
        #
	set entry_form_item_id [my wf_property wf_form_id]
	#my msg entry_form_item_id=$entry_form_item_id
        set work_flow_form [::xo::db::CrClass get_instance_from_db -item_id $form_item_id]
        set work_flow_base [$package_id pretty_link [$work_flow_form name]]
        set button_objs [list]

        # form definition button
        if {![my isobject $entry_form_item_id]} {
          # In case, the id is a form object, it is a dynamic form,
          # that we can't edit; therefore, we provide no link.
          #
          # Here, we have have an id that we use for fetching...
          #
          set form [::xo::db::CrClass get_instance_from_db -item_id $entry_form_item_id]
          set base [$package_id pretty_link [$form name]]
          set obj [::xowiki::includelet::form-menu-button-form new -volatile \
                       -package_id $package_id \
                       -base $base -form $form]
          if {[info exists return_url]} {$obj return_url $return_url}
          lappend button_objs $obj
        }
        
        # work flow definition button 
        set obj [::xowiki::includelet::form-menu-button-wf new -volatile \
                     -package_id $package_id \
                     -base $work_flow_base -form $work_flow_form]
        if {[info exists return_url]} {$obj return_url $return_url}
        lappend button_objs $obj
        # make menu
        return [my include [list form-menu -button_objs $button_objs]]
      } else {
        return [my include [list form-menu -form_item_id $form_item_id -buttons form]]
      }
    }
  }

  WorkflowPage ad_instproc call_action {-action {-attributes {}}} {
    Call the specified action in the current workflow instance.
    The specified attributes are provided like form_parameters to
    the action of the workflow.
  } {
    my instvar package_id
    if {![my is_wf_instance]} {
      error "Page [self] is not a Workflow Instance"
    }
    set ctx [::xowf::Context require [self]]
    foreach a [$ctx get_actions] {
      if {[namespace tail $a] eq "$action"} {
	# In the current state, the specified action is allowed, so
	# fake a work request with the given instance attributes 
	::xo::cc array set form_parameter \
	    [list __object_name [my name] \
		 __form_action save-form-data \
		 __form_redirect_method __none \
		 __action_$action $action]
	::xo::cc array set form_parameter $attributes

	if {[catch {::$package_id invoke -method edit -batch_mode 1} errorMsg]} {
          #my log "---call-action returns error $errorMsg"
          error $errorMsg
        }
	return "OK"
      }
    }
    error "\tNo action '$action' available in workflow instance [self] of \
	[[my page_template] name] in state [$ctx get_current_state]
	Available actions: [[$ctx current_state] get_actions]"
  }


  ad_proc migrate_from_wf_current_state {} {
    # 
    # Transform the former instance_attributes 
    #   "wf_current_state" to the xowiki::FormPage attribute "state", and
    #   "wf_assignee" to the xowiki::FormPage attribute "assignee".
    #
    set count 0
    foreach atts [db_list_of_lists dbq..entries {
      select p.state,p.assignee,pi.instance_attributes,p.xowiki_form_page_id 
      from xowiki_form_page p, xowiki_page_instance pi, cr_items i, cr_revisions r 
      where r.item_id = i.item_id and p.xowiki_form_page_id = r.revision_id and
      pi.page_instance_id = r.revision_id
    }] {
      foreach {state assignee instance_attributes xowiki_form_page_id} $atts break
      array unset __ia
      array set __ia $instance_attributes
      if {[info exists __ia(wf_current_state)] && 
          $__ia(wf_current_state) ne $state} {
        #Object msg "must update state $state for $xowiki_form_page_id to  $__ia(wf_current_state) "
        db_dml dbqd..update_state "update xowiki_form_page \
                set state = '$__ia(wf_current_state)'
                where xowiki_form_page_id  = $xowiki_form_page_id" 
        incr count
      }
      if {[info exists __ia(wf_assignee)] && 
          $__ia(wf_assignee) ne $assignee} {
        #Object msg "must update assignee $assignee for $xowiki_form_page_id to  $__ia(wf_assignee) "
        db_dml dbqd..update_state "update xowiki_form_page \
                set assignee = '$__ia(wf_assignee)'
                where xowiki_form_page_id  = $xowiki_form_page_id" 
        incr count
      }
    }
    return $count
  }

  ad_proc update_hstore {package_id} {
    update all instance attributes in hstore
  } {
    if {![::xo::db::has_hstore] && [$package_id get_parameter use_hstore 0] } {return 0}
    #
    # This proc can be used from ds/shell as follows
    #
    #    ::xowf::Package initialize -url /xowf
    #    ::xowf::update_hstore $package_id
    #
    # Check the result
    #
    #    select hkey from xowiki_page_instance where hkey is not null;
    #
    ::xowf::Package initialize -package_id $package_id
    #
    # we get all revisions, so use the lower level interface
    #
    set items [::xowiki::FormPage instantiate_objects \
                   -sql "select * from xowiki_form_pagei bt,cr_items i \
			where i.parent_id = [$package_id folder_id] and bt.item_id = i.item_id" \
                   -object_class ::xowiki::FormPage]
    set count 0
    foreach i [$items children] {
      #$i msg "working on [$i set xowiki_form_page_id]"
      $i save_in_hstore
      incr count 
    }
    $items msg "fetched $count objects from parent_id [$package_id folder_id]"
    return 1
  }

  # Some example hstore queries (over all revisions)
  #    select hkey from xowiki_page_instance where hkey is not null;
  #    select hkey from xowiki_page_instance where defined(hkey, 'team_email');
  #    select hkey from xowiki_page_instance where exist(hkey, 'team_email');
  #    select hkey from xowiki_page_instance where  'team_email=>neumann@wu-wien.ac.at' <@ hkey;
  #    select (each(hkey)).key, (each(hkey)).value from xowiki_page_instance;
  #    select page_instance_id, (each(hkey)).key, (each(hkey)).value from xowiki_page_instance 
  #        where 'assignee=>539,priority=>1' <@ hkey;   
  #    select key, count(*) from (select (each(hkey)).key from xowiki_page_instance) as stat 
  #        group by key order by count desc, key;
  #

}

#
# In order to provide either a REST or a DAV interface, we have to 
# switch to basic authentication, since non-openacs packages 
# have problems to handle openacs coockies. The basic authentication
# interface can be establised in three steps:
#
#  1) Create a basic authentication handler, Choose a URL and 
#     define optionally the package to be initialized:
#     Example:
#            ::xowf::dav create ::xowf::ba -url /ba -package ::xowf::Package
# 
#  2) Make sure, the basic authenication handler is initialied during
#     startup. Write an -init.tcl file containing a call to the
#     created handler.
#     Example:
#            ::xowf::ba register
#
#  3) Write procs with names such as GET, PUT, POST to handle
#     the requests. These procs overload the predefined behavior.
#


namespace eval ::xowf {
  ::xotcl::Class create ::xowf::dav -superclass ::xo::dav

  ::xowf::dav instproc get_package_id {} {
    my instvar uri package wf package_id
    if {$uri eq "/"} {
      # Take the first package instance
      set wf ""
      set package_id [lindex [$package instances] 0]
      $package initialize -package_id $package_id
    } else {
      set wf /$uri
      $package initialize -url $uri
    }
    # my log package_id=$package_id
    return $package_id
  }

  ::xowf::dav instproc call_action {-uri -action -attributes} {
    [my package] initialize -url $uri
    set object_name [$package_id set object]
    set page [$package_id resolve_request -path $object_name method]
    if {$page eq ""} {
      ns_return 406 text/plain "Error: cannot resolve '$object_name' in package [$package_id package_url]"
    } elseif {[catch {set msg [$page call_action \
                                   -action $action \
                                   -attributes $attributes]} errorMsg]} {
      ns_return 406 text/plain "Error: $errorMsg\n"
    } else {
      ns_return 200 text/plain "Success: $msg\n"
    }
   }


  ::xowf::dav create ::xowf::dav-todo -url /dav-todo -package ::xowf::Package

  ::xowf::dav-todo proc GET {} {
    my instvar uri wf package_id
    set p [::xowiki::Page new -package_id $package_id]
    $p include [list wf-todo -ical 1 -workflow $wf]
    #ns_return 200 text/plain GET-$uri-XXX-pid=$package_id-wf=$wf-[::xo::cc serialize]
  }

#   ::xowf::dav-todo proc GET {} {
#     set uri /xowf/153516
#     set uri /xowf/18362
#     set uri /xowf/18205
#     my call_action -uri $uri -action work -attributes [list comment hello3 effort 4]
#   }


}
