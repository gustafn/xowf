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

  Context instproc set_current_state {value} {
    my current_state [self]::$value
  }
  Context instproc get_current_state {} {
    namespace tail [my current_state]
  }

  Context instproc get_actions {} {
    set actions [list]
    #my msg "for [my current_state] actions '[[my current_state] actions]'"
    foreach action [[my current_state] actions] {
      lappend actions [self]::$action
    }
    return $actions
  }
  Context instproc defined {what} {
    set result [list]
    foreach c [my info children] {if {[$c istype $what]} {lappend result $c}}
    return $result
  }

  Context instproc form_id {parent_id} {
    #my msg "get form_id '[my exists form_id]' [my form]"
    if {[my exists form_id]} {return [my set form_id]}
    set form_id [::xo::db::CrClass lookup -name [my form] -parent_id $parent_id]
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
      set state [$obj property wf_current_state]
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
    set spec [$action next_state]
    set result ""
    if {[llength $spec]>1} {
      # we have a condition
      set c cond_[$from name]
      set arc_style {,style="setlinewidth(1)",color=gray}
      append result "  state_$c \[shape=diamond, fixedsize=1, width=0.2, height=0.2, fixedsize=1,style=solid,color=gray,label=\"\"\];\n"
      append result [my draw_arc [$from name] $c [$action name]-1 $role[$action label] ""]
      foreach entry $spec {
	array set "" [$action get_condition $entry]
	if {$(cond) ne ""} {set prefix "$(cond)"} {set prefix "else"}
	append result [my draw_arc $c $(value) [$action name] \[$prefix\] $arc_style] 
	#append result [my draw_arc [$from name] $(value) [$action name] $role$prefix[$action label]]
      }
    } else {
      set prefix ""
      append result [my draw_arc [$from name] $spec [$action name] $role$prefix[$action label] ""]
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
    foreach s [my defined State] {
      foreach a [$s actions] {
	append result [my draw_transition $s [self]::$a ""]
      }
      foreach role [$s set handled_roles] {
        set role_ctx [self]-$role
        #my msg exists?role=$role->[self]-$role->[info command [self]-$role]
	if {[info command ${role_ctx}::[$s name]] ne ""} {
	  foreach a [${role_ctx}::[$s name] actions] {
	    append result [my draw_transition $s ${role_ctx}::$a "$role:"]
	  }
        }
      }
    }

    append result "\}\n"
    set path [acs_package_root_dir xowf]/www/
    set fn $path/g.dot
    set ofn dot-$obj_id.png
    set f [open $fn w]; puts $f $result; close $f
    exec $dot -Tpng $fn -o $path/$ofn
    file delete $fn
    return "<img style='$style' src='[[[my object] package_id] package_url]/$ofn'>\n"
  }

  Context instproc check {} {
    foreach s [my defined State]     {set state([$s name])  $s}
    foreach a [my defined Action]    {set action([$a name]) $a}
    foreach a [my defined Condition] {set condition([$a name]) $a}
    foreach a [my defined Action] {
      # Are some "next_states" undefined?
      foreach entry [$a next_state] {
        array set "" [$a get_condition $entry]
        if {$(cond) ne "" && ![info exists condition($(cond))]} {
          return [list rc 1 errorMsg "Error in action [$a name]: no such condition '$(cond)' defined \
		(valid: [lsort [array names condition]])"]
        }
        if {$(value) ne "" && ![info exists state($(value))]} {
          return [list rc 1 errorMsg "Error in action [$a name]: no such state '$(value)' defined \
		(valid: [lsort [array names state]])"]
        }
      }
      foreach s [my defined State] {
        # Are some "actions" undefined?
        foreach a [$s actions] {
          if {![info exists action($a)]} {
            return [list rc 1 errorMsg "Error in state [$s name]: no such action '$a' defined \
		(valid: [lsort [array names action]])"]
          }
        }
        my set forms([$s form]) 1
      }
      foreach p [my defined ::xowiki::FormField] {
        if {[$p exists parampage]} {my set parampages([$p set parampage]) 1}
      }
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
      foreach {type pages} [list wf_form [my array names forms] wf_parampage [my array names parampages]] {
        foreach p $pages {
          set l [::xowiki::Link new -volatile -page $page -type $type -name $p]
          set item_id [$l resolve]
          #my msg "-- wf resolve for $page returned $item_id (name=$p) "
          # Rendering the link does the optional fetch of the names, and maintains the
          # variable references of the page object.
          set link_text [$l render]
        }
      }
      #my msg "-- link_text=$link_text"
      if {[$page exists references]} {
        #my msg "updating references refs=[$page set references]"
        $page update_references [$page item_id] [lsort -unique [$page set references]]
	$page unset references
      }
    }
    return [list rc 0]
  }

  #
  # State and Action, two base classes for workflow definitions
  #
  Class WorkflowConstruct -parameter {
    {handled_roles [list]}
    {label "[namespace tail [self]]"}
    {name  "[namespace tail [self]]"}
  }
  WorkflowConstruct ad_instforward property     {get property} {%[my info parent] object} %proc
  WorkflowConstruct ad_instforward set_property {set property} {%[my info parent] object} %proc

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

  Class State -superclass WorkflowConstruct -parameter {
    {actions ""}
    {view_method ""}
    {form ""}
    {form_constraints ""}
    {assigned_to}
  }

  Class Condition -superclass WorkflowConstruct -parameter expr
  Condition instproc init {} {
    [my info parent]::Action instproc [namespace tail [self]] {} "
      [my info parent] instvar {object obj}
      expr {[my expr]}
    "
  }

  #{label "#xowf.form-button-[namespace tail [self]]#"}
  Class Action -superclass WorkflowConstruct -parameter {
    {next_state ""}
    {roles all}
  }
  Action instproc activate {obj} {;}
  Action instproc get_condition {conditional_entry} {
    set e [split $conditional_entry :]
    if {[llength $e]==2} {return [list cond [lindex $e 0] value [lindex $e 1]]}
    return [list cond "" value $conditional_entry]
  }
  Action instproc get_next_state {} {
    foreach entry [my next_state] {
      array set "" [my get_condition $entry]
      if {$(cond) ne ""} {
	if {[my $(cond)]} {return $(value)}
      } else {
	return $(value)
      }
    }
  }

  Class Property \
      -superclass ::xowiki::FormField -parameter {{name "[namespace tail [self]]"}} \
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
    #my msg  wf_current_state=[my property wf_current_state]-[my info class]
    if {[my property wf_current_state] ne ""} {
      my array set __wfi [[my page_template] instance_attributes]
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
            set f [::xowiki::FormField::submit_button new -destroy_on_cleanup \
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
    # todo: resolve assingned_to, currently we just return some user
    return [my creation_user]
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
              my msg "error in action: $errorMsg"
            } else {
              set next_state [${ctx}::$action get_next_state]
              #my msg "next_state=$next_state, current_state=[$ctx get_current_state]"
              if {$next_state ne ""} {
                if {[${ctx}::$next_state exists assigned_to]} {
                  my set_property wf_assignee \
                      [my get_assignee [${ctx}::$next_state assigned_to]]
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
      set __ia(wf_current_state) [$ctx get_current_state]
      #my msg "saving ia: [array get __ia]"
      my instance_attributes [array get __ia]
      #
      # we have to flag currently storing in hstore here, since
      # saving removes the temporary variables for properties
      #
      if {[::xo::db::has_hstore]} {set save_in_hstore 1}
    }
    next
    if {[info exists save_in_hstore]} {
      # "next" sets the revision_id
      my save_in_hstore
    }
  }

  WorkflowPage instproc double_quote {value} {
    if {[regexp {[ ,\"\\=>]} $value]} {
      set value \"[string map [list \" \\\\\" \\ \\\\ ' \\\\'] $value]\"
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
    my msg "hkey='[join $keys ,]'"
    db_dml dbqd..update_hstore "update xowiki_page_instance \
                set hkey = '[join $keys ,]'
                where page_instance_id = [my page_instance_id]"
  }
  WorkflowPage instproc wf_property {name} {
    if {[my exists __wf]} {set key __wf($name)} else {set key __wfi($name)}
    if {[my exists $key]} { return [my set $key] }
    return ""
  }
  WorkflowPage instproc get_form_id {} {
    #my msg "[my is_wf_instance]"
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
      foreach p [$ctx defined ::xowiki::FormField] {
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
      set __ia(wf_current_state) [$ctx get_current_state]
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
    foreach atts [db_list [my qn history] {
      select instance_attributes from xowiki_page_instance p, cr_items i, cr_revisions r 
      where i.item_id = :item_id and r.item_id = i.item_id and page_instance_id = r.revision_id}] {
      array set __ia $atts
      set visited($__ia(wf_current_state)) 1
    }
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
        set form [::xo::db::CrClass get_instance_from_db -item_id $entry_form_item_id]
        set base [$package_id pretty_link [$form name]]
        set obj [::xowiki::includelet::form-menu-button-form new -volatile \
                     -package_id $package_id \
                     -base $base -form $form]
        if {[info exists return_url]} {$obj return_url $return_url}
        lappend button_objs $obj
        
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

  ad_proc update_hstore {package_id} {
    update all instance attributes in hstore
  } {
    if {![::xo::db::has_hstore]} {return 0}
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



