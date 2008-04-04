::xo::library doc {
  XoWiki Workflow - main library classes and objects

  @author Gustaf Neumann
  @creation-date 2008-03-05
  @cvs-id $Id $
}

# todo:
# - validation of template and form_constraints DONE
# - plain wiki pages DONE
# - Roles
# - assignment

::xo::db::require package xowiki

namespace eval ::xowf {
  ::xo::PackageMgr create ::xowf::Package \
      -package_key "xowf" -pretty_name "XoWiki Workflow" \
      -superclass ::xowiki::Package
  
  Package ad_instproc init {} {
    mixin ::xowf::WorkflowPage to every FormPage
  } {
    my destroy_on_cleanup; #TODO core?
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
       Class create Action   -superclass ::xowf::Action
       Class create State    -superclass ::xowf::State
       Class create Property -superclass ::xowf::Property -set abstract 1
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
      foreach a [$s actions] {
	set action [self]::$a
	set next_state [$action next_state]
	if {$next_state eq ""} {set next_state [namespace tail $s]}
	append result "  state_[namespace tail $s] -> state_$next_state \[label=\"$a\"\];\n"	
      }
      foreach role [$s set handled_roles] {
        set role_ctx [self]-$role
        #my msg exists?role=$role->[self]-$role->[info command [self]-$role]
        foreach a [${role_ctx}::[$s name] actions] {
          set action ${role_ctx}::$a
          set next_state [$action next_state]
          if {$next_state eq ""} {set next_state [namespace tail $s]}
          append result "  state_[namespace tail $s] -> state_$next_state \[label=\"$role:$a\"\];\n"	
        }
      }
    }
    append result "start->state_initial;"
    append result "\}\n"
    set path [acs_package_root_dir xowf]/www/
    set fn $path/g.dot
    set ofn dot-$obj_id.png
    set f [open $fn w]; puts $f $result; close $f
    exec $dot -Tpng $fn -o $path/$ofn
    file delete $fn
    return "<img style='$style' src='[[[my object] package_id] package_url]/$ofn'>\n"
  }


  #
  # State and Action, two base classes for workflow definitions
  #
  Class WorkflowConstruct -parameter {{handled_roles [list]}}
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
    {label "[namespace tail [self]]"}
    {name  "[namespace tail [self]]"}
  }

  #{label "#xowf.form-button-[namespace tail [self]]#"}
  Class Action -superclass WorkflowConstruct -parameter {
    {next_state ""}
    {roles all}
    {label "[namespace tail [self]]"}
    {name  "[namespace tail [self]]"}
  }
  Action instproc activate {obj} {;}

  Class Property -superclass ::xowiki::FormField -parameter {{name "[namespace tail [self]]"}}
  Property set abstract 1
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
              set next_state [${ctx}::$action next_state]
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
      #my msg "setting current_state to '[$ctx get_current_state]'"
      my instance_attributes [array get __ia]
      #my msg "saving ia: [array get __ia]"
    }
    next
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
      foreach p [$ctx defined ::xowiki::FormField] {set __ia([$p name]) [$p default]}
      foreach {qp_name value} [::xo::cc get_all_query_parameter] {
        if {[regexp {^p.(.+)$} $qp_name _ name]} {
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

}



