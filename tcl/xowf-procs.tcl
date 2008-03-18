::xo::library doc {
  XoWiki Workflow - main library classes and objects

  @author Gustaf Neumann
  @creation-date 2008-03-05
  @cvs-id $Id $
}

::xo::db::require package xowiki

namespace eval ::xowf {
  ::xo::PackageMgr create ::xowf::Package \
      -package_key "xowf" -pretty_name "XoWiki Workflow" \
      -superclass ::xowiki::Package
  
  Package instproc init {} {
    my destroy_on_cleanup; #TODO core?
    # called, whenever package is initialized 
    # ... nothing yet ....
    next
    ::xowiki::FormPage instmixin add ::xowf::WorkflowPage
  }
  Package instproc destroy {} {
    ::xowiki::FormPage instmixin delete ::xowf::WorkflowPage
    next
  }

  #
  # workflow context
  #
  
  Class Context -parameter {
    {current_state "[self]::initial"} 
    workflow_definition
    object
  }
  Context instproc set_current_state {value} {
    my current_state [self]::$value
  }
  Context instproc get_current_state {} {
    namespace tail [my current_state]
  }
  Context instproc possible_next_states {} {
    [my current_state] possible_next_states
  }
  Context instproc get_view_method {} {
    [my current_state] view_method
  }
  Context instproc get_form_constraints {} {
    [my current_state] form_constraints
  }
  Context instproc get_actions {} {
    set actions [list]
    #my msg "for [my current_state] actions '[[my current_state] actions]'"
    foreach action [[my current_state] actions] {
      lappend actions [self]::$action
    }
    return $actions
  }
  Context instproc available_actions {} {
    set result [list]
    foreach c [my info children] {if {[$c istype Action]} {lappend result $c}}
    return $result
  }
  Context instproc available_states {} {
    set result [list]
    foreach c [my info children] {if {[$c istype State]} {lappend result $c}}
    return $result
  }
  Context instproc form {} {
    #my msg "get form '[[my current_state] form]' from state [my current_state]"
    [my current_state] form
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
    my contains "namespace import -force ::xowf::*\n[my workflow_definition]"
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
  Context instproc as_graph {} {
    set dot ""
    catch {set dot [::util::which dot]}
    # final ressort for cases, where ::util::which is not available
    if {$dot eq "" && [file executable /usr/bin/dot]} {set dot /usr/bin/dot}
    if {$dot eq ""} {return "<font color='red'>Program 'dot' is not available! No graph displayed.</font>"}
    set obj_id [namespace tail [my object]]
    set result [subst {digraph workflow_$obj_id \{
      node \[fontname="Courier", color=lightblue2, style=filled\];
      edge \[fontname="Courier"\];
    }]
    foreach s [my available_states] {
      append result "  state_[namespace tail $s] \[label=\"[$s label]\"\];\n"
      foreach a [$s actions] {
	set action [self]::$a
	set next_state [$action next_state]
	if {$next_state eq ""} {set next_state [namespace tail $s]}
	append result "  state_[namespace tail $s] -> state_$next_state \[label=\"$a\"\];\n"	
      }
    }
    append result "\}\n"
    set path [acs_package_root_dir xowf]/www/
    set fn $path/g.dot
    set ofn dot-$obj_id.png
    set f [open $fn w]; puts $f $result; close $f
    exec $dot -Tpng $fn -o $path/$ofn
    file delete $fn
    return "<img style='width:100%' src='[[[my object] package_id] package_url]/$ofn'>\n"
  }

  #
  # State and Action, two base classes for workflow definitions
  #
  Class State -parameter {
    {actions ""}
    {view_method ""}
    {form ""}
    {form_constraints ""}
    {label "[namespace tail [self]]"}
    {name  "[namespace tail [self]]"}
  }

  #{label "#xowf.form-button-[namespace tail [self]]#"}
  Class Action -parameter {
    {next_state ""}
    {roles all}
    {label "[namespace tail [self]]"}
    {name  "[namespace tail [self]]"}
  }
  Action instproc activate {obj} {;}
  namespace export State Action
  

  #
  # MixinClass for implementing the workflow definition and workflow instance
  #
  Class WorkflowPage

  WorkflowPage instproc is_wf {} {
    #
    # Is the current page a workflow page (page, defining a workflow)?
    #
    if {[my exists __wf(workflow_definition)]} {return 1}
    if {[my property workflow_definition] ne ""} {
      my array set __wf [my instance_attributes]
      return 1
    }
    return 0
  }

  WorkflowPage instproc is_wf_instance {} {
    #
    # Is the current page a workflow instance (page, refering to a workflow)?
    #
    #my msg  wf_current_state=[my property wf_current_state]-[my info class]
    if {[my property wf_current_state] ne ""} {
      my array set __wfi [[my page_template] instance_attributes]
      return 1
    }
    return 0
  }

  WorkflowPage instproc render_form_action_buttons {{-CSSclass ""}} {
    if {[my is_wf_instance]} {
      ::html::div -class form-button {
        set ctx [::xowf::Context require [self]]
        foreach action [$ctx get_actions] {
          set success 0
          foreach role [$action roles] {
            if {[::xo::cc info methods role=$role] eq ""} {
              my msg "ignoring unknown role '$role'"
              continue
            }
            set success [::xo::cc role=$role \
                             -user_id [::xo::cc user_id] \
                             -package_id [my package_id]]
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
  WorkflowPage instproc view {{content ""}} {
    # The edit method calls view with an HTML content as argument.
    # To avoid a loop, when "view" is redirected to "edit",
    # we make sure that we only check the redirect on views
    # without content.
    if {[my is_wf_instance] && $content eq ""} {
      set ctx [::xowf::Context require [self]]
      set method [$ctx get_view_method]
      if {$method ne "" && $method ne "view"} {
        #my msg "view redirects to $method in state [$ctx get_current_state]"
        return [[my package_id] invoke -method $method]
      } 
    }
    next
  }

  WorkflowPage instproc get_form_data args {
    if {[my is_wf_instance]} {
      foreach {validation_errors category_ids} [next] break
      if {$validation_errors == 0} {
        #my msg "validation ok, action = [::xo::cc serialize]"
        # todo: we should implement ::xo::cc get_form_parameters]"
	my array set __ia [my set instance_attributes]

        foreach {name value} [::xo::cc array get form_parameter] {
          if {[regexp {^__action_(.+)$} $name _ action]} {
            #my msg action=$action
            set ctx [::xowf::Context require [self]]
            #my msg "action=$action isbj=[my isobject ${ctx}::$action]"
            if {[catch {${ctx}::$action activate [self]} errorMsg]} {
              my msg "error in action: $errorMsg"
            } else {
              set next_state [${ctx}::$action next_state]
              if {$next_state ne ""} {
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
    my instvar page_template
    if {[my is_wf_instance]} {
      set key __wfi(wf_form_id)
      if {![my exists $key]} {
	set ctx [::xowf::Context require [self]]
	my set $key [$ctx form_id [my parent_id]]
	# TODO handle case, when form_id == 0
      }
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
      lappend instance_attributes \
          wf_current_state [$ctx get_current_state]

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
  WorkflowPage instproc merge_constraints {c1 c2} {
    array set __c1 [my constraints_as_array $c1]
    foreach {att value} [my constraints_as_array $c2] {
      set key __c1($att)
      if {[info exists $key]} {append $key ",$value"} else {set $key $value}
    }
    set result [list]
    foreach {att value} [array get __c1] {lappend result $att:$value}
    return $result
  }
  WorkflowPage instproc wfi_merged_form_constraints {constraints_from_form} {
    set ctx [::xowf::Context require [self]]
    #my msg constraints_form_state=[$ctx get_form_constraints]
    return [my merge_constraints $constraints_from_form [$ctx get_form_constraints]]
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
  WorkflowPage instproc footer {} {
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



