::xo::library doc {
  XoWiki Workflow - form field procs

  @author Gustaf Neumann
  @creation-date 2008-03-05
  @cvs-id $Id $
}

::xo::db::require package xowiki

namespace eval ::xowiki {
  ###########################################################
  #
  # ::xowiki::FormField::workflow_definition
  #
  ###########################################################

  Class FormField::workflow_definition -superclass ::xowiki::FormField::textarea -parameter {
    {rows 20}
    {cols 80}
    {dpi 120}
  } -extend_slot validator workflow

  FormField::workflow_definition instproc as_graph {} {
    set ctx [::xowf::Context new -destroy_on_cleanup -object [my object] \
		 -workflow_definition [my value] ]
    return [$ctx as_graph -dpi [my dpi] -style "max-width: 35%;"]
  }
  FormField::workflow_definition instproc check=workflow {value} {
    # Do we have a syntax error in the workflow definition?
    if {![catch {set ctx [::xowf::Context new -destroy_on_cleanup -object [my object] \
		 -workflow_definition [my value]]} errorMsg]} {
      set errorMsg ""
      foreach s [$ctx defined State]  {set state([$s name])  $s}
      foreach a [$ctx defined Action] {set action([$a name]) $a}
      foreach a [$ctx defined Action] {
        # Are some "next_states" undefined?
        set next_state [$a next_state]
        if {$next_state ne "" && ![info exists state($next_state)]} {
          set errorMsg "Error in action [$a name]: no such state '$next_state' defined \
		(valid: [lsort [array names state]])"
        }
      }
      foreach s [$ctx defined State] {
        # Are some "actions" undefined?
        foreach a [$s actions] {
          if {![info exists action($a)]} {
            set errorMsg "Error in state [$s name]: no such action '$a' defined \
		(valid: [lsort [array names action]])"
          }
        }
      }
    }
    if {[info exists errorMsg]} {
      my uplevel [list set errorMsg $errorMsg]
      return 0
    }
    return 1
  }
  FormField::workflow_definition instproc pretty_value {v} {
    [my object] do_substitutions 0
    return "<div style='width: 65%; overflow:auto;float: left;'>
	<pre class='code'>[my value]</pre></div>
	<div float: right;'>[my as_graph]</div><div class='visual-clear'></div>"
  }


  ###########################################################
  #
  # ::xowiki::FormField::current_state
  #
  ###########################################################
  Class FormField::current_state -superclass ::xowiki::FormField::label -parameter {
    {as_graph true}
  }

  FormField::current_state instproc pretty_value {v} {
    set g ""
    if {[my as_graph]} {
      set ctx   [::xowf::Context require [my object]]
      set graph [$ctx as_graph -current_state $v -visited [[my object] visited_states]]
      set g "<div style='width: 35%; float: right;'>$graph</div>"
    }
    return "[next]$g"
  }


  ###########################################################
  #
  # ::xowiki::FormField::form
  #
  ###########################################################

  Class FormField::form -superclass FormField::richtext -parameter {
    {height 200}
  } -extend_slot validator form

  FormField::form instproc check=form {value} {
    set form $value
    my msg form=$form
    dom parse -simple -html $form doc
    $doc documentElement root
    
    return [expr {[$root nodeName] eq "form"}]
  }

  ###########################################################
  #
  # ::xowiki::FormField::form_constraints
  #
  ###########################################################

  Class FormField::form_constraints -superclass FormField::textarea -parameter {
    {rows 5}
  } -extend_slot validator form_constraints
  # the form_constraints checker is defined already on the ::xowiki::Page level

}