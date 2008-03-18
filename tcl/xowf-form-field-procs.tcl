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
    {validator workflow}
  }
  FormField::workflow_definition instproc as_graph {} {
    set ctx [::xowf::Context new -destroy_on_cleanup -object [my object] \
		 -workflow_definition [my value]]
    return [$ctx as_graph]
  }
  FormField::workflow_definition instproc check=workflow {value} {
    # Do w have a syntax error in the workflow definition?
    if {![catch {set ctx [::xowf::Context new -destroy_on_cleanup -object [my object] \
		 -workflow_definition [my value]]} errorMsg]} {
      set errorMsg ""
      foreach s [$ctx available_states]  {set state([$s name])  $s}
      foreach a [$ctx available_actions] {set action([$a name]) $a}
      foreach a [$ctx available_actions] {
        # Are some "next_states" undefined?
        set next_state [$a next_state]
        if {$next_state ne "" && ![info exists state($next_state)]} {
          set errorMsg "Error in action [$a name]: no such state '$next_state' defined \
		(valid: [lsort [array names state]])"
        }
      }
      foreach s [$ctx available_states] {
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
	<div style='width: 33%; float: right;'>[my as_graph]</div><div class='visual-clear'></div>"
  }


  ###########################################################
  #
  # ::xowiki::FormField::current_state
  #
  ###########################################################
  Class FormField::current_state -superclass ::xowiki::FormField::label

}