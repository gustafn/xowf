::xo::library doc {
  XoWiki Workflow - form field procs

  @author Gustaf Neumann
  @creation-date 2008-03-05
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
                 -all_roles true -in_role none \
		 -workflow_definition [my value] ]
    return [$ctx as_graph -dpi [my dpi] -style "max-width: 35%;"]
  }

  FormField::workflow_definition instproc check=workflow {value} {
    # Do we have a syntax error in the workflow definition?
    if {![catch {set ctx [::xowf::Context new -destroy_on_cleanup -object [my object] \
                 -all_roles true \
		 -workflow_definition [my value]]} errorMsg]} {
      unset errorMsg
      array set "" [$ctx check]
      if {$(rc) == 1} {set errorMsg $(errorMsg)}
    }
    
    if {[info exists errorMsg]} {
      #my msg errorMsg=$errorMsg
      my uplevel [list set errorMsg $errorMsg]
      return 0
    }
    return 1
  }
  FormField::workflow_definition instproc pretty_value {v} {
    [my object] do_substitutions 0
    set text [string map [list & "&amp;" < "&lt;" > "&gt;" \" "&quot;" ' "&apos;" @ "&#64;"] [my value]]
    return "<div style='width: 65%; overflow:auto;float: left;'>
	<pre class='code'>$text</pre></div>
	<div float: right;'>[my as_graph]</div><div class='visual-clear'></div>
        [[my object] include my-refers] 
   "
  }


  ###########################################################
  #
  # ::xowiki::FormField::current_state
  #
  ###########################################################
  Class FormField::current_state -superclass ::xowiki::FormField::label -parameter {
    {as_graph true}
  }
  FormField::current_state instproc render_input {} {
    next
    if {[my as_graph]} {
      set ctx [::xowf::Context new -destroy_on_cleanup -object [my object] \
		   -all_roles true -in_role none \
		   -workflow_definition [[my object] wf_property workflow_definition] ]
      #set ctx   [::xowf::Context require [my object]]
      set graph [$ctx as_graph -current_state [my value] -visited [[my object] visited_states]]
      ::html::div -style "width: 35%; float: right;" {
	::html::t -disableOutputEscaping $graph
      }
    }
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


# 
# these definitons are only here for the time being 
#
namespace eval ::xo::role {
  Class Role
  Role instproc get_members args {
    error "get_members are not implemented for [self]"
  }

  Role create all
  all proc is_member {-user_id:required -package_id} {
    return 1
  }

  Role create swa 
  swa proc is_member {-user_id:required -package_id} {
    return [::xo::cc cache [list acs_user::site_wide_admin_p -user_id $user_id]]
  }
  
  Role create registered_user 
  registered_user proc is_member {-user_id:required -package_id} {
    return [expr {$user_id != 0}]
  }

  Role create unregistered_user
  unregistered_user proc is_member {-user_id:required -package_id} {
    return [expr {$user_id == 0}]
  }
  
  Role create admin 
  admin proc is_member {-user_id:required -package_id:required} {
    return [::xo::cc permission -object_id $package_id -privilege admin -party_id $user_id]
  }
  admin proc get_members {-object_id:required} {
    set members [db_list_of_lists get_admins "select distinct  o.title, p.party_id
      from acs_object_party_privilege_map p, acs_objects o
      where p.object_id = $object_id and p.privilege = 'admin' and o.object_id = p.party_id"]
    #my msg members=$members
    return $members
  }
  
  Role create creator
  creator proc is_member {-user_id:required -package_id -object:required} {
    $object instvar creation_user
    return [expr {$creation_user == $user_id}]
  }

  Role create app_group_member 
  app_group_member proc is_member {-user_id:required -package_id} {
    return [::xo::cc cache [list application_group::contains_party_p \
                          -party_id $user_id \
                          -package_id $package_id]]
  }

  Role create community_member
  community_member proc is_member {-user_id:required -package_id} {
    if {[info command ::dotlrn_community::get_community_id] ne ""} {
      set community_id [my cache [list [dotlrn_community::get_community_id -package_id $package_id]]]
      if {$community_id ne ""} {
        return [my cache [list dotlrn::user_is_community_member_p \
                              -user_id $user_id \
                              -community_id $community_id]]
      }
    }
    return 0
  }

}




namespace eval ::xowiki {

  ###########################################################
  #
  # ::xowiki::FormField::role_member
  #
  ###########################################################

  Class FormField::role_member -superclass FormField -superclass FormField::select -parameter {role}
  FormField::role_member instproc render_input {} {
    my instvar role
    #my msg role=$role,obj=[my object]
    my set options [::xo::role::$role get_members -object_id [[my object] package_id]]
    #foreach m $members { my lappend options [list [::xo::get_user_name $m] $m] }
    next
  }
  FormField::role_member instproc pretty_value {v} {
    return [::xo::get_user_name $v]
  }
}