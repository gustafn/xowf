::xo::library doc {
  XoWiki Workflow - form field procs

  @author Gustaf Neumann
  @creation-date 2008-03-05
}

::xo::db::require package xowiki

namespace eval ::xowiki::formfield {
  ###########################################################
  #
  # ::xowiki::formfield::workflow_definition
  #
  ###########################################################

  Class workflow_definition -superclass textarea -parameter {
    {rows 20}
    {cols 80}
    {dpi 120}
  } -extend_slot validator workflow

  workflow_definition instproc as_graph {} {
    set ctx [::xowf::Context new -destroy_on_cleanup -object [my object] \
                 -all_roles true -in_role none \
		 -workflow_definition [my value] ]
    return [$ctx as_graph -dpi [my dpi] -style "max-width: 35%;"]
  }

  workflow_definition instproc check=workflow {value} {
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
  workflow_definition instproc pretty_value {v} {
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
  # ::xowiki::formfield::current_state
  #
  ###########################################################
  Class current_state -superclass label -parameter {
    {as_graph true}
  }
  current_state instproc render_input {} {
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

  current_state instproc pretty_value {v} {
    set g ""
    if {[my as_graph]} {
      set ctx   [::xowf::Context require [my object]]
      set graph [$ctx as_graph -current_state $v -visited [[my object] visited_states]]
      set g "<div style='width: 35%; float: right;'>$graph</div>"
    }
    return "[next]$g"
  }

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
    set members [db_list_of_lists [my qn get_admins] "select distinct o.title, p.party_id
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




namespace eval ::xowiki::formfield {

  ###########################################################
  #
  # ::xowiki::formfield::role_member
  #
  ###########################################################

  Class role_member -superclass select -parameter {role}
  role_member instproc initialize {} {
    next
    my set is_party_id 1
  }
  role_member instproc render_input {} {
    my instvar role
    #my msg role=$role,obj=[my object]
    if {[info command ::xo::role::$role] ne ""} {
      my set options [::xo::role::$role get_members -object_id [[my object] package_id]]
    } elseif {[set gid [group::get_id -group_name $role]] ne ""} {
      my set options [list]
      foreach m [group::get_members -group_id $gid] {
        my lappend options [list [::xo::get_user_name $m] $m] }
    } else {
      error "no such role or group '$role'"
    }
    next
  }
  role_member instproc pretty_value {v} {
    return [::xo::get_user_name $v]
  }
}

namespace eval ::xowiki::formfield {

  ###########################################################
  #
  # ::xowiki::formfield::mc_exercise
  #
  ###########################################################

  Class mc_exercise -superclass CompoundField -parameter {
    {feedback full}
    {inplace true}
  }

  mc_exercise instproc initialize {} {
    if {[my set __state] ne "after_specs"} return
    set javascript [::xowiki::formfield::FormField fc_encode { 
      xinha_config.toolbar = [ 
			      ['popupeditor', 'bold','italic','createlink','insertimage','separator'], 
			      ['killword','removeformat','htmlmode'] 
			     ]; 
    }]
    my instvar feedback inplace
    my create_components  [subst {
      {text  {richtext,required,editor=xinha,height=150px,label=#xowf.exercise-text#,plugins=OacsFs,javascript=$javascript,inplace=$inplace}}
      {alt-1 {mc_alternative,feedback=$feedback,label=#xowf.alternative#,inplace=$inplace}}
      {alt-2 {mc_alternative,feedback=$feedback,label=#xowf.alternative#,inplace=$inplace}}
      {alt-3 {mc_alternative,feedback=$feedback,label=#xowf.alternative#,inplace=$inplace}}
      {alt-4 {mc_alternative,feedback=$feedback,label=#xowf.alternative#,inplace=$inplace}}
      {alt-5 {mc_alternative,feedback=$feedback,label=#xowf.alternative#,inplace=$inplace}}
    }]
    my set __initialized 1
  }

  mc_exercise instproc render_input {} {
    ::xo::Page requireCSS /resources/xowf/myform.css
    next
  }

  mc_exercise instproc pretty_value {v} {
    return [[my object] property form ""]
  }

  mc_exercise instproc convert_to_internal {} {
    #
    # Build a from from the componets of the exercise on the fly.
    # Actually, this methods computes the properties "form" and
    # "form_constraints" based on the components of this form field.
    # 
    set form "<FORM>\n<table class='mchoice'>\n<tbody>"
    set fc "@categories:off @cr_fields:hidden\n"
    set intro_text [[my get_named_sub_component text] value]
    append form "<tr><td class='text' colspan='2'>$intro_text</td></tr>\n"
    foreach alt {alt-1 alt-2 alt-3 alt-4 alt-5} {
      foreach f {text correct feedback_correct feedback_incorrect} {
	if {[my exists_named_sub_component $alt $f]} {
	  set value($f) [[my get_named_sub_component $alt $f] value]
	} else {
	  set value($f) ""
	}
      }
      append form \
          "<tr><td class='selection'><input type='checkbox' name='$alt' /></td>\n" \
          "<td class='value'>$value(text)</td></tr>\n"
      set alt_fc [list]
      if {$value(correct)} {lappend alt_fc "answer=on"} else {lappend alt_fc "answer="}
      if {$value(feedback_correct) ne ""} {
        lappend alt_fc "feedback_answer_correct=[::xowiki::formfield::FormField fc_encode $value(feedback_correct)]"
      }
      if {$value(feedback_incorrect) ne ""} {
        lappend alt_fc "feedback_answer_incorrect=[::xowiki::formfield:::FormField fc_encode $value(feedback_incorrect)]"
      }
      if {[llength $alt_fc] > 0} {append fc [list $alt:[join $alt_fc ,]]\n}
      #my msg "$alt .correct = $value(correct)"
    }
    append form "</tbody></table></FORM>\n"
    [my object] set_property -new 1 form $form
    [my object] set_property -new 1 form_constraints $fc
    my set __refresh_instance_attributes [list form $form form_constraints $fc]
  }

  ###########################################################
  #
  # ::xowiki::formfield::mc_alternative
  #
  ###########################################################

  Class mc_alternative -superclass CompoundField -parameter {
    {feedback full}
    {inplace true}
  }

  mc_alternative instproc initialize {} {
    if {[my set __state] ne "after_specs"} return

    if {1} {
      set javascript [::xowiki::formfield::FormField fc_encode { 
	xinha_config.toolbar = [ 
				['popupeditor', 'bold','italic','createlink','insertimage','separator'], 
				['killword','removeformat','htmlmode'] 
			       ]; 
      }]
      set text_config [subst {editor=xinha,height=100px,label=Text,plugins=OacsFs,inplace=[my inplace],javascript=$javascript}]
    } else {
      set text_config [subst {editor=wym,height=100px,label=Text}]
    }
    if {[my feedback] eq "full"} {
      set feedback_fields {
	{feedback_correct {textarea,label=Feedback korrekt}}
	{feedback_incorrect {textarea,label=Feedback inkorrekt}}
      }
    } else {
      set feedback_fields ""
    }
    my create_components [subst {
      {text  {richtext,$text_config}}
      {correct {boolean,horizontal=true,label=Korrekt}}
      $feedback_fields
    }]
    my set __initialized 1
  }

}

namespace eval ::xowiki::formfield {

  ###########################################################
  #
  # ::xowiki::formfield::questionaire
  #
  ###########################################################

  Class questionaire -superclass {form_page} -parameter {
    {multiple true}
  }

  questionaire instproc convert_to_internal {} {
    #
    # Build a complex form composed of the specified form pages names
    # contained in the value of this field.  The form-fields have to
    # be renamed. This affects the input field names in the form and
    # the form constaints. We use the item-id contained pages as a the
    # prefix for the form-fields. This method must be most likely
    # extended for other question types.
    # 
    set form "<FORM>\n"
    set fc "@categories:off @cr_fields:hidden\n"
    set intro_text [[my object] property _text]
    append form "$intro_text\n<ol>\n"
    foreach v [my value] {
      # TODO: the next two commands should not be necessary to lookup
      # again, since the right values are already loaded into the
      # options
      set item_id [[[my object] package_id] lookup -name $v]
      set page [::xo::db::CrClass get_instance_from_db -item_id $item_id]
      append form "<li><h2>[$item_id title]</h2>\n"
      set prefix c$item_id-
      array set __ia [$page set instance_attributes]
      #
      # If for some reason, we have not form entry, we ignore it.
      # TODO: We should deal here with computed forms and with true
      # ::xowiki::forms as well...
      #
      if {![info exists __ia(form)]} {
        my msg "$v has no form included"
        continue
      }
      #
      # Replace the form-field names in the form
      #
      dom parse -simple -html $__ia(form) doc
      $doc documentElement root
      set alt_inputs [list]
      foreach n [$root selectNodes "//input\[@name != ''\]"] {
        set alt_input [$n getAttribute name]
        $n setAttribute name $prefix-$alt_input
        lappend alt_inputs $alt_input
      }
      # We have to drop the toplevel <FORM> of the included form
      foreach n [$root childNodes] {append form [$n asHTML]}
      append form "</li>\n"
      #
      # Replace the formfield names in the form constraints
      #
      foreach f $__ia(form_constraints) {
        if {[regexp {^([^:])+:(.*)$} $f _ field_name definition]} {
          # keep all form-constraints for which we have altered the name
          if {[lsearch $alt_inputs $field_name] > -1} {lappend fc $prefix$f}
        }
      }
    }
    append form "</ol></FORM>\n"
    set anon_instances true ;# TODO make me configurable
    [my object] set_property -new 1 form $form
    [my object] set_property -new 1 form_constraints $fc
    [my object] set_property -new 1 anon_instances $anon_instances
    my set __refresh_instance_attributes [list form $form form_constraints $fc anon_instances $anon_instances]
  }
}
