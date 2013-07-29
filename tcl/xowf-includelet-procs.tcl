::xo::library doc {
  XoWiki Workflow - includelet definitions

  @author Gustaf Neumann
  @creation-date 2008-03-05
}

::xo::db::require package xowiki


namespace eval ::xowiki::includelet {
  #
  # Define additional elements for includelets
  #
  Class form-menu-button-wf-instances -superclass ::xowiki::includelet::form-menu-button-answers
  Class form-menu-button-wf -superclass form-menu-button -parameter {
    {method view}
  }

  #
  # Create an includelet called wf-todo, which lists the todo items 
  # for a user_id from a single or multiple worflows)
  #
  ::xowiki::IncludeletClass create wf-todo \
      -superclass ::xowiki::Includelet \
      -parameter {
        {__decoration plain}
        {parameter_declaration {
          {-workflow ""}
          {-user_id}
          {-ical 0}
          {-max_entries}
        }}
      }

  wf-todo instproc initialize {} {
    my get_parameters
    if {![info exists user_id]} {set user_id [::xo::cc user_id]}

    set sql {
      select assignee,xowiki_form_page_id,state,i.publish_status,page_template,
          p.creation_date, p.last_modified, p,description,
          i2.name as wf_name,p.title,i.name,i.parent_id,o.package_id as pid
      from xowiki_form_pagei p,cr_items i, cr_items i2, acs_objects o 
      where (assignee = :user_id or acs_group__member_p(:user_id,assignee, 'f'))
      and i.live_revision = xowiki_form_page_id 
      and p.page_template = i2.item_id 
      and o.object_id = xowiki_form_page_id
    }
    if {$workflow ne ""} {
      # The workflow might be of one of the following forms:
      #   name
      #   <language prefix>:name
      #   //package/name
      #   //package/<language prefix>:name
      #
      # To address all workflow of a package instance, use
      #   //package/
      #
      if {[regexp {^/(/.*)/$} $workflow _ package]} {
        ::xowf::Package initialize -url $package
        #my msg "using package_id=$package_id"
        append sql " and o.package_id = :package_id"
      } else {
        if {[catch {set wf_page [[my set __including_page] resolve_included_page_name $workflow]}]} {
          set package_id -1; set page_template -1
        } else {
          set page_template [$wf_page item_id]
          set package_id [$wf_page package_id]
          my msg "could not find workflow $workflow"
        }
        #my msg "page_template=$page_template pkg=$package_id"
        append sql " and o.package_id = :package_id and p.page_template = :page_template"
      }
    }

    append sql " order by p.last_modified desc"

    my set items [::xowiki::FormPage instantiate_objects -sql $sql]
  }

  wf-todo instproc render_ical {} {
    my instvar items
    foreach i [$items children] {
      $i instvar wf_name name title state xowiki_form_page_id pid description parent_id
      ::xowf::Package initialize -package_id $pid

      $i class ::xo::ical::VTODO
      $i configure -uid $pid-$xowiki_form_page_id \
          -url [$pid pretty_link -absolute true $parent_id $name] \
          -summary "$title ($state)" \
          -description "Workflow instance of workflow $wf_name $description"
    }
    $items mixin ::xo::ical::VCALENDAR
    $items configure -prodid "-//WU Wien//NONSGML XoWiki Content Flow//EN" 
    set text [$items as_ical]
    #my log "--ical sending $text"
    #ns_return 200 text/calendar $text
    ns_return 200 text/plain $text
  }

  wf-todo instproc render {} {
    my get_parameters
    if {$ical} {return [my render_ical]}

    my instvar items
    set t [TableWidget new -volatile \
               -columns {
                 Field package -label Package
                 AnchorField wf -label Workflow
                 AnchorField title -label "Todo"
                 Field state -label [::xowiki::FormPage::slot::state set pretty_name]
               }]
    foreach i [$items children] {
      $i instvar wf_name name title state xowiki_form_page_id pid parent_id
      ::xowf::Package initialize -package_id $pid
      $t add \
          -wf $wf_name \
          -wf.href [$pid pretty_link -parent_id $parent_id $wf_name] \
          -title $title \
          -title.href [$pid pretty_link -parent_id $parent_id $name] \
          -state $state \
          -package [$pid package_url]
    }
    return [$t asHTML]
  }

}
