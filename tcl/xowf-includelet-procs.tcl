::xo::library doc {
  XoWiki Workflow - includelet definitions

  @author Gustaf Neumann
  @creation-date 2008-03-05
  @cvs-id $Id $
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
  # Create an includelet called s5, which behaves similar 
  # to the book includelet (default), or produces an S5 slide-show,
  # when slideshow flag is true.
  #
  ::xowiki::IncludeletClass create wf-todo \
      -superclass ::xowiki::Includelet \
      -parameter {
        {__decoration plain}
        {parameter_declaration {
          {-workflow}
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
          i2.name as wf_name,p.title,i.name,o.package_id as pid
      from xowiki_form_pagei p,cr_items i, cr_items i2, acs_objects o 
      where assignee = :user_id 
      and i.live_revision = xowiki_form_page_id 
      and p.page_template = i2.item_id 
      and o.object_id = xowiki_form_page_id
    }
    if {[info exists workflow]} {
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
      $i instvar wf_name name title state xowiki_form_page_id pid creation_date last_modified description
      ::xowf::Package initialize -package_id $pid
      set tcl_creation_date [::xo::db::tcl_date $creation_date tz]
      set tcl_last_modified [::xo::db::tcl_date $last_modified tz]
      set dtstamp   [::xo::ical clock_to_utc [clock scan $tcl_creation_date]]
      set dtlastmod [::xo::ical clock_to_utc [clock scan $tcl_last_modified]]
      set dtstamp   [::xo::ical clock_to_utc [clock scan $tcl_creation_date]]
      set description "Workflow instance of workflow $wf_name $description"
#PRIORITY:1
#STATUS:IN-PROCESS
#PERCENT-COMPLETE:25
      append t "BEGIN:VTODO
CREATED:$dtstamp
LAST-MODIFIED:$dtlastmod
DTSTAMP:$dtstamp
UID:$pid-$xowiki_form_page_id
SUMMARY:[::xo::ical text_to_ical $title]
URL:[$pid pretty_link -absolute true $name] 
DESCRIPTION:[::xo::ical text_to_ical $description]
END:VTODO
"
    }
    set text "BEGIN:VCALENDAR
PRODID:-//WU Wien//NONSGML XoWiki Content Flow//EN
VERSION:2.0
${t}END:VCALENDAR
"
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
                 AnchorField item -label item
                 Field title -label [::xowiki::Page::slot::title set pretty_name]
                 Field state -label [::xowiki::FormPage::slot::state set pretty_name]
               }]
    foreach i [$items children] {
      $i instvar wf_name name title state xowiki_form_page_id pid
      ::xowf::Package initialize -package_id $pid
      $t add \
          -wf $wf_name \
          -wf.href [$pid pretty_link $wf_name] \
          -title $title \
          -item $xowiki_form_page_id \
          -item.href [$pid pretty_link $name] \
          -state $state \
          -package [$pid package_url]
    }
    return [$t asHTML]
  }

}
