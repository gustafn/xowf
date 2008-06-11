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
          {-package_id ""}
          {-ical 0}
          {-max_entries}
        }}
      }

  wf-todo instproc render {} {
    my get_parameters
    set user_id 539
    
    set sql {
      select assignee,xowiki_form_page_id,state,i.publish_status,page_template,
          i2.name as wf_name,p.title,i.name,o.package_id as pid
      from xowiki_form_pagei p,cr_items i, cr_items i2, acs_objects o 
      where assignee = :user_id 
      and i.live_revision = xowiki_form_page_id 
      and p.page_template = i2.item_id 
      and o.object_id = xowiki_form_page_id
    }
    my msg package_id=$package_id
    if {$package_id ne ""} {
      append sql " and o.package_id = :package_id"
    }
    append sql " order by p.last_modified desc"

    set items [::xowiki::FormPage instantiate_objects -sql $sql]
    
    set t [TableWidget new -volatile \
               -columns {
                 Field wf -label Workflow
                 AnchorField item -label item
                 Field title -label [::xowiki::Page::slot::title set pretty_name]
                 Field state -label [::xowiki::FormPage::slot::state set pretty_name]
                 Field package_id -label package_id
               }]
    foreach i [$items children] {
      $i instvar wf_name name title state xowiki_form_page_id pid
      ::xowf::Package initialize -package_id $pid
      $t add \
          -wf $wf_name \
          -title $title \
          -item $xowiki_form_page_id \
          -item.href [$pid pretty_link $name] \
          -state $state \
          -package_id $pid 
    }
    return [$t asHTML]
  }

}
