namespace eval ::xowf {
  #
  # simple at-handler
  #
  
  #
  # define a simple acs-type for at jobs
  #
  Class create ::xowf::atjob -slots {
    ::xo::Attribute create owner_id
    ::xo::Attribute create party_id
    ::xo::Attribute create cmd
    ::xo::Attribute create time
    ::xo::Attribute create object
  }

  atjob proc sql_timestamp {tcltime} {
    # make time accurate by minute
    set sql_stamp [clock format $tcltime -format "%Y-%m-%d %H:%M"]
    return "TO_TIMESTAMP('$sql_stamp','YYYY-MM-DD HH24:MI')"
  }
  atjob proc ansi_time {tcltime} {
    return [clock format $tcltime -format "%Y-%m-%d %H:%M"]
  }

  atjob instproc init {} {
    my destroy_on_cleanup
  }

  #
  # 
  # temporary cleanup
  #
  # delete from acs_objects where object_type  = '::xowf::atjob';
  # delete from acs_attributes where object_type = '::xowf::atjob';
  # delete from acs_object_types where object_type = '::xowf::atjob';
  # drop table xowf_atjob;

  atjob instproc persist {} {
    my instvar party_id cmd

    set class [self class]
    set owner_id [[my object] item_id]
    set package_id [[my object] package_id]
    set ansi_time [$class ansi_time [clock scan [my time]]]
    if {![info exists party_id]} {my party_id [::xo::cc set untrusted_user_id]}

    set form_id [$class form_id -package_id $package_id -parent_id [[my object] parent_id]]
    if {$form_id != 0} {
      ::xo::db::CrClass get_instance_from_db -item_id $form_id
      set instance_attributes [$form_id default_instance_attributes]
      lappend instance_attributes cmd $cmd
      set name [::xowiki::autoname new -name [$form_id name] -parent_id $owner_id] 
      set f [::xowiki::FormPage new -destroy_on_cleanup \
                 -package_id $package_id \
                 -parent_id $owner_id \
                 -name $name \
                 -nls_language [$form_id nls_language] \
                 -publish_status "production" \
                 -publish_date $ansi_time \
                 -creation_user $party_id \
                 -instance_attributes $instance_attributes \
                 -page_template $form_id]
      $f save_new -use_given_publish_date true
      my log "--at formpage saved"
    }
  }

  atjob proc form_id {-parent_id -package_id} {
    set form_name en:atjob-form
    set form_id [::xo::db::CrClass lookup -name $form_name -parent_id $parent_id]
    if {$form_id == 0} {
      set page [$package_id resolve_page $form_name __m]
      if {$page ne ""} {set form_id [$page item_id]}
      if {$form_id == 0} {
        ns_log error "Cannot lookup form $form_name; ignore request"
      }
    }
    return $form_id
  }
  
  atjob proc run_jobs {item_ids} {
    my log "---run xowf jobs START"

    set items [::xowiki::FormPage instantiate_objects \
		   -object_class ::xowiki::FormPage \
		   -sql "select i.item_id, i.name, i.parent_id, i.publish_status, o.creation_user,
                             i.live_revision as revision_id, page_template, instance_attributes
                      	from cr_items i, xowiki_page_instance t, acs_objects o
			where i.item_id in ([join $item_ids ,]) and
			i.live_revision = t.page_instance_id and o.object_id = i.item_id"]

    my log "--at we got [llength [$items children]] scheduled items"

    foreach item [$items children] {
      #my log "--at *** job=[$item serialize] ***\n"
      set owner_id [$item parent_id]
      set party_id [$item creation_user]
      set __ia [$item instance_attributes]
      if {![dict exists $__ia cmd]} {
        #ns_log notice "--at ignore strange entry [$item serialize]"
        ns_log notice "--at ignore strange entry, no cmd in [$item instance_attributes]"
        continue
      }
      set cmd [dict get $__ia cmd]

      # We assume, the owner object is a cr-item
      ::xo::db::CrClass get_instance_from_db -item_id $owner_id

      # We assume, the package is from the xowiki family; make sure, the url looks like real
      ::xo::Package initialize \
          -package_id [$owner_id package_id] \
          -user_id $party_id \
          -init_url 0 -actual_query ""
      $package_id set_url -url [$package_id package_url][$owner_id name]

      my log "--at executing atjob $cmd"
      if {[catch {eval $owner_id $cmd} errorMsg]} {
        ns_log error "\n*** atjob $owner_id $cmd lead to error ***\n$errorMsg"
      } else {
        $item set_live_revision -revision_id [$item revision_id] -publish_status "expired"
      }
    }
    my log "---run xowf jobs END"
  }

  atjob proc check {{-with_older false}} {
    my log "--at START"
    #
    # check, if there are jobs scheduled for execution
    #
    set op [expr {$with_older ? "<=" : "=" }]
    set ansi_time [my ansi_time [clock seconds]]

    #
    # Get the entries.  The items have to be retrieved bottom up,
    # since the query iterates over all instances. In most situations,
    # we fetch the values only for the current time (when with_older
    # is not set). The entries have to be in state "'production" and
    # have to have a parent_id that is an ::xowiki::FormPage. This
    # reduced the number of hits significantly and seems sufficiently
    # fast.
    #
    # To make sure we are not fetching pages from unmounted instances
    # we check for package_id not null.
    #
    set item_ids ::xo::dc list get_due_atjobs "
                      select i.item_id
                      from cr_items i, cr_items i2, cr_revisions r, xowiki_page_instance t, acs_objects o
                      where i.item_id = r.item_id and i.live_revision = r.revision_id 
                      and r.revision_id = t.page_instance_id and o.object_id = i.item_id
                      and i2.item_id = t.page_template and i2.content_type = '::xowiki::Form'
                      and i2.name = 'en:atjob-form'
                      and r.publish_date $op to_timestamp(:ansi_time,'YYYY-MM-DD HH24:MI')
                      and i.publish_status = 'production'
		      and o.package_id is not null
                    " ]

    my log "--at we got [llength $item_ids] scheduled items"
    
    #
    # Running the jobs here in this proc could lead to a problem with
    # the exact match for the time, when e.g. the jobs take longer
    # than one minute. Therefore, we collect the jobs ids here but we
    # execute these in a separate thread via a job queue without
    # waiting. If the list of jobs gets large, we might consider
    # splitting the list and run multiple jobs in parallel.
    #
    if {[llength $item_ids]} {
        set queue xowfatjobs
        if {$queue ni [ns_job queues]} {
            ns_job create $queue
        }
        ns_job queue -detached $queue [list ::xowf::atjob run_jobs $item_ids]
    }

    my log "--at END"
  }

}
