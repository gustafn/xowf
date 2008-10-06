namespace eval ::xowf {
  #
  # simple at-handler
  #
  
  #
  # define a simple acs-type for at jobs
  #
#   ::xo::db::Class create ::xowf::atjob -superclass ::xo::db::Object  -slots {
#     ::xo::db::Attribute create owner_id -datatype integer -references acs_objects(object_id)
#     ::xo::db::Attribute create party_id -datatype integer -references parties(party_id)
#     ::xo::db::Attribute create cmd
#     ::xo::db::Attribute create done -datatype boolean -default f
#     ::xo::db::Attribute create time -datatype timestamp
#     ::xo::Attribute create object
#   }
#   ::xo::db::require index -table xowf_atjob -col done
#   ::xo::db::require index -table xowf_atjob -col time

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

#   atjob instproc persist {} {
#     my instvar party_id cmd

#     set class [self class]
#     set owner_id [[my object] item_id]
#     set package_id [[my object] package_id]
#     set sql_timestamp [$class sql_timestamp [clock scan [my time]]]
#     if {![info exists party_id]} {my party_id [::xo::cc set untrusted_user_id]}

#     set already_recorded [db_0or1row [my qn is_recorded] "
#       select 1 from [$class table_name]
#       where owner_id = :owner_id and party_id = :party_id and cmd = :cmd 
#       and time = $sql_timestamp"]

#     if {$already_recorded} {
#       my debug_message "entry already recorded"
#     } else {
#       $class get_context package_id creation_user creation_ip
#       db_transaction {
#         set id [$class new_acs_object \
#                     -package_id $package_id \
#                     -creation_user $creation_user \
#                     -creation_ip $creation_ip \
#                     ""]
#         $class initialize_acs_object [self] $id
#         db_dml [my qn insert_job] \
#             "insert into [$class table_name] ([$class id_column], owner_id, time, party_id, cmd) \
#              values ($id, $owner_id, $sql_timestamp, $party_id, :cmd)"
#       }
#     }
#   }


#   atjob proc check {{-with_older false}} {
#     #
#     # check, if there is something to do
#     #
#     set op [expr {$with_older ? "<=" : "=" }]
#     set items [my get_instances_from_db \
#                    -where_clause "and done='f' and time $op [my sql_timestamp [clock seconds]]"]
    
#     foreach job [$items children] {
#       my log "\n*** job=[$job serialize] ***\n"
#       set owner_id [$job owner_id]

#       # We assume, the owner object is a cr-item
#       ::xo::db::CrClass get_instance_from_db -item_id $owner_id

#       # We assume, the package is from the xowiki family; make sure, the url looks like real
#       ::xo::Package initialize \
#           -package_id [$owner_id package_id] \
#           -user_id [$job party_id] \
#           -init_url 0 -actual_query ""
#       $package_id set_url -url [$package_id package_url][$owner_id name]

#       my log "\n*** executing atjob [$job cmd] ***\n"
#       if {[catch {eval [$job cmd]} errorMsg]} {
#         ns_log error "\n*** atjob [$job cmd] lead to error ***\n$errorMsg"
#       } else {
#         $job done t
#         $job save
#       }
#     }
#   }

  #
  # 
  # temporary cleanup
  #
  # delete from acs_objects where object_type  = '::xowf::atjob';
  # delete from acs_attributes where object_type = '::xowf::atjob';
  # delete from acs_object_types where object_type = '::xowf::atjob';
  # drop table xowf_atjob;

  atjob set form_name en:atjob-form

  atjob instproc persist {} {
    my instvar party_id cmd

    set class [self class]
    set owner_id [[my object] item_id]
    set package_id [[my object] package_id]
    #set sql_timestamp [$class sql_timestamp [clock scan [my time]]]
    set ansi_time [$class ansi_time [clock scan [my time]]]
    if {![info exists party_id]} {my party_id [::xo::cc set untrusted_user_id]}

    set form_id [$class form_id -package_id $package_id -parent_id [[my object] parent_id]]
    if {$form_id != 0} {
      ::xo::db::CrClass get_instance_from_db -item_id $form_id
      set instance_attributes [$form_id default_instance_attributes]
      lappend instance_attributes cmd $cmd
      set f [::xowiki::FormPage new -destroy_on_cleanup \
                 -package_id $package_id \
                 -parent_id $owner_id \
                 -nls_language [[my object] nls_language] \
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
    set items [::xowiki::FormPage instantiate_objects \
                   -object_class ::xowiki::FormPage \
                   -sql "
                      select i.item_id, i.name, i.parent_id, i.publish_status, o.creation_user, 
                             r.revision_id, page_template,instance_attributes 
                      from cr_items i, cr_items i2, cr_revisions r, xowiki_page_instance t, acs_objects o
                      where i.item_id = r.item_id and i.live_revision = r.revision_id 
                      and r.revision_id = t.page_instance_id and o.object_id = i.item_id
                      and i2.item_id = i.parent_id and i2.content_type = '::xowiki::FormPage'
                      and r.publish_date $op to_timestamp('$ansi_time','YYYY-MM-DD HH24:MI')
                      and i.publish_status = 'production'
                    " ]

    my log "--at we got [llength [$items children]] scheduled items"

    foreach item [$items children] {
      my log "--at *** job=[$item serialize] ***\n"
      set owner_id [$item parent_id]
      set party_id [$item creation_user]
      array unset __ia
      array set __ia [$item instance_attributes]
      if {![info exists __ia(cmd)]} {
        ns_log notice "--at ignore strange entry [$item serialize]"
        continue
      }
      set cmd $__ia(cmd)

      # We assume, the owner object is a cr-item
      ::xo::db::CrClass get_instance_from_db -item_id $owner_id

      # We assume, the package is from the xowiki family; make sure, the url looks like real
      ::xo::Package initialize \
          -package_id [$owner_id package_id] \
          -user_id $party_id \
          -init_url 0 -actual_query ""
      $package_id set_url -url [$package_id package_url][$owner_id name]

      my log "--at executing atjob $cmd"
      if {[catch {eval $cmd} errorMsg]} {
        ns_log error "\n*** atjob $cmd lead to error ***\n$errorMsg"
      } else {
        $item set_live_revision -revision_id [$item revision_id] -publish_status "expired"
      }
    }
    my log "--at END"
  }

}
