if {[info command ::ns_cache_eval] eq ""} {proc ::ns_cache_eval {args} {::ns_cache eval {*}$args}}
# register the dav interface for the todos
::xowf::dav-todo register

# run the checker for the scheduled at-jobs
ad_schedule_proc -thread t 60 ::xowf::atjob check

# make sure, we have not missed some at-jobs, while we were down
::xowf::atjob check -with_older true