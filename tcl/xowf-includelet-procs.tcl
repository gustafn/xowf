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
#   ::xowiki::IncludeletClass create wf \
#       -superclass ::xowiki::Includelet \
#       -parameter {
#         {__decoration plain}
#         {parameter_declaration {
#           {-category_id}
#           {-slideshow:boolean false}
#           {-pagenr 0}
#           {-style standard}
#           {-menu_buttons "view edit copy create delete"}
#         }}
#       }

#   wf instproc render {} {
#     my get_parameters
#     return "..."
#   }

}
