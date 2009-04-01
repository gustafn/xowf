namespace eval ::xowiki::formfield {
  ###########################################################
  #
  # ::xowiki::formfield::FormGeneratorField
  #
  ###########################################################

  Class FormGeneratorField -superclass CompoundField -parameter {
  }
  FormGeneratorField set abstract 1
  FormGeneratorField instproc pretty_value {v} {
    return [[my object] property form ""]
  }
  FormGeneratorField instproc render_input {} {
    ::xo::Page requireCSS /resources/xowf/myform.css
    next
  }

}
namespace eval ::xowiki::formfield {

  ###########################################################
  #
  # ::xowiki::formfield::test_item
  #
  ###########################################################
  Class test_item -superclass FormGeneratorField -parameter {
    {question_type mc}
    {nr_choices 5}
    {feedback_level full}
  }
  
  #
  # provide a default setting for xinha javascript for test-items
  #
  test_item set xinha(javascript) [::xowiki::formfield::FormField fc_encode { 
    xinha_config.toolbar = [ 
                            ['popupeditor', 'bold','italic','createlink','insertimage','separator'], 
                            ['killword','removeformat','htmlmode'] 
                           ]; 
  }]

  test_item instproc feed_back_definition {auto_correct} {
    #
    # Return the definition of the feed_back widgets depending on the
    # value of auto_correct. If we can't determine automatically,
    # what's wrong, we can't provide different feedback for right or
    # wrong.
    #
    my instvar inplace feedback_level
    if {$feedback_level eq "none"} {
      return ""
    }

    set widget "richtext,editor=xinha,slim=true,inplace=$inplace,plugins=OacsFs,height=150px"
    if {$auto_correct} {
      return [subst {
        {feedback_correct   {$widget,label=#xowf.feedback_correct#}}
        {feedback_incorrect {$widget,label=#xowf.feedback_incorrect#}}
      }]
    }
    return [subst {
      {feedback {$widget,label=#xowf.feedback#}}
    }]
  }

  #
  # test_item is the wrapper for interaction to be used in
  # evaluations. Different wrapper can be defined in a similar way for
  # questionairs, which might need less input fields.
  #
  test_item instproc initialize {} {
    if {[my set __state] ne "after_specs"} return
    my instvar inplace feedback_level
    set options ""
    #
    # Provide some settings for name short-cuts
    #
    switch -- [my question_type] {
      mc { # we should support as well: minChoices, maxChoices, shuffle
           set interaction_class mc_interaction
           set options nr_choices=[my nr_choices]
         }
      sc { # we should support as well: minChoices, maxChoices, shuffle
           set interaction_class mc_interaction
           set options nr_choices=[my nr_choices],multiple=false
         }
      ot { set interaction_class text_interaction }
      default {error "unknown question type: [my question_type]"}
    }

    set auto_correct [expr {[$interaction_class exists auto_correct] && 
                            [$interaction_class set auto_correct] == false ? 0 : 1}]
    
    # For the time being, we set inplace to false, otherwise we can't
    # currently edit empty fields
    set inplace true

    #
    # handle feedback_level
    #
    # The object might be a form, just use the property, if we are on
    # a FormPage.
    if {[[my object] istype ::xowiki::FormPage]} {
      set feedback_level_property [[my object] property feedback_level]
      if {$feedback_level_property ne ""} {
        set feedback_level $feedback_level_property
      }
    }

    #
    my create_components  [subst {
      {minutes numeric,size=2,label=#xowf.Minutes#}
      {grading {select,options={exact exact} {partial partial},default=exact,label=#xowf.Grading-Schema#}}
      {interaction {$interaction_class,$options,feedback_level=$feedback_level,inplace=$inplace,form_item_wrapper_CSSclass=hidden-field-set}}
      [my feed_back_definition $auto_correct]
    }]
    my set __initialized 1
  }


}

namespace eval ::xowiki::formfield {
  ###########################################################
  #
  # ::xowiki::formfield::mc_interaction
  #
  ###########################################################

  Class mc_interaction -superclass FormGeneratorField -parameter {
    {feedback_level full}
    {inplace false}
    {shuffle false}
    {nr_choices 5}
    {multiple true}
  }

  mc_interaction instproc set_compound_value {value} {
    set r [next]
    if {![my multiple]} {
      # For single choice questions, we have a fake-field for denoting
      # the correct entry. We have to distribute this to the radio
      # element, which is rendered.
      set correct_field_name [my get_named_sub_component_value correct]
      if {$correct_field_name ne ""} {
        foreach c [my components] {
          if {[$c name] eq $correct_field_name} {
            ${c}::correct value $correct_field_name
          }
        }
      }
    }
    return $r
  }

  mc_interaction instproc initialize {} {
    if {[my set __state] ne "after_specs"} return
    test_item instvar {xinha(javascript) javascript}
    my instvar feedback_level inplace input_field_names
    #
    # build choices
    #
    set choice_definition "{mc_choice,feedback_level=$feedback_level,label=#xowf.alternative#,inplace=$inplace,multiple=[my multiple]}"
    set input_field_names [my generate_fieldnames [my nr_choices]]
    set choices ""
    if {![my multiple]} {
      append choices "{correct radio,omit}\n"
    }
    foreach n $input_field_names {append choices "{$n $choice_definition}\n"}
    #
    # create component structure
    #
    my create_components  [subst {
      {text  {richtext,required,editor=xinha,height=150px,label=#xowf.exercise-text#,plugins=OacsFs,javascript=$javascript,inplace=$inplace}}
      $choices
    }]
    my set __initialized 1
  }

  mc_interaction instproc convert_to_internal {} {
    #
    # Build a from from the componets of the exercise on the fly.
    # Actually, this methods computes the properties "form" and
    # "form_constraints" based on the components of this form field.
    # 
    set form "<FORM>\n<table class='mchoice'>\n<tbody>"
    set fc "@categories:off @cr_fields:hidden\n"
    set intro_text [my get_named_sub_component_value text]
    append form "<tr><td class='text' colspan='2'><div class='question_text'>$intro_text</div></td></tr>\n"

    #my msg " input_field_names=[my set input_field_names]"
   
    if {![my multiple]} {
      set correct_field_name [my get_named_sub_component_value correct]
    }
    
    foreach input_field_name [my set input_field_names] {
      foreach f {text correct feedback_correct feedback_incorrect} {
        set value($f) [my get_named_sub_component_value $input_field_name $f]
      }
      # skip empty entries
      if {$value(text) eq ""} continue

      #
      # fill values into form
      #
      if {[my multiple]} {
        set correct $value(correct)
        append form \
            "<tr><td class='selection'><input type='checkbox' name='$input_field_name' value='$input_field_name'/></td>\n" \
            "<td class='value'>$value(text)</td></tr>\n"
      } else {
        #my msg $correct_field_name,[my name],$input_field_name
        set correct [expr {"[my name].$input_field_name" eq $correct_field_name}]
        append form \
            "<tr><td class='selection'><input type='radio' name='radio' value='$input_field_name' /></td>\n" \
            "<td class='value'>$value(text)</td></tr>\n"
      }
      #my msg "[array get value] corr=$correct"

      #
      # build form constraints per input field
      #
      set if_fc [list]
      if {$correct} {lappend if_fc "answer=$input_field_name"} else {lappend if_fc "answer="}
      if {$value(feedback_correct) ne ""} {
        lappend if_fc "feedback_answer_correct=[::xowiki::formfield::FormField fc_encode $value(feedback_correct)]"
      }
      if {$value(feedback_incorrect) ne ""} {
        lappend if_fc "feedback_answer_incorrect=[::xowiki::formfield:::FormField fc_encode $value(feedback_incorrect)]"
      }
      if {[llength $if_fc] > 0} {append fc [list $input_field_name:checkbox,[join $if_fc ,]]\n}
      #my msg "$input_field_name .correct = $value(correct)"
    }

    if {![my multiple]} {
      regexp {[.]([^.]+)$} $correct_field_name _ correct_field_value
      lappend fc "radio:text,answer=$correct_field_value"
    }
    append form "</tbody></table></FORM>\n"
    [my object] set_property -new 1 form $form
    [my object] set_property -new 1 form_constraints $fc
  }

  ###########################################################
  #
  # ::xowiki::formfield::mc_choice
  #
  ###########################################################

  Class mc_choice -superclass FormGeneratorField -parameter {
    {feedback_level full}
    {inplace true}
    {multiple true}
  }

  mc_choice instproc initialize {} {
    if {[my set __state] ne "after_specs"} return

    if {1} {
      test_item instvar {xinha(javascript) javascript}
      set text_config [subst {editor=xinha,height=100px,label=Text,plugins=OacsFs,inplace=[my inplace],javascript=$javascript}]
    } else {
      set text_config [subst {editor=wym,height=100px,label=Text}]
    }
    if {[my feedback_level] eq "full"} {
      set feedback_fields {
	{feedback_correct {textarea,cols=60,label=#xowf.feedback_correct#}}
	{feedback_incorrect {textarea,cols=60,label=#xowf.feedback_incorrect#}}
      }
    } else {
      set feedback_fields ""
    }
    if {[my multiple]} {
      # We are in a multiple choice item; provide for editing a radio
      # group per alternative.
      my create_components [subst {
        {text  {richtext,$text_config}}
        {correct {boolean,horizontal=true,label=#xowf.correct#}}
        $feedback_fields
      }]
    } else {
      # We are in a single choice item; provide for editing a single
      # radio group spanning all entries.  Use as name for grouping
      # the form-field name minus the last segment.
      regsub -all {[.][^.]+$} [my name] "" groupname
      my create_components [subst {
        {text  {richtext,$text_config}}
        {correct {radio,label=#xowf.correct#,forced_name=$groupname.correct,options={"" [my name]}}}
        $feedback_fields
      }]
    }
    my set __initialized 1
  }
}

namespace eval ::xowiki::formfield {
  ###########################################################
  #
  # ::xowiki::formfield::text_interaction
  #
  ###########################################################

  Class text_interaction -superclass FormGeneratorField -parameter {
    {feedback_level full}
    {inplace true}
  }
  text_interaction set auto_correct false

  text_interaction instproc initialize {} {
    if {[my set __state] ne "after_specs"} return
    test_item instvar {xinha(javascript) javascript}
    my instvar feedback_level inplace input_field_names
    #
    # create component structure
    #
    my create_components  [subst {
      {text  {richtext,required,editor=xinha,height=150px,label=#xowf.exercise-text#,plugins=OacsFs,javascript=$javascript,inplace=$inplace}}
      {lines {numeric,default=10,size=3,label=#xowf.lines#}}
      {columns {numeric,default=60,size=3,label=#xowf.columns#}}
    }]
    my set __initialized 1
  }

  text_interaction instproc convert_to_internal {} {
    set form "<FORM>\n"
    set fc "@categories:off @cr_fields:hidden\n"
    set intro_text [my get_named_sub_component_value text]
    set lines      [my get_named_sub_component_value lines]
    set columns    [my get_named_sub_component_value columns]
    append form "<div class='question_text'>$intro_text</div>\n"
    append form "<textarea name='answer' rows='$lines' cols='$columns'></textarea>\n" 
    append fc "answer:textarea"
    append form "</FORM>\n"
    [my object] set_property -new 1 form $form
    [my object] set_property -new 1 form_constraints $fc
  }
}