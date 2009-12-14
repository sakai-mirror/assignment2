/**
 * This file includes javascript for the "View Student's Submission" screen where grading takes place
 */

var asnn2gradeview = asnn2gradeview || {};

asnn2gradeview.override_submission_settings = function() {
    override_checkbox = jQuery("input:checkbox[@name='page-replace\:\:override_settings']").get(0);

    if (override_checkbox) {
        if (override_checkbox.checked) {
            // change text back to normal
            jQuery("#override_settings_container").removeClass("inactive");

            // enable all of the form elements
            jQuery("select[@name='page-replace\:\:resubmission_additional-selection']").removeAttr("disabled");
            jQuery("input:checkbox[@name='page-replace\:\:require_accept_until']").removeAttr("disabled");
            // TODO - get the rsf date widget to work when these are disabled -it
            // is throwing syntax error upon submission
            //jQuery("input[@name='page-replace\:\:accept_until\:1\:date-field']").removeAttr("disabled");
            //jQuery("input[@name='page-replace\:\:accept_until\:1\:time-field']").removeAttr("disabled");
        } else {
            // gray out the text
            jQuery("#override_settings_container").addClass("inactive");

            // disable all form elements
            jQuery("select[@name='page-replace\:\:resubmission_additional-selection']").attr("disabled", "disabled");
            jQuery("input:checkbox[@name='page-replace\:\:require_accept_until']").attr("disabled", "disabled");
            // jQuery("input[@name='page-replace\:\:accept_until\:1\:date-field']").attr("disabled","disabled");
            //jQuery("input[@name='page-replace\:\:accept_until\:1\:time-field']").attr("disabled","disabled");
        }
    }
}

asnn2gradeview.set_accept_until_on_submission_level = function() {
    el = jQuery("input:checkbox[@name='page-replace\:\:require_accept_until']").get(0);
    if (el) {
        if (el.checked) {
            jQuery("#accept_until_container").show();
        } else {
            jQuery("#accept_until_container").hide();
        }
    }
}
