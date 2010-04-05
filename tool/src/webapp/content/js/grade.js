/**
 * This file includes javascript for the "View Student's Submission" screen where grading takes place
 */

var asnn2gradeview = asnn2gradeview || {};

/**
 * Handles the rendering of the "Override Assignment-Level Settings" options
 */
asnn2gradeview.override_submission_settings = function() {
    override_checkbox = jQuery("input:checkbox[name='page-replace\:\:override_settings']").get(0);

    if (override_checkbox) {
        if (override_checkbox.checked) {
            // change text back to normal
            jQuery("#override_settings_container").removeClass("inactive");

            // enable all of the form elements
            jQuery("select[name='page-replace\:\:resubmission_additional-selection']").removeAttr("disabled");
            jQuery("input:checkbox[name='page-replace\:\:require_accept_until']").removeAttr("disabled");
            
            // display the editable date/time entry
            jQuery('#resubmission_due_section').show();
            jQuery('#resubmission_due_read_only').hide();
        } else {
            // gray out the text
            jQuery("#override_settings_container").addClass("inactive");

            // disable all form elements
            jQuery("select[name='page-replace\:\:resubmission_additional-selection']").attr("disabled", "disabled");
            jQuery("input:checkbox[name='page-replace\:\:require_accept_until']").attr("disabled", "disabled");
            
            // if due date was extended, we replace the editable date inputs with read-only form. The date widget
            // gets cranky if we try to disable it and throws validation errors upon form submission
            var dueDateExtended = jQuery("input:checkbox[name='page-replace\:\:require_accept_until']");
            if (dueDateExtended.get(0) && dueDateExtended.get(0).checked) {
                jQuery('#resubmission_due_section').hide();
                jQuery('#resubmission_due_read_only').show();
                var readOnlyText = jQuery("#resubmission_due_read_only");
                dateText = jQuery("input[name='page-replace\:\:accept_until\:1\:date-field']");
                timeText = jQuery("input[name='page-replace\:\:accept_until\:1\:time-field']");
                readOnlyText.text(dateText.val() + " " + timeText.val());
            }
        }
    }
}

asnn2gradeview.set_accept_until_on_submission_level = function() {
    el = jQuery("input:checkbox[name='page-replace\:\:require_accept_until']").get(0);
    if (el) {
        if (el.checked) {
            jQuery("#accept_until_container").show();
        } else {
            jQuery("#accept_until_container").hide();
        }
    }
}
