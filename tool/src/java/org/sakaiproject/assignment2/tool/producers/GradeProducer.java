/**********************************************************************************
 * $URL$
 * $Id$
 ***********************************************************************************
 *
 * Copyright (c) 2007, 2008 The Sakai Foundation.
 *
 * Licensed under the Educational Community License, Version 1.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.opensource.org/licenses/ecl1.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 **********************************************************************************/

package org.sakaiproject.assignment2.tool.producers;

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalContentReviewLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.GradeInformation;
import org.sakaiproject.assignment2.logic.GradebookItem;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.tool.beans.AssignmentSubmissionBean;
import org.sakaiproject.assignment2.tool.params.FilePickerHelperViewParams;
import org.sakaiproject.assignment2.tool.params.GradeViewParams;
import org.sakaiproject.assignment2.tool.params.ViewSubmissionsViewParams;
import org.sakaiproject.assignment2.tool.producers.evolvers.AttachmentInputEvolver;
import org.sakaiproject.assignment2.tool.producers.fragments.FragmentGradebookDetailsProducer;
import org.sakaiproject.assignment2.tool.producers.fragments.FragmentSubmissionGradePreviewProducer;
import org.sakaiproject.assignment2.tool.producers.renderers.AttachmentListRenderer;
import org.sakaiproject.assignment2.tool.producers.renderers.GradebookDetailsRenderer;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIBoundBoolean;
import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UICommand;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIELBinding;
import uk.org.ponder.rsf.components.UIForm;
import uk.org.ponder.rsf.components.UIInput;
import uk.org.ponder.rsf.components.UIInputMany;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UISelect;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.components.decorators.UIAlternativeTextDecorator;
import uk.org.ponder.rsf.components.decorators.UIFreeAttributeDecorator;
import uk.org.ponder.rsf.components.decorators.UITooltipDecorator;
import uk.org.ponder.rsf.evolvers.FormatAwareDateInputEvolver;
import uk.org.ponder.rsf.evolvers.TextInputEvolver;
import uk.org.ponder.rsf.flow.ARIResult;
import uk.org.ponder.rsf.flow.ActionResultInterceptor;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCase;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCaseReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

/**
 * This view is for grading a submission from a student. Typically you get to it
 * by going Assignment List -> Submissions -> Student Name.
 * 
 * @author rjlowe
 * @author wagnermr
 * @author sgithens
 *
 */
public class GradeProducer implements ViewComponentProducer, NavigationCaseReporter, ViewParamsReporter, ActionResultInterceptor {

    public static final String VIEW_ID = "grade";
    public String getViewID() {
        return VIEW_ID;
    }

    private TextInputEvolver richTextEvolver;
    private MessageLocator messageLocator;
    private AssignmentLogic assignmentLogic;
    private ExternalLogic externalLogic;
    private Locale locale;
    private AttachmentListRenderer attachmentListRenderer;
    private AssignmentSubmissionLogic submissionLogic;
    private EntityBeanLocator asvEntityBeanLocator;
    private AssignmentPermissionLogic permissionLogic;
    private AttachmentInputEvolver attachmentInputEvolver;
    private ExternalGradebookLogic gradebookLogic;
    private ExternalContentReviewLogic contentReviewLogic;
    
    // The images were named incorrectly, so the expand is actually the collapse.  Annoying, I know.
    private final String COLLAPSE_IMAGE_URL = "/sakai-assignment2-tool/content/images/expand.png";
    private final String EXPAND_IMAGE_URL = "/sakai-assignment2-tool/content/images/collapse.png";


    /*
     * You can change the date input to accept time as well by uncommenting the lines like this:
     * dateevolver.setStyle(FormatAwareDateInputEvolver.DATE_TIME_INPUT);
     * and commenting out lines like this:
     * dateevolver.setStyle(FormatAwareDateInputEvolver.DATE_INPUT);
     * -AZ
     * And vice versa - RWE
     */
    private FormatAwareDateInputEvolver dateEvolver;
    public void setDateEvolver(FormatAwareDateInputEvolver dateEvolver) {
        this.dateEvolver = dateEvolver;
    }

    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {

        //Get Params
        GradeViewParams params = (GradeViewParams) viewparams;
        String userId = params.userId;
        Long assignmentId = params.assignmentId;
        if (assignmentId == null || userId == null){
            //handle error
            return;
        }

        AssignmentSubmission as = submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(assignmentId, userId);
        Assignment2 assignment = assignmentLogic.getAssignmentByIdWithAssociatedData(assignmentId);

        //Grade Permission?
        boolean grade_perm = permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(userId, assignment);
        boolean gbItemExists = assignment.isGraded() && assignment.getGradebookItemId() != null && 
                gradebookLogic.gradebookItemExists(assignment.getGradebookItemId());
        
        // check to see if content review is enabled for this assignment
        boolean contentReviewEnabled = assignment.isContentReviewEnabled() && contentReviewLogic.isContentReviewAvailable(assignment.getContextId());

        // use a date which is related to the current users locale
        DateFormat df = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT, locale);

        // get the hover/alt text for the toggles
        String toggleHoverText = messageLocator.getMessage("assignment2.assignment_grade.toggle.hover");
        
        //Breadcrumbs
        UIInternalLink.make(tofill, "breadcrumb", 
                messageLocator.getMessage("assignment2.list.heading"),
                new SimpleViewParameters(ListProducer.VIEW_ID));
        UIInternalLink.make(tofill, "breadcrumb2",
                messageLocator.getMessage("assignment2.assignment_grade-assignment.heading", new Object[] { assignment.getTitle()}),
                new ViewSubmissionsViewParams(ViewSubmissionsProducer.VIEW_ID, assignment.getId()));
        UIMessage.make(tofill, "last_breadcrumb", "assignment2.assignment_grade.heading", 
                new Object[]{assignment.getTitle(), externalLogic.getUserDisplayName(params.userId)});

        //Heading messages
        UIMessage.make(tofill, "heading", "assignment2.assignment_grade.heading", 
                new Object[]{assignment.getTitle(), externalLogic.getUserDisplayName(params.userId)});
        UIMessage.make(tofill, "page-title", "assignment2.assignment_grade.title");
        //navBarRenderer.makeNavBar(tofill, "navIntraTool:", VIEW_ID);
        //UIMessage.make(tofill, "heading", "assignment2.assignment_grade.heading", new Object[]{assignment.getTitle()});

        // if gbItem is still null at this point, it must no longer exist. display warning
        // to user
        if (assignment.isGraded() && !gbItemExists) {
            UIOutput.make(tofill, "no_gb_item", messageLocator.getMessage("assignment2.assignment_grade.gb_item_deleted"));
        }

        //AssignmentSubmission OTP Stuff
        String asOTP = "AssignmentSubmission.";
        String OTPKey = "";
        if (as != null && as.getId() != null){
            OTPKey += as.getId();
        } else {
            OTPKey += EntityBeanLocator.NEW_PREFIX + "1";
        }
        asOTP += OTPKey;

        //AssignmentSubmissionVersion OTP Stuff
        String asvOTP = "AssignmentSubmissionVersion.";

        //Initialize js otpkey
        UIVerbatim.make(tofill, "attachment-ajax-init", /*"otpkey=\"" + org.sakaiproject.util.Web.escapeUrl(asvOTPKey) + "\";\n" +*/
                "userId=\"" + userId + "\";\n" +
                "assignmentId=\"" + assignmentId + "\";\n" +
                "fragGBDetailsPath=\"" + externalLogic.getAssignmentViewUrl(FragmentGradebookDetailsProducer.VIEW_ID) + "\";");


        /**
         * Begin the Form
         */
        UIForm form = UIForm.make(tofill, "form");

        // if this assignment requires non-electronic submission, there is no submission status
        if (assignment.getSubmissionType() == AssignmentConstants.SUBMIT_NON_ELECTRONIC) {
            UIMessage.make(form, "non-electronic-submission", "assignment2.assignment_grade.nonelectronic_sub");
        } 

        // Instructions
        UIOutput instructionsToggleSection = UIOutput.make(tofill, "instructions_toggle_section");
        instructionsToggleSection.decorate(new UITooltipDecorator(toggleHoverText));
        if (assignment.getInstructions() == null || assignment.getInstructions().equals("")) {
            UIMessage.make(tofill, "instructions", "assignment2.assignment_grade.no_instructions");
        }
        else {
            UIVerbatim.make(tofill, "instructions", assignment.getInstructions());
        }
        
        if (assignment.getAttachmentSet() != null && !assignment.getAttachmentSet().isEmpty()) {
            UIOutput.make(tofill, "assignAttachmentsFieldset");
            attachmentListRenderer.makeAttachmentFromAssignmentAttachmentSet(tofill, "assign_attach_list:", params.viewID, 
                    assignment.getAttachmentSet());
        }

        // add the alt text to the toggle for the instructions
        UIOutput instructionsToggle = UIOutput.make(tofill, "instructions_toggle");
        instructionsToggle.decorate(new UIAlternativeTextDecorator(toggleHoverText));

        
        /**
         * Provide feedback section
         */
        
        // iterate through all of the submissions
        List<AssignmentSubmissionVersion> versionHistory = submissionLogic.getVersionHistoryForSubmission(as);

        // set the status at the top of the page
        if (assignment.getSubmissionType() != AssignmentConstants.SUBMIT_NON_ELECTRONIC) {
            AssignmentSubmissionVersion currVersion = submissionLogic.getCurrentVersionFromHistory(versionHistory);
            int statusConstant = submissionLogic.getSubmissionStatusConstantForCurrentVersion(currVersion, assignment.getDueDate());
            Date studentSaveDate = currVersion != null ? currVersion.getStudentSaveDate() : null;
            Date submittedDate = currVersion != null ? currVersion.getSubmittedDate() : null;
            String statusText = getVersionStatusText(statusConstant, studentSaveDate, submittedDate);
            UIOutput.make(form, "status", statusText);
        }
        
        // we need to decide whether we show the toggle-able submission history or just the single feedback section
        // Display single feedback section when:
        // * student has not made a submission (this includes when a student is currently working on a draft but there are no submitted versions) -or- 
        // * submission is non-electronic 
        //
        // Display toggle-able sections when:
        // * there is at least one submission
        
        int numSubmittedVersions = getNumSubmittedVersions(versionHistory);
        
        if (numSubmittedVersions == 0 || assignment.getSubmissionType() == AssignmentConstants.SUBMIT_NON_ELECTRONIC) {

            // instructors have the ability to provide feedback without there being a submission.
            // we need to determine if this is new feedback or if we should display existing
            // feedback
            AssignmentSubmissionVersion feedbackOnlyVersion = getFeedbackOnlyVersion(versionHistory);
            
            AssignmentSubmissionVersion versionToDisplay;
            String otpKey;
            if (feedbackOnlyVersion == null) {
                // this will be new feedback, so use the new otpKey and a new AssignmentSubmissionVersion object
                otpKey = asvOTP + EntityBeanLocator.NEW_PREFIX + "1"; 
                versionToDisplay = new AssignmentSubmissionVersion();
            } else {
                // display the existing feedback-only version
                otpKey = asvOTP += feedbackOnlyVersion.getId();
                versionToDisplay = feedbackOnlyVersion;
            }  
            
            UIBranchContainer versionContainer = UIBranchContainer.make(form, "feedback_section:");
            UIOutput.make(versionContainer, "versionFeedback");
            makeAdditionalFeedbackSection(versionContainer, otpKey, versionToDisplay, !grade_perm);
        
        } else {
            // we will display the toggle-able sections
            // figure out which version(s) should be expanded in the UI. the rest will be collapsed
            Long versionToExpand = getVersionToExpand(versionHistory);
            
            for (AssignmentSubmissionVersion version : versionHistory) {
                // do not include drafts in this display
                if (!version.isDraft()) {
                    String otpKey = asvOTP + version.getId();
                    UIBranchContainer versionContainer = UIBranchContainer.make(form, "feedback_section:");

                    // make the toggle for each version
                    boolean expand = versionToExpand != null && versionToExpand.equals(version.getId());
                    makeVersionToggle(versionContainer, version, assignment.getDueDate(), expand, toggleHoverText);

                    makeFeedbackOnSubmissionSection(versionContainer, otpKey, params, version, assignment, !grade_perm, contentReviewEnabled);
                    makeAdditionalFeedbackSection(versionContainer, otpKey, version, !grade_perm);
                }
            }
        }
        
        /**
         * Override assignment-level settings section
         */
        // Submission-Level resubmission settings - not available for non-electronic
        // assignments
        if (assignment.getSubmissionType() != AssignmentConstants.SUBMIT_NON_ELECTRONIC) {
            UIOutput.make(form, "resubmission_settings");

            UIOutput resubmitToggleSection = UIOutput.make(tofill, "resubmission_toggle_section");
            resubmitToggleSection.decorate(new UITooltipDecorator(toggleHoverText));
            
            UIOutput resubmitToggle = UIOutput.make(tofill, "resubmission_toggle");
            resubmitToggle.decorate(new UIAlternativeTextDecorator(toggleHoverText));

            int current_times_submitted_already = 0;
            if (as != null && as.getSubmissionHistorySet() != null) {
                current_times_submitted_already = submissionLogic.getNumSubmittedVersions(as.getUserId(), assignmentId);
            }

            boolean is_override = (as.getNumSubmissionsAllowed() != null);
            int numSubmissionsAllowed;
            Date resubmitUntil;
            boolean is_require_accept_until = false;
            boolean render_resubmission_date = false;
            boolean accept_until_label = false;
            Date dueDate;

            if (as.getNumSubmissionsAllowed() != null) {
                // this student already has an override, so use these settings
                numSubmissionsAllowed = as.getNumSubmissionsAllowed();
                resubmitUntil = as.getResubmitCloseDate();
            } else {
                // otherwise, populate the fields with the assignment-level due date, not the accept until ( ONC-2206 )
                numSubmissionsAllowed = assignment.getNumSubmissionsAllowed();
                resubmitUntil = assignment.getDueDate();
                if (resubmitUntil==null)
                {
                    // this means there is no due date, so try the accept until date
                    resubmitUntil = assignment.getAcceptUntilDate();
                }
            }

            dueDate = assignment.getDueDate();
            if (dueDate!=null)
            {
                render_resubmission_date = true;
            }
            else if (dueDate==null && assignment.getAcceptUntilDate()!=null)
            {
                dueDate = assignment.getAcceptUntilDate();
                render_resubmission_date = true;
                accept_until_label = true;
            }
            
            // if resubmit is still null, throw the current date and time in there
            // it will only show up if the user clicks the "Set accept until" checkbox
            if (resubmitUntil == null && dueDate!=null) {
                resubmitUntil = dueDate;
                is_require_accept_until = false;
            }
            else if (resubmitUntil == null && dueDate==null) 
            {
                resubmitUntil = new Date();
                is_require_accept_until = false;
            } else {
                is_require_accept_until = true;
            }

            as.setNumSubmissionsAllowed(numSubmissionsAllowed);
            as.setResubmitCloseDate(resubmitUntil);

            if (grade_perm) {
                UIBoundBoolean.make(form, "override_settings", "#{AssignmentSubmissionBean.overrideResubmissionSettings}", is_override);

                UIOutput.make(form, "resubmit_change");

                int size = 20;
                String[] number_submissions_options = new String[size+1];
                String[] number_submissions_values = new String[size+1];
                number_submissions_values[0] = "" + AssignmentConstants.UNLIMITED_SUBMISSION;
                number_submissions_options[0] = messageLocator.getMessage("assignment2.indefinite_resubmit");
                for (int i=0; i < size; i++){
                    number_submissions_values[i + 1] = (i + current_times_submitted_already) + "";
                    number_submissions_options[i + 1] = i + "";
                }

                //Output
                String currSubmissionMsg = "assignment2.assignment_grade.resubmission_curr_submissions";
                if (current_times_submitted_already == 1) {
                    currSubmissionMsg = "assignment2.assignment_grade.resubmission_curr_submissions_1";
                }

                UIMessage.make(form, "resubmission_curr_submissions", currSubmissionMsg, 
                        new Object[] { current_times_submitted_already});

                UIVerbatim.make(form, "addtl_sub_label", messageLocator.getMessage("assignment2.assignment_grade.resubmission_allow_number"));

                UISelect.make(form, "resubmission_additional", number_submissions_values, number_submissions_options, 
                        asOTP + ".numSubmissionsAllowed", numSubmissionsAllowed + "");

                if (render_resubmission_date)
                {
                    UIOutput.make(tofill, "resubmission_dates");

                    DateFormat df2 = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT, locale);
                    if (accept_until_label)
                    {
                        UIMessage.make(tofill, "original_due_date", "assignment2.assignment_grade.accept_until_date", new Object[] {df2.format(dueDate)});
                    }
                    else
                    {
                        UIMessage.make(tofill, "original_due_date", "assignment2.assignment_grade.original_due_date", new Object[] {df2.format(dueDate)});
                    }

                    UIBoundBoolean require = UIBoundBoolean.make(form, "require_accept_until", "#{AssignmentSubmissionBean.resubmitUntil}", is_require_accept_until);
                    require.mustapply = true;

                    UIInput acceptUntilDateField = UIInput.make(form, "accept_until:", asOTP + ".resubmitCloseDate");
                    //set dateEvolver
                    dateEvolver.setStyle(FormatAwareDateInputEvolver.DATE_TIME_INPUT);
                    dateEvolver.setInvalidDateKey("assignment2.assignment_grade.resubmission.accept_until_date.invalid");
                    dateEvolver.setInvalidTimeKey("assignment2.assignment_grade.resubmission.accept_until_time.invalid");
                    dateEvolver.evolveDateInput(acceptUntilDateField, resubmitUntil);
                }
            } else {
                // display text-only representation
                UIOutput.make(form, "resubmit_no_change");

                //Output
                String currSubmissionMsg = "assignment2.assignment_grade.resubmission.curr_submissions.text-only";
                if (current_times_submitted_already == 1) {
                    currSubmissionMsg = "assignment2.assignment_grade.resubmission.curr_submissions.1.text-only";
                }

                UIMessage.make(form, "resub_curr_submissions_text", currSubmissionMsg, 
                        new Object[] { current_times_submitted_already});

                String numRemainingSubmissionsText;
                if (numSubmissionsAllowed == AssignmentConstants.UNLIMITED_SUBMISSION) {
                    numRemainingSubmissionsText = messageLocator.getMessage("assignment2.indefinite_resubmit");
                } else {
                    numRemainingSubmissionsText = (numSubmissionsAllowed - current_times_submitted_already) + "";
                }
                UIMessage.make(form, "resub_allowed_submissions_text", "assignment2.assignment_grade.resubmission.allow_number.text-only", 
                        new Object[] {numRemainingSubmissionsText});

                if (is_require_accept_until) {
                    UIMessage.make(form, "resub_until_text", "assignment2.assignment_grade.resubmission.accept_until.text-only", 
                            new Object[] {df.format(resubmitUntil)});
                }
            }
        }

        /**
         * Grading via the Gradebook Section
         */
        if (assignment.isGraded() && gbItemExists){
            makeGradebookGradingSection(tofill, form, assignment, as.getUserId(), !grade_perm);
        }        

        form.parameters.add(new UIELBinding("#{AssignmentSubmissionBean.assignmentId}", assignmentId));
        form.parameters.add(new UIELBinding("#{AssignmentSubmissionBean.userId}", userId));
        if (grade_perm){
            UICommand.make(form, "release_feedback", UIMessage.make("assignment2.assignment_grade.release_feedback"),
            "#{AssignmentSubmissionBean.processActionSaveAndReleaseFeedbackForSubmission}");
            UICommand.make(form, "submit", UIMessage.make("assignment2.assignment_grade.submit"), "#{AssignmentSubmissionBean.processActionGradeSubmit}");
            //UICommand.make(form, "preview", UIMessage.make("assignment2.assignment_grade.preview"), "#{AssignmentSubmissionBean.processActionGradePreview}");
            UICommand.make(form, "cancel", UIMessage.make("assignment2.assignment_grade.cancel"), "#{AssignmentSubmissionBean.processActionCancel}");
        }
    }
    
    /**
     * This is used to generate the section for providing feedback on an actual submission. This includes
     * the inline text editor and the submitted attachments
     * @param versionContainer
     * @param otpKey
     * @param params
     * @param version
     * @param assignment
     * @param readOnly true if the user does not have permission to add/modify this feedback
     * @param contentReviewEnabled
     */
    private void makeFeedbackOnSubmissionSection(UIBranchContainer versionContainer, String otpKey, GradeViewParams params,
            AssignmentSubmissionVersion version, Assignment2 assignment, boolean readOnly, boolean contentReviewEnabled) {
        // Only display submission info if there has actually been a submission
        if (version.getSubmittedDate() != null) {
            // If assignment allows for submitted text
            if  (assignment.getSubmissionType() == AssignmentConstants.SUBMIT_INLINE_ONLY || 
                    assignment.getSubmissionType() == AssignmentConstants.SUBMIT_INLINE_AND_ATTACH) {
                UIOutput.make(versionContainer, "submitted_text_fieldset");

                if (readOnly){
                    UIVerbatim.make(versionContainer, "feedback_text:", version.getAnnotatedTextFormatted()); 
                } else {
                    UIInput feedback_text = UIInput.make(versionContainer, "feedback_text:", otpKey + ".annotatedText");
                    feedback_text.mustapply = Boolean.TRUE;
                    // SWG TODO Switching back to regular rich text edit until I get
                    // the FCK Editor working
                    richTextEvolver.evolveTextInput(feedback_text);
                    //assnCommentTextEvolver.evolveTextInput(feedback_text);
                    
                    UIOutput.make(versionContainer, "inline_feedback_instructions", messageLocator.getMessage("assignment2.assignment_grade.feedback_instructions"));
                }
            }

            //If assignment allows for submitted attachments, display the attachment section
            if (assignment.getSubmissionType() == AssignmentConstants.SUBMIT_ATTACH_ONLY ||
                    assignment.getSubmissionType() == AssignmentConstants.SUBMIT_INLINE_AND_ATTACH) {
                UIOutput.make(versionContainer, "submitted_attachments_fieldset");

                if (contentReviewEnabled && version.getSubmissionAttachSet() != null && !version.getSubmissionAttachSet().isEmpty()) {
                    contentReviewLogic.populateReviewProperties(assignment, version.getSubmissionAttachSet(), true);
                }

                attachmentListRenderer.makeAttachmentFromSubmissionAttachmentSet(versionContainer, "submitted_attachment_list:", params.viewID, 
                        version.getSubmissionAttachSet());
            }
        }
    }
    
    /**
     * This is used to render the "additional" feedback not directly related to a piece of the
     * submission. This includes the feedback notes and feedback attachments.
     * @param versionContainer
     * @param otpKey
     * @param version
     * @param readOnly true if the user does not have permission to add/modify this feedback
     */
    private void makeAdditionalFeedbackSection(UIBranchContainer versionContainer, String otpKey, AssignmentSubmissionVersion version, boolean readOnly) {
        UIOutput.make(versionContainer, "attachmentsFieldset");

        if (readOnly) {
            // Feedback Notes
            if (version.getFeedbackNotes() == null || 
                    "".equals(version.getFeedbackNotes().trim())) {
                UIMessage.make(versionContainer, "feedback_notes_no_edit", "assignment2.assignment_grade.no_feedback.text-only");
            } else {
                UIVerbatim.make(versionContainer, "feedback_notes_no_edit", version.getFeedbackNotes());   
            }
            // Feedback Attachments
            UIOutput.make(versionContainer, "feedback_attach_read_only");
            attachmentListRenderer.makeAttachmentFromFeedbackAttachmentSet(versionContainer, 
                    "feedback_attachment_list:", null, 
                    version.getFeedbackAttachSet());
        } else {
            // Feedback Notes
            UIInput feedback_notes = UIInput.make(versionContainer, "feedback_notes:", otpKey + ".feedbackNotes");
            feedback_notes.mustapply = Boolean.TRUE;
            richTextEvolver.evolveTextInput(feedback_notes);
            
            // Feedback Attachments
            UIOutput.make(versionContainer, "add_attach");
            String elementId = version.getId() != null ? version.getId() + "" : null;
            UIInternalLink addAttachLink = UIInternalLink.make(versionContainer, "add_attachments:", UIMessage.make("assignment2.assignment_grade.add_feedback_attach"),
                    new FilePickerHelperViewParams(AddAttachmentHelperProducer.VIEWID, Boolean.TRUE, 
                            Boolean.TRUE, 500, 700, otpKey));

            addAttachLink.decorate(new UIFreeAttributeDecorator("onclick", attachmentInputEvolver.getOnclickMarkupForAddAttachmentEvent(elementId)));

            UIInputMany attachmentInput = UIInputMany.make(versionContainer, "attachment_list:", otpKey + ".feedbackAttachmentRefs", 
                    version.getFeedbackAttachmentRefs());
            
            attachmentInputEvolver.evolveAttachment(attachmentInput, elementId);

            UIOutput noAttach = UIOutput.make(versionContainer, "no_attachments_yet", messageLocator.getMessage("assignment2.assignment_grade.no_feedback_attach"));
            if (version.getFeedbackAttachSet() != null && !version.getFeedbackAttachSet().isEmpty()) {
                noAttach.decorate(new UIFreeAttributeDecorator("style", "display:none;"));
            }
        }
    }
    
    /**
     * creates the toggle for displaying the submission history
     * @param versionContainer
     * @param version
     * @param assignDueDate
     * @param expand
     */
    private void makeVersionToggle(UIBranchContainer versionContainer, AssignmentSubmissionVersion version, Date assignDueDate, boolean expand, String toggleHoverText) {
        UIOutput toggle = UIOutput.make(versionContainer, "version_toggle");
        toggle.decorate(new UITooltipDecorator(toggleHoverText));
        
        // make the image
        UIOutput img = UIOutput.make(versionContainer, "version_toggle_img");
        String img_location = expand ? EXPAND_IMAGE_URL : COLLAPSE_IMAGE_URL;
        img.decorate(new UIFreeAttributeDecorator("src", img_location));
        img.decorate(new UIAlternativeTextDecorator(toggleHoverText));
        
        // figure out the status so we can determine what the heading should be
        int status = submissionLogic.getSubmissionStatusConstantForCurrentVersion(version, assignDueDate);
        String headerText;
        if (version.getSubmittedVersionNumber() == AssignmentSubmissionVersion.FEEDBACK_ONLY_VERSION_NUMBER) {
            headerText = messageLocator.getMessage("assignment2.assignment_grade.feedback_only_version");
        } else {
            headerText = getVersionStatusText(status, version.getStudentSaveDate(), version.getSubmittedDate());
        }
        
        UIOutput.make(versionContainer, "version_toggle_heading", headerText);
        
        UIOutput feedbackDetails = UIOutput.make(versionContainer, "versionFeedback");
        if (!expand) {
            feedbackDetails.decorate(new UIFreeAttributeDecorator("style", "display:none;"));
        }
    }
    
    private void makeGradebookGradingSection(UIContainer tofill, UIForm form, Assignment2 assignment, String studentId, boolean readOnly) {
        if (assignment == null || assignment.getGradebookItemId() == null) {
            return;
        }
        
        UIOutput.make(tofill, "gradebook-details-container");
        
        int gradeEntryType = gradebookLogic.getGradebookGradeEntryType(assignment.getContextId());
        String inputLabel;
        String inputAppender;
        if (gradeEntryType == ExternalGradebookLogic.ENTRY_BY_POINTS) {
            inputLabel = messageLocator.getMessage("assignment2.gradebook.grading.points");
            // get the points possible
            GradebookItem gbItem = gradebookLogic.getGradebookItemById(assignment.getContextId(), assignment.getGradebookItemId());
            inputAppender = messageLocator.getMessage("assignment2.gradebook.grading.points.appender", gbItem.getPointsPossible());
        } else if (gradeEntryType == ExternalGradebookLogic.ENTRY_BY_PERCENT) {
            inputLabel = messageLocator.getMessage("assignment2.gradebook.grading.percent");
            inputAppender = messageLocator.getMessage("assignment2.gradebook.grading.percent.appender");
        } else if (gradeEntryType == ExternalGradebookLogic.ENTRY_BY_LETTER) {
            inputLabel = messageLocator.getMessage("assignment2.gradebook.grading.letter");
            inputAppender = null;
        } else {
            inputLabel = messageLocator.getMessage("assignment2.gradebook.grading.unknown");
            inputAppender = null;
        }
        
        // get the grade information for this student from the gradebook
        String grade = "";
        String gradeComment = "";
        GradeInformation gradeInfo = gradebookLogic.getGradeInformationForStudent(assignment.getContextId(), assignment.getGradebookItemId(), studentId);
        if (gradeInfo != null) {
            grade = gradeInfo.getGradebookGrade();
            gradeComment = gradeInfo.getGradebookComment();
        }
        
        UIOutput.make(tofill, "grade_input_label", inputLabel);
        UIOutput.make(tofill, "grade_input_appender", inputAppender);
        UIInput gradeInput = UIInput.make(form, "grade_input", "#{AssignmentSubmissionBean.grade}", grade);
        if (readOnly) {
            gradeInput.decorate(new UIFreeAttributeDecorator("disabled", "disabled"));
        }

        UIInput commentInput = UIInput.make(form, "grade_comment_input", "#{AssignmentSubmissionBean.gradeComment}", gradeComment);
        if (readOnly) {
            commentInput.decorate(new UIFreeAttributeDecorator("disabled", "disabled"));
        }
    }
    
    /**
     * 
     * @param versionHistory
     * @return given the version history, returns the id of the version that should be expanded
     * in the history list
     */
    private Long getVersionToExpand(List<AssignmentSubmissionVersion> versionHistory) {
        Long versionToExpand = null;
        if (versionHistory != null && !versionHistory.isEmpty()) {
            if (versionHistory.size() == 1) {
                versionToExpand = versionHistory.get(0).getId();
            } else {
                // the versions are in submitted order, so let's grab the most recent submitted one. if it's a 
                // draft, let's expand the one right before it. we don't include drafts in this display
                AssignmentSubmissionVersion ver = versionHistory.get(0);
                if (ver.isDraft()) {
                    versionToExpand = versionHistory.get(1).getId();
                } else {
                    versionToExpand = ver.getId();
                }
            }
        }
        
        return versionToExpand;
    }
    
    /**
     * 
     * @param versionHistory
     * @return given the version history, returns the total number of versions from the history
     * that were submitted
     */
    private int getNumSubmittedVersions(List<AssignmentSubmissionVersion> versionHistory) {
        int numSubmitted = 0;
        if (versionHistory != null) {
            for (AssignmentSubmissionVersion version : versionHistory) {
                if (version.getSubmittedDate() != null) {
                    numSubmitted++;
                }
            }
        }
        return numSubmitted;
    }
    
    /**
     * 
     * @param versionHistory
     * @return give the version history, returns the feedback-only version, if it exists. this
     * is identified by {@link AssignmentSubmissionVersion#getSubmittedVersionNumber()} equal to
     * {@link AssignmentSubmissionVersion#FEEDBACK_ONLY_VERSION_NUMBER}.  if no feedback-only
     * version exists, returns null
     * 
     */
    private AssignmentSubmissionVersion getFeedbackOnlyVersion(List<AssignmentSubmissionVersion> versionHistory) {
        AssignmentSubmissionVersion feedbackOnlyVersion = null;
        if (versionHistory != null) {
            for (AssignmentSubmissionVersion version : versionHistory) {
                if (version.getSubmittedVersionNumber() == AssignmentSubmissionVersion.FEEDBACK_ONLY_VERSION_NUMBER) {
                    feedbackOnlyVersion = version;
                    break;
                }
            }
        }
        
        return feedbackOnlyVersion;
    }
    
    /**
     * 
     * @param statusConstant
     * @param studentSaveDate
     * @param submittedDate
     * @return the internationalized text describing the given status
     */
    private String getVersionStatusText(int statusConstant, Date studentSaveDate, Date submittedDate) {
        DateFormat df = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT, locale);
        String statusText = messageLocator.getMessage("assignment2.assignment_grade.status.not_started");
        if (statusConstant == AssignmentConstants.SUBMISSION_NOT_STARTED) {
            statusText =  messageLocator.getMessage("assignment2.assignment_grade.status.not_started");
        } else if (statusConstant == AssignmentConstants.SUBMISSION_IN_PROGRESS) {
            statusText =  messageLocator.getMessage("assignment2.assignment_grade.status.draft", df.format(studentSaveDate));
        } else if (statusConstant == AssignmentConstants.SUBMISSION_SUBMITTED) {
            statusText =  messageLocator.getMessage("assignment2.assignment_grade.status.submitted", df.format(submittedDate));
        } else if (statusConstant == AssignmentConstants.SUBMISSION_LATE) {
            statusText =  messageLocator.getMessage("assignment2.assignment_grade.status.submitted.late", df.format(submittedDate));
        }
        
        return statusText;
    }

    public List<NavigationCase> reportNavigationCases() {
        List<NavigationCase> nav= new ArrayList<NavigationCase>();
        nav.add(new NavigationCase("release_all", new GradeViewParams(
                GradeProducer.VIEW_ID, null, null)));
        nav.add(new NavigationCase("submit", new ViewSubmissionsViewParams(
                ViewSubmissionsProducer.VIEW_ID)));
        nav.add(new NavigationCase("preview", new SimpleViewParameters(
                FragmentSubmissionGradePreviewProducer.VIEW_ID)));
        nav.add(new NavigationCase("cancel", new ViewSubmissionsViewParams(
                ViewSubmissionsProducer.VIEW_ID)));
        return nav;
    }

    public void interceptActionResult(ARIResult result, ViewParameters incoming, Object actionReturn) {
        if (actionReturn instanceof String 
                && actionReturn != null
                && ((String)actionReturn).startsWith(AssignmentSubmissionBean.SAVE_AND_EDIT_PREFIX)) {
            String editNextVersion = (String) actionReturn;
            editNextVersion = editNextVersion.substring(AssignmentSubmissionBean.SAVE_AND_EDIT_PREFIX.length());
            GradeViewParams nextParams = (GradeViewParams) incoming.copy();
            nextParams.versionId = Long.parseLong(editNextVersion);
            result.resultingView = nextParams;
        } else if (result.resultingView instanceof ViewSubmissionsViewParams) {
            ViewSubmissionsViewParams outgoing = (ViewSubmissionsViewParams) result.resultingView;
            GradeViewParams in = (GradeViewParams) incoming;
            outgoing.assignmentId = in.assignmentId;
            outgoing.pageIndex = in.viewSubPageIndex;
        } else if (result.resultingView instanceof GradeViewParams) {
            GradeViewParams outgoing = (GradeViewParams) result.resultingView;
            GradeViewParams in = (GradeViewParams) incoming;
            outgoing.assignmentId = in.assignmentId;
            outgoing.userId = in.userId;
            outgoing.versionId = in.versionId;
        }

    }

    public ViewParameters getViewParameters() {
        return new GradeViewParams();
    }

    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }

    public void setRichTextEvolver(TextInputEvolver richTextEvolver) {
        this.richTextEvolver = richTextEvolver;
    }

    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
        this.assignmentLogic = assignmentLogic;
    }

    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }

    public void setLocale(Locale locale) {
        this.locale = locale;
    }

    public void setAttachmentListRenderer(AttachmentListRenderer attachmentListRenderer){
        this.attachmentListRenderer = attachmentListRenderer;
    }

    public void setSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
        this.submissionLogic = submissionLogic;
    }

    public void setAsvEntityBeanLocator(EntityBeanLocator asvEntityBeanLocator) {
        this.asvEntityBeanLocator = asvEntityBeanLocator;
    }

    public void setPermissionLogic(AssignmentPermissionLogic permissionLogic) {
        this.permissionLogic = permissionLogic;
    }

    public void setAttachmentInputEvolver(AttachmentInputEvolver attachmentInputEvolver)
    {
        this.attachmentInputEvolver = attachmentInputEvolver;
    }
    
    public void setExternalGradebookLogic(ExternalGradebookLogic gradebookLogic) {
        this.gradebookLogic = gradebookLogic;
    }
    
    public void setExternalContentReviewLogic(ExternalContentReviewLogic contentReviewLogic) {
        this.contentReviewLogic = contentReviewLogic;
    }

}
