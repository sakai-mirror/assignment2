package org.sakaiproject.assignment2.tool.producers.renderers;

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.components.decorators.DecoratorList;
import uk.org.ponder.rsf.components.decorators.UICSSDecorator;
import uk.org.ponder.rsf.components.decorators.UIFreeAttributeDecorator;
import uk.org.ponder.rsf.producers.BasicProducer;

/**
 * This renders the aggregated view of all the submissions from a student for
 * a particular assignment. It does this by using collapsable drop downs for 
 * each version. For each one of those it uses the 
 * {@link AsnnSubmissionVersionRenderer} to render that drop downs content.
 * 
 * This also needs to show some nifty notifications on the title bars for
 * each version if that version has any feedback that has not been read. Also,
 * if any of the aggregated versions have new feedback, an overall notification
 * should be present somewhere at the top of the widget.
 * 
 * Currently this renderer is marking the submissions as viewed, so only use it
 * for a student who is actually viewing a submission.
 * 
 * @author sgithens
 *
 */
public class AsnnSubmissionHistoryRenderer implements BasicProducer {

    // Dependency
    private AsnnSubmissionVersionRenderer asnnSubmissionVersionRenderer;
    public void setAsnnSubmissionVersionRenderer(AsnnSubmissionVersionRenderer asnnSubmissionVersionRenderer) {
        this.asnnSubmissionVersionRenderer = asnnSubmissionVersionRenderer;
    }

    // Dependency
    private AssignmentSubmissionLogic submissionLogic;
    public void setSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
        this.submissionLogic = submissionLogic;
    }

    // Dependency
    private Locale locale;
    public void setLocale(Locale locale) {
        this.locale = locale;
    }

    private MessageLocator messageLocator;
    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }

    public void fillComponents(UIContainer parent, String clientID, AssignmentSubmission assignmentSubmission) {       

        Assignment2 assignment = assignmentSubmission.getAssignment();
        List<AssignmentSubmissionVersion> versionHistory = submissionLogic.getVersionHistoryForSubmission(assignmentSubmission);

        DateFormat df = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT, locale);

        if (versionHistory.size() >= 1) {
            // Show the history view if:
            // 1) there is more than one version or 
            // 2) there is one submitted version and submission is still open
            // Do not display history if there is one version and that version is draft.
            // That info will be displayed in the editor/summary view

            // check to see if submission is still open. we only include draft
            // versions in this display if submission is closed and there is more than one version
            boolean submissionOpen = submissionLogic.isSubmissionOpenForStudentForAssignment(assignmentSubmission.getUserId(), assignment.getId());
            boolean includeDraftVersion = !submissionOpen && versionHistory.size() > 1;

            UIJointContainer joint = new UIJointContainer(parent, clientID, "asnn2-submission-history-widget:");
            UIOutput.make(joint, "submissions-header");
            UIOutput.make(joint, "multiple-submissions");

            // add alt text to the expand/collapse img for history section
            Map<String, String> histSectionAltText = new HashMap<String, String>();
            String histHoverText = messageLocator.getMessage("assignment2.student-submission.history.header.toggle");
            histSectionAltText.put("alt", histHoverText);
            histSectionAltText.put("title", histHoverText);
            DecoratorList altTextDecoratorList = new DecoratorList(new UIFreeAttributeDecorator(histSectionAltText));
            UIOutput histToggle = UIOutput.make(joint, "history_toggle");
            histToggle.decorators = altTextDecoratorList;

            // add alt text to the expand/collapse img for individual submissions
            Map<String, String> subAltText = new HashMap<String, String>();
            String subHoverText = messageLocator.getMessage("assignment2.student-submission.history.version.header.toggle");
            subAltText.put("alt", subHoverText);
            subAltText.put("title", subHoverText);
            DecoratorList subTextDecoratorList = new DecoratorList(new UIFreeAttributeDecorator(subAltText));
            UIOutput subToggle = UIOutput.make(joint, "submission_toggle");
            subToggle.decorators = subTextDecoratorList;

            for (AssignmentSubmissionVersion version: versionHistory) {
                // do not include draft versions in this history display unless
                // submission is closed and there were multiple submissions
                if (!version.isDraft() || includeDraftVersion) {
                    UIBranchContainer versionDiv = UIBranchContainer.make(joint, "submission-version:");
                    if (version.isDraft()) {
                        UIMessage.make(versionDiv, "header-text", "assignment2.student-submission.history.version.header.draft",
                                new Object[] {df.format(version.getStudentSaveDate())});
                    } else if (version.getSubmittedDate() == null) {
                        UIMessage.make(versionDiv, "header-text", "assignment2.student-submission.history.version.header.no_submission");
                    } else {
                        if (assignment.getDueDate() != null && assignment.getDueDate().before(version.getSubmittedDate())) {
                            UIMessage.make(versionDiv, "header-text", "assignment2.student-submission.history.version.header.late", 
                                    new Object[] {df.format(version.getSubmittedDate())});
                        } else {
                            UIMessage.make(versionDiv, "header-text", "assignment2.student-submission.history.version.header", 
                                    new Object[] {df.format(version.getSubmittedDate())});
                        }
                    }
                    boolean newfeedback = false;
                    String feedbackReadText = messageLocator.getMessage("assignment2.student-submission.feedback.read");
                    String feedbackUnreadText = messageLocator.getMessage("assignment2.student-submission.feedback.unread");

                    // Make the envelope icons for feedback if necessary
                    if (version.isFeedbackReleased() && version.isFeedbackRead()) {
                        UIOutput readFBImg = UIOutput.make(versionDiv, "open-feedback-img");
                        newfeedback = true;

                        // add alt text to this image
                        Map<String, String> readFBMap = new HashMap<String, String>();
                        readFBMap.put("alt", feedbackReadText);
                        readFBMap.put("title", feedbackReadText);
                        DecoratorList readFBDecoratorList = new DecoratorList(new UIFreeAttributeDecorator(readFBMap));
                        readFBImg.decorators = readFBDecoratorList;
                    }
                    else if (version.isFeedbackReleased()) {
                        UIOutput unreadFBImg = UIOutput.make(versionDiv, "new-feedback-img");

                        // add alt text to this image
                        Map<String, String> unreadFBMap = new HashMap<String, String>();
                        unreadFBMap.put("alt", feedbackUnreadText);
                        unreadFBMap.put("title", feedbackUnreadText);
                        DecoratorList unreadFBDecoratorList = new DecoratorList(new UIFreeAttributeDecorator(unreadFBMap));
                        unreadFBImg.decorators = unreadFBDecoratorList;
                    }

                    UIContainer versionContainer = asnnSubmissionVersionRenderer.fillComponents(versionDiv, "submission-entry:", version, true);
                    Map<String,String> stylemap = new HashMap<String,String>();
                    stylemap.put("display", "none");
                    versionContainer.decorate(new UICSSDecorator(stylemap));
                    UIVerbatim.make(versionDiv, "jsinit", "asnn2.assnSubVersionDiv('" +
                            versionDiv.getFullID()+"',"+newfeedback+
                            ",'"+assignmentSubmission.getId()+"','"+
                            version.getId()+"','"+feedbackReadText+"');");
                }
            }
            // add the <hr> tag separating this section - we only want it to appear if this 
            // section appears!
            UIOutput.make(parent, "previous_submissions_sep");
        }
    }

    public void fillComponents(UIContainer parent, String clientID) {

    }

}
