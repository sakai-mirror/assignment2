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

import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.components.decorators.UICSSDecorator;
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
            
            for (AssignmentSubmissionVersion version: versionHistory) {
                // do not include draft versions in this history display
                if (!version.isDraft() || includeDraftVersion) {
                    UIBranchContainer versionDiv = UIBranchContainer.make(joint, "submission-version:");
                    if (version.isDraft()) {
                        UIMessage.make(versionDiv, "header-text", "assignment2.student-submission.history.version.header.draft",
                                new Object[] {df.format(version.getStudentSaveDate())});
                    } else if (version.getSubmittedDate() == null) {
                        UIMessage.make(versionDiv, "header-text", "assignment2.student-submission.history.version.header.no_submission");
                    } else {
                        UIMessage.make(versionDiv, "header-text", "assignment2.student-submission.history.version.header", 
                                new Object[] {df.format(version.getSubmittedDate())});
                    }
                    boolean newfeedback = false;
                    // Make the envelope icons for feedback if necessary
                    if (version.isFeedbackReleased() && version.isFeedbackRead()) {
                        UIOutput.make(versionDiv, "open-feedback-img");
                        newfeedback = true;
                    }
                    else if (version.isFeedbackReleased()) {
                        UIOutput.make(versionDiv, "new-feedback-img");
                    }
                    UIContainer versionContainer = asnnSubmissionVersionRenderer.fillComponents(versionDiv, "submission-entry:", version, true);
                    Map<String,String> stylemap = new HashMap<String,String>();
                    stylemap.put("display", "none");
                    versionContainer.decorate(new UICSSDecorator(stylemap));
                    UIVerbatim.make(versionDiv, "jsinit", "asnn2.assnSubVersionDiv('" +
                            versionDiv.getFullID()+"',"+newfeedback+
                            ",'"+assignmentSubmission.getId()+"','"+
                            version.getId()+"');");
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
