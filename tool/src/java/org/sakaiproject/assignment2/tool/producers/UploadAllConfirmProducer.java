package org.sakaiproject.assignment2.tool.producers;

import java.util.List;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.tool.beans.UploadBean;
import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.assignment2.tool.params.ViewSubmissionsViewParams;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UICommand;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIForm;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

/**
 * This View renders the confirmation screen after you upload a CSV file to 
 * bulk update the grades. It has a table of the parsed data with Student ID, 
 * Name, Grade, and comments, as well as any errors that were found. 
 * 
 * You can click Ok to finish the process or Back to go back a screen and try 
 * uploading another file.
 * 
 * @author sgithens
 *
 */
public class UploadAllConfirmProducer implements ViewComponentProducer, ViewParamsReporter {
    public static final String VIEW_ID = "uploadall-confirm";

    // Dependency
    private MessageLocator messageLocator;
    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }

    // Dependency
    private AssignmentLogic assignmentLogic;
    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
        this.assignmentLogic = assignmentLogic;
    }

    // Property 
    private UploadBean uploadBean;
    public void setUploadBean(UploadBean uploadBean) {
        this.uploadBean = uploadBean;
    }
    public UploadBean getUploadBean() {
        return uploadBean;
    }

    public void fillComponents(UIContainer tofill, ViewParameters viewparams,
            ComponentChecker checker) {
        AssignmentViewParams params = (AssignmentViewParams) viewparams;

        Assignment2 assignment = assignmentLogic.getAssignmentById(params.assignmentId);

        if (uploadBean.parsedContent != null) {
            for (List<String> parts: uploadBean.parsedContent) {

                // make sure there are 4 parts to the content representing
                // username, student name, grade, grade comment.
                // it is possible to have grade and/or comment null and we
                // don't want to hit an IndexOutOfBoundsException during processing
                // later on
                if (parts.size() < 4) {
                    while (parts.size() < 4) {
                        parts.add("");
                    }
                }
                
                UIBranchContainer row = UIBranchContainer.make(tofill, "student-row:");
                
                UIOutput.make(row, "student-id", parts.get(0));
                UIOutput.make(row, "student-name", parts.get(1));
                UIOutput.make(row, "grade", parts.get(2));
                UIOutput.make(row, "comments", parts.get(3));
            }
        }

        makeBreadcrumbs(tofill, assignment);
        
        UIForm finishForm = UIForm.make(tofill, "finish-form");
        UICommand.make(finishForm, "ok-button", UIMessage.make("assignment2.upload_grades_confirm.button.ok"), "UploadBean.processUploadConfirmAndSave");
        UICommand.make(finishForm, "cancel-button", UIMessage.make("assignment2.upload_grades_confirm.button.back"), "UploadBean.processBackToUpload");

    }

    private void makeBreadcrumbs(UIContainer tofill, Assignment2 assignment) {
        // Make BreadCrumbs
        UIInternalLink.make(tofill, "breadcrumb_asnn_list", 
                messageLocator.getMessage("assignment2.list.heading"),
                new SimpleViewParameters(ListProducer.VIEW_ID));
        UIInternalLink.make(tofill, "breadcrumb_asnn_submissions", 
                messageLocator.getMessage("assignment2.upload_grades.breadcrumb.back_to_submissions", assignment.getTitle() )
                , new ViewSubmissionsViewParams(ViewSubmissionsProducer.VIEW_ID, assignment.getId()));
    }

    public String getViewID() {
        return VIEW_ID;
    }

    public ViewParameters getViewParameters()
    {
        return new AssignmentViewParams();
    }

}
