package org.sakaiproject.assignment2.tool.producers;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.tool.beans.UploadBean;
import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.assignment2.tool.params.ViewSubmissionsViewParams;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

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
