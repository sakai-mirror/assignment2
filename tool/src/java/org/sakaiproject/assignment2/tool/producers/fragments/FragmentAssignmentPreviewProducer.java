package org.sakaiproject.assignment2.tool.producers.fragments;

import java.text.DateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.tool.beans.PreviewAssignmentBean;
import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.assignment2.tool.producers.renderers.StudentViewAssignmentRenderer;
import org.sakaiproject.assignment2.tool.producers.AssignmentListSortViewProducer;
import org.sakaiproject.tool.api.SessionManager;
import org.sakaiproject.tool.api.ToolSession;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.content.ContentTypeReporter;
import uk.org.ponder.rsf.content.ContentTypeInfoRegistry;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class FragmentAssignmentPreviewProducer implements ViewComponentProducer, ViewParamsReporter, ContentTypeReporter {

    public static final String VIEW_ID = "fragment-assignment_preview";
    public String getViewID() {
        return VIEW_ID;
    }


    private AssignmentLogic assignmentLogic;
	private PreviewAssignmentBean previewAssignmentBean;
	private StudentViewAssignmentRenderer studentViewAssignmentRenderer;
	private MessageLocator messageLocator;

    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	AssignmentViewParams params = (AssignmentViewParams) viewparams;

        Assignment2 assignment;
        String OTPKey = "";
        
    	//we are coming from the add/edit assignment page
    	assignment = previewAssignmentBean.getAssignment();
    	if (assignment.getAssignmentId() != null) {
    		OTPKey = assignment.getAssignmentId().toString();
    	} else {
    		OTPKey = EntityBeanLocator.NEW_PREFIX + "1";
    	}
        
        
        AssignmentSubmission assignmentSubmission = new AssignmentSubmission();
        // set the textual representation of the status
        assignmentSubmission.setSubmissionStatus(messageLocator.getMessage("assignment2.student-submit.status.0"));
        String ASOTPKey = EntityBeanLocator.NEW_PREFIX + "1";
        studentViewAssignmentRenderer.makeStudentView(tofill, "portletBody:", assignmentSubmission, assignment, params, ASOTPKey, Boolean.TRUE);

    }
    
    public ViewParameters getViewParameters() {
        return new AssignmentViewParams();
    }
	
	public String getContentType() {
		return ContentTypeInfoRegistry.HTML_FRAGMENT;
	}

    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
    	this.assignmentLogic = assignmentLogic;
    }
        
    public void setPreviewAssignmentBean(PreviewAssignmentBean previewAssignmentBean) {
    	this.previewAssignmentBean = previewAssignmentBean;
    }

	public void setStudentViewAssignmentRenderer(
			StudentViewAssignmentRenderer studentViewAssignmentRenderer) {
		this.studentViewAssignmentRenderer = studentViewAssignmentRenderer;
	}

	public void setMessageLocator(MessageLocator messageLocator) {
		this.messageLocator = messageLocator;
	}
}