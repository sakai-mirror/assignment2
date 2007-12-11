package org.sakaiproject.assignment2.tool.producers;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.tool.beans.PreviewAssignmentSubmissionBean;
import org.sakaiproject.assignment2.tool.params.AssignmentAddViewParams;
import org.sakaiproject.assignment2.tool.params.SimpleAssignmentViewParams;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.content.ContentTypeReporter;
import uk.org.ponder.rsf.content.ContentTypeInfoRegistry;
import uk.org.ponder.rsf.flow.ARIResult;
import uk.org.ponder.rsf.flow.ActionResultInterceptor;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCase;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCaseReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class StudentPreviewProducer implements ViewComponentProducer, ViewParamsReporter, ContentTypeReporter {

    public static final String VIEW_ID = "student-preview";
    public String getViewID() {
        return VIEW_ID;
    }

    private AssignmentLogic assignmentLogic;
    private ExternalLogic externalLogic;
	private PreviewAssignmentSubmissionBean previewAssignmentSubmissionBean;
	private Locale locale;
	private AttachmentListRenderer attachmentListRenderer;

	
    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	AssignmentAddViewParams params = (AssignmentAddViewParams) viewparams;
    	
    	//Fill here
    	
    }
    
    public ViewParameters getViewParameters() {
        return new AssignmentAddViewParams();
    }
    
	public void setPreviewAssignmentSubmissionBean(PreviewAssignmentSubmissionBean previewAssignmentSubmissionBean) {
		this.previewAssignmentSubmissionBean = previewAssignmentSubmissionBean;
	}
	
	public void setLocale(Locale locale) {
		this.locale = locale;
	}
	
	public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
		this.assignmentLogic = assignmentLogic;
	}
	
	public void setExternalLogic(ExternalLogic externalLogic) {
		this.externalLogic = externalLogic;
	}
	
	public void setAttachmentListRenderer(AttachmentListRenderer attachmentListRenderer) {
		this.attachmentListRenderer = attachmentListRenderer;
	}
	
	public String getContentType() {
		return ContentTypeInfoRegistry.HTML_FRAGMENT;
	}
}