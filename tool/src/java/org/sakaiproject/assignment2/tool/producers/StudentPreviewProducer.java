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
import uk.org.ponder.rsf.flow.ARIResult;
import uk.org.ponder.rsf.flow.ActionResultInterceptor;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCase;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCaseReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class StudentPreviewProducer implements ViewComponentProducer, NavigationCaseReporter, ViewParamsReporter, ActionResultInterceptor {

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
    
	public List reportNavigationCases() {
    	List<NavigationCase> nav= new ArrayList<NavigationCase>();
    	nav.add(new NavigationCase("back_to_list", new SimpleViewParameters(
    		AssignmentListSortViewProducer.VIEW_ID)));
        nav.add(new NavigationCase("post", new SimpleViewParameters(
            AssignmentListSortViewProducer.VIEW_ID)));
        nav.add(new NavigationCase("failure", new AssignmentAddViewParams(
        	AssignmentAddProducer.VIEW_ID)));
        nav.add(new NavigationCase("preview", new SimpleAssignmentViewParams(
        	AssignmentPreviewProducer.VIEW_ID, null)));
        nav.add(new NavigationCase("save_draft", new SimpleViewParameters(
        	AssignmentListSortViewProducer.VIEW_ID)));
        nav.add(new NavigationCase("cancel", new SimpleViewParameters(
        	AssignmentListSortViewProducer.VIEW_ID)));
        return nav;
    }
	
	public void interceptActionResult(ARIResult result, ViewParameters incoming, Object actionReturn) {
		if (result.resultingView instanceof AssignmentAddViewParams) {
			AssignmentAddViewParams outgoing = (AssignmentAddViewParams) result.resultingView;
			outgoing.fromViewId = AssignmentPreviewProducer.VIEW_ID;
			outgoing.viewID = AssignmentAddProducer.VIEW_ID;
		}
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
}