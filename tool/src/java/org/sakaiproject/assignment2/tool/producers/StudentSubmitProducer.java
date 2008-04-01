
package org.sakaiproject.assignment2.tool.producers;

import java.util.ArrayList;
import java.util.List;

import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.assignment2.tool.params.SimpleAssignmentViewParams;
import org.sakaiproject.assignment2.tool.producers.renderers.StudentViewAssignmentRenderer;
import org.sakaiproject.assignment2.tool.producers.fragments.FragmentSubmissionPreviewProducer;
import org.sakaiproject.assignment2.tool.producers.fragments.FragmentAttachmentsProducer;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.tool.api.SessionManager;
import org.sakaiproject.tool.api.ToolSession;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.flow.ARIResult;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCase;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCaseReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;
import uk.org.ponder.rsf.flow.ActionResultInterceptor;

public class StudentSubmitProducer implements ViewComponentProducer, NavigationCaseReporter, ViewParamsReporter, ActionResultInterceptor {
	public static final String VIEW_ID = "student-submit";
	public String getViewID() {
		return VIEW_ID;
	}
	
    String reqStar = "<span class=\"reqStar\">*</span>";

    private MessageLocator messageLocator;
    private ExternalLogic externalLogic;
    private AssignmentSubmissionLogic submissionLogic;
    private SessionManager sessionManager;
    private EntityBeanLocator assignment2BeanLocator;
    private EntityBeanLocator assignmentSubmissionBeanLocator;
    private StudentViewAssignmentRenderer studentViewAssignmentRenderer;

    @SuppressWarnings("unchecked")
	public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	SimpleAssignmentViewParams params = (SimpleAssignmentViewParams) viewparams;
    		
    	//Clear out session attachment information if everything successful
    	ToolSession session = sessionManager.getCurrentToolSession();
    	
    	if (params.clearSession) {
	    	session.removeAttribute("attachmentRefs");
	    	session.removeAttribute("removedAttachmentRefs");
    	}
    	
    	//get Passed assignmentId to pull in for editing if any
    	Long assignmentId = params.assignmentId;
    	if (assignmentId == null){
    		//error 
    		return;
    	}
    	Assignment2 assignment = (Assignment2) assignment2BeanLocator.locateBean(assignmentId.toString());
    	if (assignment == null) {
    		//error 
    		return;
    	}
    	
    	AssignmentSubmission submission = submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(assignmentId, externalLogic.getCurrentUserId());
    	
    	String ASOTPKey = "";
    	if (submission == null || submission.getId() == null) {
    		ASOTPKey += EntityBeanLocator.NEW_PREFIX + "1";
    	} else {
    		ASOTPKey += submission.getId();
    	}
    	
    	//Now do submission stuff
    	AssignmentSubmission assignmentSubmission = (AssignmentSubmission) assignmentSubmissionBeanLocator.locateBean(ASOTPKey); 
    	    	
        // set the textual representation of the status
    	if (assignmentSubmission != null && submission.getSubmissionStatusConstant() != null) {
    		assignmentSubmission.setSubmissionStatus(messageLocator.getMessage(
    				"assignment2.student-submit.status." + 
    				submission.getSubmissionStatusConstant()));
    	}
    	
    	studentViewAssignmentRenderer.makeStudentView(tofill, "portletBody:", assignmentSubmission, assignment, params, ASOTPKey, Boolean.FALSE); 
        
    	//Initialize js otpkey
    	UIVerbatim.make(tofill, "attachment-ajax-init", "otpkey=\"" + org.sakaiproject.util.Web.escapeUrl(ASOTPKey) + "\";\n" +
    			"fragAttachPath=\"" + externalLogic.getAssignmentViewUrl(FragmentAttachmentsProducer.VIEW_ID) + "\";\n");
   	
    }
	
	public List<NavigationCase> reportNavigationCases() {
    	List<NavigationCase> nav= new ArrayList<NavigationCase>();
        nav.add(new NavigationCase("submit", new SimpleViewParameters(
            StudentAssignmentListProducer.VIEW_ID)));
        nav.add(new NavigationCase("preview", new SimpleViewParameters(
        	FragmentSubmissionPreviewProducer.VIEW_ID)));
        nav.add(new NavigationCase("save_draft", new SimpleViewParameters(
        	StudentAssignmentListProducer.VIEW_ID)));
        nav.add(new NavigationCase("cancel", new SimpleViewParameters(
        	StudentAssignmentListProducer.VIEW_ID)));
        return nav;
    }
	
	public void interceptActionResult(ARIResult result, ViewParameters incoming, Object actionReturn){
		//If the form fails, then make sure to add a view param not to clear the session
		if (actionReturn.equals("failure")) {
			SimpleAssignmentViewParams params = (SimpleAssignmentViewParams) result.resultingView;
			params.clearSession = false;
		}
	}
	
    public ViewParameters getViewParameters() {
        return new SimpleAssignmentViewParams();
    }
    
    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }
    
    public void setExternalLogic(ExternalLogic externalLogic) {
    	this.externalLogic = externalLogic;
    }
    
	public void setSessionManager(SessionManager sessionManager) {
		this.sessionManager = sessionManager;
	}
	
	public void setAssignment2EntityBeanLocator(EntityBeanLocator entityBeanLocator) {
		this.assignment2BeanLocator = entityBeanLocator;
	}
	
	public void setAssignmentSubmissionBeanLocator(EntityBeanLocator entityBeanLocator) {
		this.assignmentSubmissionBeanLocator = entityBeanLocator;
	}
	
	public void setAssignmentSubmissionEntityBeanLocator(EntityBeanLocator entityBeanLocator) {
		this.assignmentSubmissionBeanLocator = entityBeanLocator;
	}

	public void setSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
		this.submissionLogic = submissionLogic;
	}

	public void setStudentViewAssignmentRenderer(
			StudentViewAssignmentRenderer studentViewAssignmentRenderer) {
		this.studentViewAssignmentRenderer = studentViewAssignmentRenderer;
	}

}
