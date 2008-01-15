package org.sakaiproject.assignment2.tool.producers.fragments;

import java.text.DateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.tool.beans.PreviewAssignmentSubmissionBean;
import org.sakaiproject.assignment2.tool.params.AssignmentAddViewParams;
import org.sakaiproject.assignment2.tool.producers.renderers.AttachmentListRenderer;
import org.sakaiproject.tool.api.SessionManager;
import org.sakaiproject.tool.api.ToolSession;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
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

public class FragmentSubmissionGradePreviewProducer implements ViewComponentProducer, ViewParamsReporter, ContentTypeReporter {

    public static final String VIEW_ID = "fragment-submission-grade_preview";
    public String getViewID() {
        return VIEW_ID;
    }


    private AssignmentLogic assignmentLogic;
    private ExternalLogic externalLogic;
	private PreviewAssignmentSubmissionBean previewAssignmentSubmissionBean;
	private Locale locale;
	private AttachmentListRenderer attachmentListRenderer;
	private SessionManager sessionManager;

    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	AssignmentAddViewParams params = (AssignmentAddViewParams) viewparams;
    	
    	//use a date which is related to the current users locale
        DateFormat df = DateFormat.getDateInstance(DateFormat.MEDIUM, locale);
    	
    	UIMessage.make(tofill, "page-title", "assignment2.assignment_preview.title");
        //navBarRenderer.makeNavBar(tofill, "navIntraTool:", VIEW_ID);

    	AssignmentSubmission as = previewAssignmentSubmissionBean.getAssignmentSubmission();
    	AssignmentSubmissionVersion asv = previewAssignmentSubmissionBean.getAssignmentSubmissionVersion();
    	Assignment2 assignment = as.getAssignment();
    	    	
    	
    	
        UIMessage.make(tofill, "preview_heading", "assignment2.fragment-submission-grade_preview.heading", new Object[]{ assignment.getTitle() });
        //Free from memory - if that does what I think it will do :-\
        previewAssignmentSubmissionBean.setAssignmentSubmission(null);
    	
        
        UIOutput.make(tofill, "student", externalLogic.getUserDisplayName(as.getUserId()));
        UIOutput.make(tofill, "status", as.getSubmissionStatus());
        UIVerbatim.make(tofill, "instructions", assignment.getInstructions());
        
        if (as.isAllowResubmit() != null && as.isAllowResubmit()){
        	UIMessage.make(tofill, "allow_resubmit", "assignment2.fragment-submission-grade_preview.number_resubmit", 
        		new Object[]{df.format(as.getResubmitCloseTime())});
        }
        
    	//Handle Attachments
    	Set<String> set = new HashSet();
    	if (assignment != null && assignment.getAttachmentSet() != null) {
	    	for (AssignmentAttachment aa : assignment.getAttachmentSet()) {
	    		set.add(aa.getAttachmentReference());
	    	}
    	}

    	//get New attachments from session set
    	ToolSession session = sessionManager.getCurrentToolSession();
    	if (session.getAttribute("attachmentRefs") != null) {
    		set.addAll((Set)session.getAttribute("attachmentRefs"));
    	}
    	
    	//Now remove ones from session
    	if (session.getAttribute("removedAttachmentRefs") != null){
    		set.removeAll((Set<String>)session.getAttribute("removedAttachmentRefs"));
    	}
    	
    	attachmentListRenderer.makeAttachment(tofill, "attachment_list:", params.viewID, set, Boolean.FALSE);
    	
    	UIVerbatim.make(tofill, "feedbackText", asv.getAnnotatedTextFormatted());
        UIVerbatim.make(tofill, "feedback_notes", asv.getFeedbackNotes());
    }
    
    public ViewParameters getViewParameters() {
        return new AssignmentAddViewParams();
    }
	
	public String getContentType() {
		return ContentTypeInfoRegistry.HTML_FRAGMENT;
	}

    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
    	this.assignmentLogic = assignmentLogic;
    }
    
    public void setExternalLogic(ExternalLogic externalLogic) {
    	this.externalLogic = externalLogic;
    }
        
    public void setPreviewAssignmentSubmissionBean(PreviewAssignmentSubmissionBean previewAssignmentSubmissionBean) {
    	this.previewAssignmentSubmissionBean = previewAssignmentSubmissionBean;
    }
    
    public void setLocale(Locale locale) {
    	this.locale = locale;
    }
    
	public void setAttachmentListRenderer(AttachmentListRenderer attachmentListRenderer){
		this.attachmentListRenderer = attachmentListRenderer;
	}
	
	public void setSessionManager(SessionManager sessionManager) {
		this.sessionManager = sessionManager;
	}
}