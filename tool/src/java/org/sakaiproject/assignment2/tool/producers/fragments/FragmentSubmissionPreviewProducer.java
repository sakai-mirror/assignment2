package org.sakaiproject.assignment2.tool.producers.fragments;

import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.tool.beans.PreviewAssignmentSubmissionBean;
import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.assignment2.tool.producers.renderers.AttachmentListRenderer;
import org.sakaiproject.tool.api.SessionManager;
import org.sakaiproject.tool.api.ToolSession;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.content.ContentTypeReporter;
import uk.org.ponder.rsf.content.ContentTypeInfoRegistry;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class FragmentSubmissionPreviewProducer implements ViewComponentProducer, ViewParamsReporter, ContentTypeReporter {

    public static final String VIEW_ID = "fragment-submission_preview";
    public String getViewID() {
        return VIEW_ID;
    }

	private PreviewAssignmentSubmissionBean previewAssignmentSubmissionBean;
	private AttachmentListRenderer attachmentListRenderer;
	private SessionManager sessionManager;
		
    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	AssignmentViewParams params = (AssignmentViewParams) viewparams;
    	
    	//get the assignmentsubmission object
    	AssignmentSubmission as = previewAssignmentSubmissionBean.getAssignmentSubmission();
    	if (as == null || as.getCurrentSubmissionVersion() == null) {
    		return; 
    	}
    	AssignmentSubmissionVersion asv = previewAssignmentSubmissionBean.getAssignmentSubmissionVersion();
    	
    	UIVerbatim.make(tofill, "submittedText", asv.getSubmittedText());

    	//Handle ATtachments 
    	Set<String> set = new HashSet<String>();    		
		if (as != null) {
			//Next get the current assignment submission version
			AssignmentSubmissionVersion subasv = (AssignmentSubmissionVersion) as.getCurrentSubmissionVersion();
			//Now get the attachment set
			if (subasv != null && subasv.getSubmissionAttachSet() != null) {
				for (SubmissionAttachment asa : subasv.getSubmissionAttachSet()) {
					set.add(asa.getAttachmentReference());
				}
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
    	
    }
    
    public ViewParameters getViewParameters() {
        return new AssignmentViewParams();
    }
    
	public void setPreviewAssignmentSubmissionBean(PreviewAssignmentSubmissionBean previewAssignmentSubmissionBean) {
		this.previewAssignmentSubmissionBean = previewAssignmentSubmissionBean;
	}
	
	public void setAttachmentListRenderer(AttachmentListRenderer attachmentListRenderer) {
		this.attachmentListRenderer = attachmentListRenderer;
	}
	
	public String getContentType() {
		return ContentTypeInfoRegistry.HTML_FRAGMENT;
	}
	
	public void setSessionManager(SessionManager sessionManager) {
		this.sessionManager = sessionManager;
	}
}