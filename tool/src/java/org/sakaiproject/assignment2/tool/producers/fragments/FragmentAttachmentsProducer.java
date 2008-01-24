package org.sakaiproject.assignment2.tool.producers.fragments;

import java.util.Set;
import java.util.HashSet;

import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.AssignmentSubmissionAttachment;
import org.sakaiproject.assignment2.model.AssignmentFeedbackAttachment;
import org.sakaiproject.assignment2.tool.beans.PreviewAssignmentBean;
import org.sakaiproject.assignment2.tool.params.FragmentAttachmentsViewParams;
import org.sakaiproject.assignment2.tool.producers.renderers.AttachmentListRenderer;
import org.sakaiproject.content.api.ContentHostingService;
import org.sakaiproject.content.api.ContentResource;
import org.sakaiproject.content.api.ContentTypeImageService;

import org.sakaiproject.entity.api.Reference;
import org.sakaiproject.tool.api.SessionManager;
import org.sakaiproject.tool.api.ToolSession;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UILink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.content.ContentTypeReporter;
import uk.org.ponder.rsf.content.ContentTypeInfoRegistry;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class FragmentAttachmentsProducer implements ViewComponentProducer, ViewParamsReporter, ContentTypeReporter{
	
    public static final String VIEW_ID = "fragment-attachments";
    public String getViewID() {
        return VIEW_ID;
    }
    
    private EntityBeanLocator assignment2EntityBeanLocator;
	@SuppressWarnings("unchecked")
	public void setAssignment2EntityBeanLocator(EntityBeanLocator entityBeanLocator) {
		this.assignment2EntityBeanLocator = entityBeanLocator;
	}
	
	private EntityBeanLocator assignmentSubmissionEntityBeanLocator;
	public void setAssignmentSubmissionEntityBeanLocator(EntityBeanLocator entityBeanLocator) {
		this.assignmentSubmissionEntityBeanLocator = entityBeanLocator;
	}
	
	private EntityBeanLocator asvEntityBeanLocator;
	public void setAsvEntityBeanLocator(EntityBeanLocator asvEntityBeanLocator) {
		this.asvEntityBeanLocator = asvEntityBeanLocator;
	}
	
	private ContentHostingService contentHostingService;
	public void setContentHostingService(ContentHostingService contentHostingService) {
		this.contentHostingService = contentHostingService;
	}
	
	private SessionManager sessionManager;
	public void setSessionManager(SessionManager sessionManager) {
		this.sessionManager = sessionManager;
	}
	
	private ExternalLogic externalLogic;
	public void setExternalLogic(ExternalLogic externalLogic) {
		this.externalLogic = externalLogic;
	}
    
	private MessageLocator messageLocator;
	public void setMessageLocator(MessageLocator messageLocator) {
		this.messageLocator = messageLocator;
	}
    
	private AttachmentListRenderer attachmentListRenderer;
	public void setAttachmentListRenderer(AttachmentListRenderer attachmentListRenderer){
		this.attachmentListRenderer = attachmentListRenderer;
	}
	
    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	FragmentAttachmentsViewParams params = (FragmentAttachmentsViewParams) viewparams;
    	
    	if (params.otpkey == null || params.otpkey.equals("")){
    		return;
    	}
    	
    	Set<String> set = new HashSet();
    	
    	if (params.attachmentSetType == params.ASSIGNMENT_ATTACHMENT) {
    		//This means we are dealing with AssignmentAttachments
	    	Assignment2 assignment = (Assignment2) assignment2EntityBeanLocator.locateBean(params.otpkey);
	    	if (assignment != null && assignment.getAttachmentSet() != null) {
		    	for (AssignmentAttachment aa : assignment.getAttachmentSet()) {
		    		set.add(aa.getAttachmentReference());
		    	}
	    	}
    	} else if (params.attachmentSetType == params.ASSIGNMENT_SUBMISSION_ATTACHMENT) {
    		//This means we are dealing with AssignmentSubmissionAttachments
    		//First get the assignment submission
    		AssignmentSubmission as = (AssignmentSubmission) assignmentSubmissionEntityBeanLocator.locateBean(params.otpkey);
    		if (as != null) {
    			//Next get the current assignment submission version
    			AssignmentSubmissionVersion asv = (AssignmentSubmissionVersion) as.getCurrentSubmissionVersion();
    			//Now get the attachment set
    			if (asv != null && asv.getSubmissionAttachSet() != null) {
    				for (AssignmentSubmissionAttachment asa : asv.getSubmissionAttachSet()) {
    					set.add(asa.getAttachmentReference());
    				}
    			}
    		}
    	} else if (params.attachmentSetType == params.ASSIGNMENT_FEEDBACK_ATTACHMENT) {
    		AssignmentSubmissionVersion asv = (AssignmentSubmissionVersion) asvEntityBeanLocator.locateBean(params.otpkey);
    		if (asv != null && asv.getFeedbackAttachSet() != null){
    			for (AssignmentFeedbackAttachment afa : asv.getFeedbackAttachSet()){
    				set.add(afa.getAttachmentReference());
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
    	
    	attachmentListRenderer.makeAttachment(tofill, "attachment_list:", params.viewID, set, params.remove);
    	
    }
    
	public ViewParameters getViewParameters() {
	    return new FragmentAttachmentsViewParams();
	}
	
	public String getContentType() {
		return ContentTypeInfoRegistry.HTML_FRAGMENT;
	}
}