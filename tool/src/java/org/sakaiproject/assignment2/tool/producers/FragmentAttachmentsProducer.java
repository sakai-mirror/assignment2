package org.sakaiproject.assignment2.tool.producers;

import java.util.Set;
import java.util.HashSet;

import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.tool.beans.PreviewAssignmentBean;
import org.sakaiproject.assignment2.tool.params.FragmentAttachmentsViewParams;
import org.sakaiproject.content.api.ContentHostingService;
import org.sakaiproject.content.api.ContentResource;
import org.sakaiproject.content.api.ContentTypeImageService;

import org.sakaiproject.entity.api.Reference;
import org.sakaiproject.tool.api.SessionManager;
import org.sakaiproject.tool.api.ToolSession;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UILink;
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
    
    private EntityBeanLocator entityBeanLocator;
	@SuppressWarnings("unchecked")
	public void setAssignment2EntityBeanLocator(EntityBeanLocator entityBeanLocator) {
		this.entityBeanLocator = entityBeanLocator;
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
	
    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	FragmentAttachmentsViewParams params = (FragmentAttachmentsViewParams) viewparams;
    	
    	if (params.otpkey == null || params.otpkey.equals("")){
    		return;
    	}
    	
    	Set<String> set = new HashSet();
    	
    	Assignment2 assignment = (Assignment2) entityBeanLocator.locateBean(params.otpkey);
    	if (assignment.getAttachmentSet() != null) {
	    	for (AssignmentAttachment aa : assignment.getAttachmentSet()) {
	    		set.add(aa.getAttachmentReference());
	    	}
    	}
    	
    	//get New attachments from session set
    	ToolSession session = sessionManager.getCurrentToolSession();
    	if (session.getAttribute("attachmentRefs") != null) {
    		set.addAll((Set)session.getAttribute("attachmentRefs"));
    	}
    	
    	for (String ref : set) {
    		//create a new <ol> to loop
    		UIBranchContainer row = UIBranchContainer.make(tofill, "attachments:");
    		//get the attachment
    		try {
	    		ContentResource cr = contentHostingService.getResource(ref);
	    		UILink.make(row, "attachment_image", externalLogic.getContentTypeImagePath(cr));
	    		UILink.make(row, "attachment_link", cr.getProperties().getProperty(cr.getProperties().getNamePropDisplayName()),
	    				cr.getUrl());
    		} catch (Exception e) {
    			//do nothing
    		}
    	}
    	
    }
    
	public ViewParameters getViewParameters() {
	    return new FragmentAttachmentsViewParams();
	}
	
	public String getContentType() {
		return ContentTypeInfoRegistry.HTML_FRAGMENT;
	}

}