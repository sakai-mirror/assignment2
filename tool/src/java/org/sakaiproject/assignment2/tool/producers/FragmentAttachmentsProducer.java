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
    
	private MessageLocator messageLocator;
	public void setMessageLocator(MessageLocator messageLocator) {
		this.messageLocator = messageLocator;
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
    	
    	//Now remove ones from session
    	if (session.getAttribute("removedAttachmentRefs") != null){
    		set.removeAll((Set<String>)session.getAttribute("removedAttachmentRefs"));
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
	    		String file_size = getReadableFileSize(cr.getContentLength());
	    		UIOutput.make(row, "attachment_size", file_size);
	    		
	    		//Add remove link
	    		if (params.remove) {
	    			UIVerbatim.make(row, "attachment_remove", 
	    					"<a href=\"#\" " +
	    					"onclick=\"" +
	    					"$.get('ajax-callback?removeAttachment=true&refId=" + org.sakaiproject.util.Web.escapeUrl(ref) + "');" +
	    					"$(this).parent('span').parent('li').remove();" +
	    					"\">" +
	    					messageLocator.getMessage("assignment2.remove") +
	    					"</a>");
	    		}
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
	
	private String getReadableFileSize(int sizeVal){
		double retVal = sizeVal;
		String sizeSuffix = "bytes";
		int GB = 1024 * 1024 * 1024;
		int MB = 1024 * 1024;
		int KB = 1024;
		if (sizeVal > GB) {
		retVal = sizeVal / GB;
		sizeSuffix = "GB";
		}
		else if(sizeVal > MB) {
		retVal = sizeVal / MB;
		sizeSuffix = "MB";
		}
		else if (sizeVal > KB) {
		retVal = sizeVal / KB;
		sizeSuffix = "KB";
		}
		String finalVal = "(".concat(Double.toString(retVal).concat(" " + sizeSuffix.concat(")")));
		return finalVal;
	}

}