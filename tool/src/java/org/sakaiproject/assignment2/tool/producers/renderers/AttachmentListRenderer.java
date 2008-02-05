package org.sakaiproject.assignment2.tool.producers.renderers;

import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
import org.sakaiproject.assignment2.model.FeedbackAttachment;
import org.sakaiproject.assignment2.tool.producers.fragments.AjaxCallbackProducer;
import org.sakaiproject.content.api.ContentHostingService;
import org.sakaiproject.content.api.ContentResource;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UILink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UIVerbatim;

import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;

public class AttachmentListRenderer {
	
	private ContentHostingService contentHostingService;
	public void setContentHostingService(ContentHostingService contentHostingService) {
		this.contentHostingService = contentHostingService;
	}
	
	private ExternalLogic externalLogic;
	public void setExternalLogic(ExternalLogic externalLogic) {
		this.externalLogic = externalLogic;
	}
	
	private MessageLocator messageLocator;
	public void setMessageLocator(MessageLocator messageLocator) {
		this.messageLocator = messageLocator;
	}
	
	private EntityBeanLocator assignment2EntityBeanLocator;
	public void setAssignment2EntityBeanLocator(EntityBeanLocator assignment2EntityBeanLocator) {
		this.assignment2EntityBeanLocator = assignment2EntityBeanLocator;
	}
	
	public void makeAttachmentFromAssignmentAttachmentSet(UIContainer tofill, String divID, String currentViewID, Set<AssignmentAttachment> aaSet, Boolean remove) {
		Set<String> refSet = new HashSet();
		if (aaSet != null){
			for (AssignmentAttachment aa : aaSet) {
				refSet.add(aa.getAttachmentReference());
			}
		}
		makeAttachment(tofill, divID, currentViewID, refSet, remove);
	}
	
	public void makeAttachmentFromAssignment2OTPAttachmentSet(UIContainer tofill, String divID, String currentViewID, String a2OTPKey, Boolean remove) {
		Assignment2 assignment = (Assignment2)assignment2EntityBeanLocator.locateBean(a2OTPKey);
		Set<String> refSet = new HashSet();
		if (assignment != null && assignment.getAttachmentSet() != null){
			for (AssignmentAttachment aa : assignment.getAttachmentSet()) {
				refSet.add(aa.getAttachmentReference());
			}
		}
		makeAttachment(tofill, divID, currentViewID, refSet, remove);
	}
	
	public void makeAttachmentFromSubmissionAttachmentSet(UIContainer tofill, String divID, String currentViewID,
			Set<SubmissionAttachment> asaSet, Boolean remove) {
		Set<String> refSet = new HashSet();
		if (asaSet != null) {
			for (SubmissionAttachment asa : asaSet) {
				refSet.add(asa.getAttachmentReference());
			}
		}
		makeAttachment(tofill, divID, currentViewID, refSet, remove);
	}
	
	public void makeAttachmentFromFeedbackAttachmentSet(UIContainer tofill, String divID, String currentViewID,
			Set<FeedbackAttachment> afaSet, Boolean remove) {
		Set<String> refSet = new HashSet();
		if (afaSet != null) {
			for (FeedbackAttachment afa : afaSet) {
				refSet.add(afa.getAttachmentReference());
			}
		}
		makeAttachment(tofill, divID, currentViewID, refSet, remove);
	}

	
    public void makeAttachment(UIContainer tofill, String divID, String currentViewID, Set<String> refSet, Boolean remove) {
        
    	if (refSet == null || refSet.size() == 0){
        	UIJointContainer joint = new UIJointContainer(tofill, divID, "attachments:", ""+1);
        	UIMessage.make(joint, "no_attachments_yet", "assignment2.no_attachments_yet");
        	return;
        }

        int i = 1;
        for (String ref : refSet){
        	UIJointContainer joint = new UIJointContainer(tofill, divID, "attachments:", ""+i);
	        try {
	    		ContentResource cr = contentHostingService.getResource(ref);
	    		UILink.make(joint, "attachment_image", externalLogic.getContentTypeImagePath(cr));
	    		UILink.make(joint, "attachment_link", cr.getProperties().getProperty(cr.getProperties().getNamePropDisplayName()),
	    				cr.getUrl());
	    		String file_size = getReadableFileSize(cr.getContentLength());
	    		UIOutput.make(joint, "attachment_size", file_size);
	    		
	    		//Add remove link
	    		if (remove) {
	    			String url = externalLogic.getAssignmentViewUrl(AjaxCallbackProducer.VIEW_ID);
	    			UIVerbatim.make(joint, "attachment_remove", 
	    					"<a href=\"#\" " +
	    					"onclick=\"" +
	    					"$.get('" + url + "?removeAttachment=true&refId=" + org.sakaiproject.util.Web.escapeUrl(ref) + "',function(){" +
	    					"if(refresh_ajax){refresh_ajax();}});" + //"$(this).parent('span').parent('li').remove();" +
	    					"\">" +
	    					messageLocator.getMessage("assignment2.remove") +
	    					"</a>");
	    		}
			} catch (Exception e) {
				//do nothing
			}
			i++;
        } //Ending for loop
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
