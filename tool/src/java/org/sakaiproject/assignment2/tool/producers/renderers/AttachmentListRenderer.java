/**********************************************************************************
 * $URL$
 * $Id$
 ***********************************************************************************
 *
 * Copyright (c) 2007, 2008 The Sakai Foundation.
 *
 * Licensed under the Educational Community License, Version 1.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.opensource.org/licenses/ecl1.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 **********************************************************************************/

package org.sakaiproject.assignment2.tool.producers.renderers;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.FeedbackAttachment;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
import org.sakaiproject.assignment2.tool.beans.AttachmentBean;
import org.sakaiproject.content.api.ContentHostingService;
import org.sakaiproject.content.api.ContentResource;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIInput;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UILink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.components.decorators.UICSSDecorator;

public class AttachmentListRenderer {
	private static final Log LOG = LogFactory.getLog(AttachmentListRenderer.class);
	
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
	
	private AttachmentBean attachmentBean;
	public void setAttachmentBean (AttachmentBean attachmentBean) {
		this.attachmentBean = attachmentBean;
	}
	
	
	public void makeAttachmentFromAssignmentAttachmentSet(UIContainer tofill, String divID, String currentViewID, Set<AssignmentAttachment> aaSet, Boolean remove) {
		Set<String> refSet = new HashSet();
		if (aaSet != null){
			for (AssignmentAttachment aa : aaSet) {
				refSet.add(aa.getAttachmentReference());
			}
		}
		makeAttachment(tofill, divID, currentViewID, refSet, remove, 0);
	}
	
	public void makeAttachmentFromAssignment2OTPAttachmentSet(UIContainer tofill, String divID, String currentViewID, String a2OTPKey, Boolean remove) {
		Assignment2 assignment = (Assignment2)assignment2EntityBeanLocator.locateBean(a2OTPKey);
		Set<String> refSet = new HashSet();
		if (assignment != null && assignment.getAttachmentSet() != null){
			for (AssignmentAttachment aa : assignment.getAttachmentSet()) {
				refSet.add(aa.getAttachmentReference());
			}
		}
		makeAttachment(tofill, divID, currentViewID, refSet, remove, 0);
	}
	
	public void makeAttachmentFromSubmissionAttachmentSet(UIContainer tofill, String divID, String currentViewID,
			Set<SubmissionAttachment> asaSet, Boolean remove) {
		Set<String> refSet = new HashSet();
		if (asaSet != null) {
			for (SubmissionAttachment asa : asaSet) {
				refSet.add(asa.getAttachmentReference());
			}
		}
		makeAttachment(tofill, divID, currentViewID, refSet, remove, 0);
	}
	
	public void makeAttachmentFromFeedbackAttachmentSet(UIContainer tofill, String divID, String currentViewID,
			Set<FeedbackAttachment> afaSet, Boolean remove) {
		Set<String> refSet = new HashSet();
		if (afaSet != null) {
			for (FeedbackAttachment afa : afaSet) {
				refSet.add(afa.getAttachmentReference());
			}
		}
		makeAttachment(tofill, divID, currentViewID, refSet, remove, 0);
	}

	
    public void makeAttachment(UIContainer tofill, String divID, String currentViewID, Set<String> refSet, Boolean remove, int idOffset) {
    	//First add those in session from AttachmentBean
    	if (attachmentBean.attachmentRefs != null && attachmentBean.attachmentRefs.length > 0) {
    		for (String ref : attachmentBean.attachmentRefs) {
    			if (ref != null) {
    				refSet.add(ref);
    			}
    		}
    	}
    	
    	int i = 1;
    	//basically if idOffset = 0, then this is a fresh attachment list
    	// 	then you can add the line "No Attachments" to toggle on/off
    	if (idOffset == 0) {
	        UIJointContainer joint = new UIJointContainer(tofill, divID, "attachments:", ""+1);
	        UIMessage li = UIMessage.make(joint, "no_attachments_yet", "assignment2.no_attachments_yet");
	        
	        //If no refs, set to hidden
	        if (refSet.size() > 0){
	        	Map<String,String> styleMap = new HashMap<String,String>();
	        	styleMap.put("display", "none");
	        	li.decorate(new UICSSDecorator(styleMap));
	        }
	        i++;
    	}
    	
    	int index = 0;
        for (String ref : refSet){
        	UIJointContainer joint = new UIJointContainer(tofill, divID, "attachments:", ""+(i+idOffset));
	        try {
	    		ContentResource cr = contentHostingService.getResource(ref);
	    		UILink.make(joint, "attachment_image", externalLogic.getContentTypeImagePath(cr));
	    		UILink.make(joint, "attachment_link", cr.getProperties().getProperty(cr.getProperties().getNamePropDisplayName()),
	    				cr.getUrl());
	    		String file_size = getReadableFileSize(cr.getContentLength());
	    		UIOutput.make(joint, "attachment_size", file_size);
	    		UIInput hidden = UIInput.make(joint, "attachment_item", "#{AttachmentBean.attachmentRefs." + Integer.valueOf(index+idOffset) + "}", ref);
	    		
	    		hidden.mustapply = true;
	    		
	    		//Add remove link
	    		if (remove) {
	    			UIVerbatim.make(joint, "attachment_remove", 
	    					"<a href=\"#\" " +
	    					"onclick=\"" +
	    					"jQuery(this).parent('span').parent('li').remove();updateDisplayNoAttachments();" +
	    					"\">" +
	    					messageLocator.getMessage("assignment2.remove") +
	    					"</a>");
	    		}
			} catch (Exception e) {
				LOG.error(e.getMessage(), e);
				//do nothing
			}
			i++;
			index++;
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
