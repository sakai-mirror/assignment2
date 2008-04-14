package org.sakaiproject.assignment2.tool.producers.fragments;

import java.util.Set;
import java.util.HashSet;

import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
import org.sakaiproject.assignment2.model.FeedbackAttachment;
import org.sakaiproject.assignment2.tool.params.FragmentAttachmentsViewParams;
import org.sakaiproject.assignment2.tool.producers.renderers.AttachmentListRenderer;

import org.sakaiproject.tool.api.SessionManager;
import org.sakaiproject.tool.api.ToolSession;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.content.ContentTypeReporter;
import uk.org.ponder.rsf.content.ContentTypeInfoRegistry;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class FragmentAttachmentsProducer implements ViewComponentProducer, ViewParamsReporter, ContentTypeReporter{
	
    public static final String VIEW_ID = "fragment-attachments";
    public String getViewID() {
        return VIEW_ID;
    }
    
	private AttachmentListRenderer attachmentListRenderer;
	public void setAttachmentListRenderer(AttachmentListRenderer attachmentListRenderer){
		this.attachmentListRenderer = attachmentListRenderer;
	}
	
    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	FragmentAttachmentsViewParams params = (FragmentAttachmentsViewParams) viewparams;
    	
    	Set<String> set = new HashSet<String>();
    	set.add(params.attachmentRef);
    	
    	if (params.idOffset < 1) {
    		params.idOffset = 0;
    	}
    	
    	attachmentListRenderer.makeAttachment(tofill, "attachment_list:", params.viewID, set, params.remove, params.idOffset);
    	
    }
    
	public ViewParameters getViewParameters() {
	    return new FragmentAttachmentsViewParams();
	}
	
	public String getContentType() {
		return ContentTypeInfoRegistry.HTML_FRAGMENT;
	}
}