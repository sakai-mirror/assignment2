package org.sakaiproject.assignment2.logic.impl;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.ExternalContentReviewLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.contentreview.service.ContentReviewService;
import org.sakaiproject.contentreview.exception.QueueException;

public class ExternalContentReviewLogicImpl implements ExternalContentReviewLogic {

    private static Log log = LogFactory.getLog(ExternalContentReviewLogicImpl.class);
    
    private ContentReviewService contentReview;
    public void setContentReviewService(ContentReviewService contentReview) {
        this.contentReview = contentReview;
    }

    public void init(){
        if(log.isDebugEnabled()) log.debug("init");
    }

    public boolean isContentReviewAvailable() {
        return true;
    }
    
    public void reviewAttachment(String userId, String siteId, Assignment2 assign, String attachmentReference) {
        if (assign == null || attachmentReference == null) {
            throw new IllegalArgumentException("Null assignment or contentId passed to " +
            		"reviewAttachments. assign: " + " contentId: " + attachmentReference);
        }
        
        try
        {
            contentReview.queueContent(userId, siteId, getTaskId(assign), attachmentReference);
        }
        catch (QueueException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    
    /**
     * 
     * @param assign
     * @return the "taskId" required by the {@link ContentReviewService} to uniquely
     * identify this assignment in the service
     */
    private String getTaskId(Assignment2 assign) {
        return "Asnn2 Provisioned " + assign.getId();
    }

}