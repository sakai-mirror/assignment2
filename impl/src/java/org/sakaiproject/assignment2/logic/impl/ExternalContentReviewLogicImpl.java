package org.sakaiproject.assignment2.logic.impl;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.ExternalContentLogic;
import org.sakaiproject.assignment2.logic.ExternalContentReviewLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.content.api.ContentResource;
import org.sakaiproject.contentreview.exception.QueueException;
import org.sakaiproject.contentreview.exception.ReportException;
import org.sakaiproject.contentreview.exception.SubmissionException;
//import org.sakaiproject.contentreview.model.ContentReviewItem;
import org.sakaiproject.contentreview.service.ContentReviewService;

public class ExternalContentReviewLogicImpl implements ExternalContentReviewLogic {

    private static Log log = LogFactory.getLog(ExternalContentReviewLogicImpl.class);
    
    private ContentReviewService contentReview;
    public void setContentReviewService(ContentReviewService contentReview) {
        this.contentReview = contentReview;
    }
    
    private ExternalContentLogic contentLogic;
    public void setExternalContentLogic(ExternalContentLogic contentLogic) {
        this.contentLogic = contentLogic;
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
    
    /* TODO figure out dependency issue for ContentReviewItem
public List<ContentReviewItem> getReviewItemsForAssignment(Assignment2 assign) {
        if (assign == null) {
            throw new IllegalArgumentException("Null assignment passed to getReviewItemsForAssignment");
        }
        
        List<ContentReviewItem> reviewItems = new ArrayList<ContentReviewItem>();
        
        try
        {
            reviewItems = contentReview.getReportList(assign.getContextId(), getTaskId(assign));
        }
        catch (QueueException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (SubmissionException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (ReportException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        return reviewItems;
    }*/
    
    public boolean isAttachmentAcceptableForReview(String attachmentReference) {
        if (attachmentReference == null) {
            throw new IllegalArgumentException("Null attachmentReference passed to isAttachmentEligibleForReview");
        }
        
        boolean acceptable = false;
        
        // we need to retrieve the ContentResource for this attachment
        ContentResource resource = contentLogic.getContentResource(attachmentReference);
        if (resource != null) {
            acceptable = contentReview.isAcceptableContent(resource);
        }
        
        return acceptable;
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