package org.sakaiproject.assignment2.logic.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.ExternalContentLogic;
import org.sakaiproject.assignment2.logic.ExternalContentReviewLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.component.cover.ComponentManager;
import org.sakaiproject.content.api.ContentResource;
import org.sakaiproject.contentreview.exception.QueueException;
import org.sakaiproject.contentreview.exception.ReportException;
import org.sakaiproject.contentreview.exception.SubmissionException;
import org.sakaiproject.contentreview.model.ContentReviewItem;
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
        //if no contentReviewService was set try discovering it
        if (contentReview == null)
        {
            contentReview = (ContentReviewService) ComponentManager.get(ContentReviewService.class.getName());
        }
    }

    public boolean isContentReviewAvailable() {
        boolean available = false;
        if (contentReview != null) {
            available = true;
        }

        return available;
    }

    public void reviewAttachment(String userId, Assignment2 assign, String attachmentReference) {
        if (assign == null || attachmentReference == null) {
            throw new IllegalArgumentException("Null assignment or contentId passed to " +
                    "reviewAttachments. assign: " + " contentId: " + attachmentReference);
        }

        try
        {
            contentReview.queueContent(userId, assign.getContextId(), getTaskId(assign), attachmentReference);
        }
        catch (QueueException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }


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
    }

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
    
    public void populateReviewProperties(Assignment2 assignment, Collection<SubmissionAttachment> attachments, boolean instructorView) {
        if (assignment == null) {
            throw new IllegalArgumentException("Null assignment passed to populateReviewProperties");
        }

        if (attachments != null && !attachments.isEmpty()) {
            // let's get all of the review items for this assignment
            List<ContentReviewItem> allReviewItems = getReviewItemsForAssignment(assignment);
            // put these items into a map of the attachment reference to the review item for easier access
            Map<String, ContentReviewItem> attRefReviewItemMap = new HashMap<String, ContentReviewItem>();
            if (allReviewItems != null) {
                for (ContentReviewItem reviewItem : allReviewItems) {
                    attRefReviewItemMap.put(reviewItem.getContentId(), reviewItem);
                }
            }

            // now let's iterate through the passed attachments and populate the
            // properties, if appropriate
            for (SubmissionAttachment attach : attachments) {
                if (attRefReviewItemMap.containsKey(attach.getAttachmentReference())) {
                    ContentReviewItem reviewItem = attRefReviewItemMap.get(attach.getAttachmentReference());
                    populateProperties(reviewItem, attach, instructorView);
                }
            }
        }
    }
    
    /**
     * Populates the properties from this review item on the given attach
     * @param reviewItem
     * @param attach
     * @param instructorView true if this is for the instructor view. false if for student view
     */
    private void populateProperties(ContentReviewItem reviewItem, SubmissionAttachment attach, boolean instructorView) {
        if (reviewItem == null || attach == null || !reviewItem.getContentId().equals(attach.getAttachmentReference())) {
            throw new IllegalArgumentException("invalid params passed to populateProperties");
        }
        
        Map properties = attach.getProperties() != null ? attach.getProperties() : new HashMap();
        properties.put(AssignmentConstants.PROP_REVIEW_SCORE, reviewItem.getReviewScore());
        properties.put(AssignmentConstants.PROP_REVIEW_ICON_URL, reviewItem.getIconUrl());

        // now retrieve the report url
        String reportUrl = getReportUrl(attach.getAttachmentReference(), instructorView);
        if (reportUrl != null) {
            properties.put(AssignmentConstants.PROP_REVIEW_URL, reportUrl);
        }

        attach.setProperties(properties);
    }
    
    public String getReportUrl(String attachmentReference, boolean instructorView) {
        if (attachmentReference == null) {
            throw new IllegalArgumentException("Null attachmentReference passed to getReportUrl");
        }
        
        String reportUrl = null;
        
        if (instructorView) {
            try
            {
                reportUrl = contentReview.getReviewReportInstructor(attachmentReference);
            }
            catch (QueueException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            catch (ReportException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        } else {
            try
            {
                reportUrl = contentReview.getReviewReportStudent(attachmentReference);
            }
            catch (QueueException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            catch (ReportException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        
        return reportUrl;
    }

}