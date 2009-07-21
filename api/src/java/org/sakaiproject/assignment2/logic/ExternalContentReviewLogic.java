package org.sakaiproject.assignment2.logic;

import java.util.Collection;
import java.util.List;

import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
import org.sakaiproject.contentreview.model.ContentReviewItem;

/**
 * For accessing the systems Content Review System.  An example is a plagiarism
 * checking tool.
 * 
 * @author sgithens
 *
 */
public interface ExternalContentReviewLogic {

    /**
     * This will determine whether the current site has the ability to use
     * the Content Review Service.
     * 
     * This will most likely be looking at the current site on the request 
     * thread. There will be another version for passing in a site, reference,
     * or other 3akai like thing to determine if that object/resource has
     * the ability to use Content Review.
     * 
     * @return
     */
    public boolean isContentReviewAvailable();
    
    /**
     * Submit the attachment with the given content hosting reference to the content review service
     * for review. To minimize errors, check {@link #isAttachmentAcceptableForReview(String)} before
     * calling this method
     * @param userId if null, assumes current user
     * @param assign the assignment that this attachment is associated with
     * @param attachmentReference the reference for the attachment in content hosting
     */
    public void reviewAttachment(String userId, Assignment2 assign, String attachmentReference);
    
    /**
     * 
     * @param assign
     * @return a list of the {@link ContentReviewItem}s associated with the given assignment
     */
    public List<ContentReviewItem> getReviewItemsForAssignment(Assignment2 assign);
    
    /**
     * 
     * @param attachmentReference
     * @return true if the attachment with the given reference is acceptable for
     * the review service
     */
    public boolean isAttachmentAcceptableForReview(String attachmentReference);
    
    /**
     * 
     * @param assignment
     * @param attachments set of {@link SubmissionAttachment}s for the given assignment that you
     * want to retrieve review info for (ie score, icon url, etc)
     * @param instructorView true if this report is for the instructor view. false if this is
     * for the student view
     */
    public void populateReviewProperties(Assignment2 assignment, Collection<SubmissionAttachment> attachments, boolean instructorView);

    /**
     * 
     * @param attachmentReference
     * @param instructorView true if you want the report for an instructor. false if
     * you want the report for the student
     * @return the url of the review report for the given attachmentReference. Returns
     * null if url cannot be retrieved
     */
    public String getReportUrl(String attachmentReference, boolean instructorView);
}
