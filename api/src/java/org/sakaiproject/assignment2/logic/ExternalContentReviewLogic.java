package org.sakaiproject.assignment2.logic;

import org.sakaiproject.assignment2.model.Assignment2;

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
     * for review
     * @param userId if null, assumes current user
     * @param siteId if null, assumes current site
     * @param assign the assignment that this attachment is associated with
     * @param attachmentReference the reference for the attachment in content hosting
     */
    public void reviewAttachment(String userId, String siteId, Assignment2 assign, String attachmentReference);

}
