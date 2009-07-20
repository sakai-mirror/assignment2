package org.sakaiproject.assignment2.logic;

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

}
