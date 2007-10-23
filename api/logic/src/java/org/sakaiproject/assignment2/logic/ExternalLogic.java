package org.sakaiproject.assignment2.logic;

/**
 * This is the interface for logic which is external to our app logic
 * 
 * @author Sakai App Builder -AZ
 */
public interface ExternalLogic {

    public final static String NO_LOCATION = "noLocationAvailable";

    public final static String ANON_USER_PREFIX = "anon_";

    // permissions
    public final static String ASSIGNMENT2_CREATE = "assignment2.create";

    /**
     * @return the current sakai user id (not username)
     */
    public String getCurrentUserId();

    /**
     * Get the display name for a user by their unique id
     * 
     * @param userId
     *            the current sakai user id (not username)
     * @return display name (probably firstname lastname) or "----------" (10 hyphens) if none found
     */
    public String getUserDisplayName(String userId);

    /**
     * @return the current location id of the current user
     */
    public String getCurrentLocationId();

    /**
     * Check if this user has super admin access
     * 
     * @param userId
     *            the internal user id (not username)
     * @return true if the user has admin access, false otherwise
     */
    public boolean isUserAdmin(String userId);

    /**
     * Check if a user has a specified permission within a context, primarily a convenience method and passthrough
     * 
     * @param userId
     *            the internal user id (not username)
     * @param permission
     *            a permission string constant
     * @param locationId
     *            a unique id which represents the current location of the user (entity reference)
     * @return true if allowed, false otherwise
     */
    public boolean isUserAllowedInLocation(String userId, String permission, String locationId);

    /**
     * Cleans up the users submitted strings to protect us from XSS
     * 
     * @param userSubmittedString any string from the user which could be dangerous
     * @return a cleaned up string which is now safe
     */
    public String cleanupUserStrings(String userSubmittedString);
    
    /**
     * Returns URL to viewId pass in
     * @param viewId of view to build path to
     * @return a url path to the vie
     */
    public String getAssignmentViewUrl(String viewId);

}
