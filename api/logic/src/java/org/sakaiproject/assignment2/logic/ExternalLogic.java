package org.sakaiproject.assignment2.logic;

import java.util.Collection;
import java.util.List;

/**
 * This is the interface for logic which is external to our app logic
 * 
 * @author Sakai App Builder -AZ
 */
public interface ExternalLogic {

    public final static String NO_LOCATION = "noLocationAvailable";

    // permissions
    public final static String ASSIGNMENT2_CREATE = "assignment2.create";
    public final static String ASSIGNMENT2_DELETE = "assignment2.delete";
    public final static String ASSIGNMENT2_REVISE = "assignment2.revise";
    public final static String ASSIGNMENT2_SUBMIT = "assignment2.submit";
    public final static String ASSIGNMENT2_READ = "assignment2.read";
    public final static String ASSIGNMENT2_RECEIVE_NOTIF = "assignment2.receive.notifications";
    public final static String ASSIGNMENT2_ALL_GROUPS_UNGRADED = "assignment2.allGroups.ungraded";

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
     * 
     * @return the current context for the current user
     */
    public String getCurrentContextId();

    /**
     * Check if this user has super admin access
     * 
     * @param userId
     *            the internal user id (not username)
     * @return true if the user has admin access, false otherwise
     */
    public boolean isUserAdmin(String userId);

    /**
     * Check if the current user has a specified permission within the current
     * site, primarily a convenience method and passthrough
     * 
     * @param permission
     *            a permission string constant
     * @return true if allowed, false otherwise
     */
    public boolean getCurrentUserHasPermission(String permission);

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

    /**
     * Return a Collection of all Groups
     * @return a collection
     */
    public Collection getSiteGroups();
    
    /**
     * 
     * @return a collection of the groups that the current user is a member of
     */
    public Collection getCurrentUserMemberships();
    
    /**
     * 
     * @return list of the group ids of the groups that the current user is
     * a member of
     */
    public List getCurrentUserGroupIdList();

}
