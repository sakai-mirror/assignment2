package org.sakaiproject.assignment2.logic;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.sakaiproject.content.api.ContentResource;

/**
 * This is the interface for logic which is external to our app logic
 * 
 * @author Sakai App Builder -AZ
 */
public interface ExternalLogic {

    public final static String NO_LOCATION = "noLocationAvailable";

    // permissions
    public final static String ASSIGNMENT2_EDIT = "assignment2.edit";
    public final static String ASSIGNMENT2_SUBMIT = "assignment2.submit";
    public final static String ASSIGNMENT2_READ = "assignment2.read";
    public final static String ASSIGNMENT2_RECEIVE_NOTIF = "assignment2.receive.notifications";
    public final static String ASSIGNMENT2_ALL_GROUPS_UNGRADED = "assignment2.allGroups.ungraded";
    
    //tool ids for external tools that we integrate with
    public final static String TOOL_ID_SCHEDULE = "sakai.schedule";
    public final static String TOOL_ID_ANNC = "sakai.announcements";

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
    
    /**
     * 
     * @return a map of group id to group name for all of the sections/groups
     * associated with the current site
     */
    public Map getGroupIdToNameMapForSite();
    
    /**
     * @param contextId
     * @param toolId
     * @return true if tool with the given toolId exists in the site with the given siteId
     */
    public boolean siteHasTool(String contextId, String toolId);

    /**
     * 
     * @param contentReference
     * @return String of path for <img> tag for resource image type icon
     */
    public String getContentTypeImagePath(ContentResource contentReference);
}
