package org.sakaiproject.assignment2.logic;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.sakaiproject.content.api.ContentResource;
import org.sakaiproject.user.api.User;

/**
 * This is the interface for logic which is external to our app logic
 * 
 * @author Sakai App Builder -AZ
 */
public interface ExternalLogic {

    public final static String NO_LOCATION = "noLocationAvailable";
    
    //tool ids for external tools that we integrate with
    /**
     * the tool id for Sakai's Schedule/Calendar tool
     */
    public final static String TOOL_ID_SCHEDULE = "sakai.schedule";
    /**
     * the tool id for Sakai's Announcements tool
     */
    public final static String TOOL_ID_ANNC = "sakai.announcements";
    /**
     * the tool id for Sakai's original Assignments tool
     */
    public final static String TOOL_ID_OLD_ASSIGN = "sakai.assignment.grades";
    /**
     * the tool if for this Assignment tool
     */
    public final static String TOOL_ID_ASSIGNMENT2 = "sakai.assignment2";

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
     *  Get the user's full name in Last, First format
     * @param userId
     *            the current sakai user id (not username)
     * @return full name in Last, First format
     */
    public String getUserFullName(String userId);
    
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
     * 
     * @return the title of the Assignment2 tool
     */
    public String getToolTitle();

    /**
     * Check if this user has super admin access
     * 
     * @param userId
     *            the internal user id (not username)
     * @return true if the user has admin access, false otherwise
     */
    public boolean isUserAdmin(String userId);

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
     * @param contextId
     * @return a collection of Groups associated with the given contextId
     */
    public Collection getSiteGroups(String contextId);
    
    /**
     * @param userId
     * @param contextId
     * @return a collection of the groups that the given user is a member of
     * in the given contextId
     */
    public Collection getUserMemberships(String userId, String contextId);
    
    /**
     * @param userId
     * @param contextId
     * @return list of the group ids of the groups that the given user is
     * a member of in the given contextId
     */
    public List<String> getUserMembershipGroupIdList(String userId, String contextId);
    
    /**
     * @param currentContextId
     * @return a map of group id to group name for all of the sections/groups
     * associated with the given contextId
     */
    public Map<String, String> getGroupIdToNameMapForSite(String currentContextId);
    
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
    
    /**
     * 
     * @param contextId
     * @return a list of the student Ids of all of the students in the given site
     */
    public List<String> getStudentsInSite(String contextId);
    
    /**
     * 
     * @param sectionId
     * @return a list of the student ids of students in the given section  
     */
    public List<String> getStudentsInSection(String sectionId);
    
    /**
     * 
     * @param gradeableObjectId
     * @param returnViewId
     * @return url to helper
     */
    public String getUrlForGradebookItemHelper(Long gradeableObjectId, String returnViewId);
    
    /**
     * 
     * @param gradeableObjectId
     * @param userId
     * @param returnViewId
     * @return url to helper
     */
    public String getUrlForGradeGradebookItemHelper(Long gradeableObjectId, String userId, String returnViewId);
    
    /**
     * 
     * @param userId
     * @return
     */
    public User getUser(String userId);
    
    /**
     * 
     * @param sizeVal
     * @return a string representing a human readable format of the file size
     */
    public String getReadableFileSize(int sizeVal);
}
