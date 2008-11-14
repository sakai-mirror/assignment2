/**********************************************************************************
 * $URL$
 * $Id$
 ***********************************************************************************
 *
 * Copyright (c) 2007 The Sakai Foundation.
 * 
 * Licensed under the Educational Community License, Version 1.0 (the "License"); 
 * you may not use this file except in compliance with the License. 
 * You may obtain a copy of the License at
 * 
 *      http://www.opensource.org/licenses/ecl1.php
 * 
 * Unless required by applicable law or agreed to in writing, software 
 * distributed under the License is distributed on an "AS IS" BASIS, 
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 *
 **********************************************************************************/

package org.sakaiproject.assignment2.logic;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.sakaiproject.content.api.ContentResource;
import org.sakaiproject.site.api.Site;
import org.sakaiproject.user.api.User;

/**
 * This is the interface for logic which is external to our app logic
 * 
 * @author Sakai App Builder -AZ
 */
public interface ExternalLogic {
    
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
     * 
     * @param userId
     * @return the User object associated with the given userId. Returns null if no User object found.
     */
    public User getUser(String userId);

    /**
     * Get the display name for a user by their unique id
     * 
     * @param userId
     *            the current sakai user id (not username)
     * @return display name (probably firstname lastname) or "----------" (10 hyphens) if none found
     */
    public String getUserDisplayName(String userId);

    /**
     * @param userId
     *            the current sakai user id (not username)
     * @return the user's sort name as defined in the User object
     */
    public String getUserSortName(String userId);
    
    /**
     * 
     * @param userId
     * @return the email address associated with this userId. Returns null if not found.
     */
    public String getUserEmail(String userId);
    
    /**
     * 
     * @return the current context for the current user
     */
    public String getCurrentContextId();
    
    /**
     * 
     * @param contextId
     * @return the Site associated with the given contextId.
     * Returns null if the Site could not be retrieved.
     */
    public Site getSite(String contextId);
    
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
     * @param contextId
     * @return a list of userIds of members of this site with a "student"-type role
     */
    public List<String> getStudentsInSite(String contextId);
    
    /**
     * 
     * @param contextId
     * @return a list of userIds of members of this site with a "TA"-type role
     * 
     */
    public List<String> getTAsInSite(String contextId);
    
    /**
     * 
     * @param contextId
     * @return a list of userIds of members of this site with an "instructor"-type role
     */
    public List<String> getInstructorsInSite(String contextId);
    
    /**
     * 
     * @param groupId
     * @return a list of the student ids of students in the Group with the given groupId  
     */
    public List<String> getStudentsInGroup(String groupId);
    
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
     * @param gradebookItemName - the gradebook item name that you would like the helper
     * to have automatically populated
     * @param returnViewId
     * @return url to the "create a gradebook item" helper.
     */
    public String getUrlForGradebookItemHelper(Long gradeableObjectId, String gradebookItemName, String returnViewId);
    
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
     * @param userIds
     * @return given a list of userIds, returns a map of userId to the associated
     * User object
     */
    public Map<String, User> getUserIdUserMap(List<String> userIds);
    
    /**
     * 
     * @param contextId
     * @return a map of the displayId to userId for all of the students in the
     * given site. Useful for display scenarios that require use of the displayId
     * (such as upload and download) that we need to convert to the equivalent
     * userId for processing
     */
    public Map<String, String> getUserDisplayIdUserIdMapForStudentsInSite(String contextId);
}
