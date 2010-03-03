/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/branches/ASNN-617/api/src/java/org/sakaiproject/assignment2/logic/AssignmentBundleLogic.java $
 * $Id: AssignmentBundleLogic.java 61480 2009-06-29 18:39:09Z swgithen@mtu.edu $
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

import java.util.List;

import org.sakaiproject.assignment2.model.constants.AssignmentConstants;

/**
 * This interface is used for Assignment2-specific authorization based upon
 * the fine-grained site-scoped Sakai permissions. This is solely used for
 * answering questions based upon these Sakai permissions and is not meant
 * for use outside the logic layer. For more situation-specific
 * permission answers, use the {@link AssignmentPermissionLogic}
 *
 */
public interface AssignmentAuthzLogic {
    
    /**
     * permissions that are considered assignment-level permissions and
     * should be checked for group access
     */
    public static final String[] assignmentLevelPermissions = {
        AssignmentConstants.PERMISSION_ADD_ASSIGNMENTS,
        AssignmentConstants.PERMISSION_EDIT_ASSIGNMENTS,
        AssignmentConstants.PERMISSION_MANAGE_SUBMISSIONS,
        AssignmentConstants.PERMISSION_REMOVE_ASSIGNMENTS,
        AssignmentConstants.PERMISSION_SUBMIT,
        AssignmentConstants.PERMISSION_VIEW_ASSIGNMENTS
    };
    
    /**
     * permissions that are considered site-level permissions and
     * do not need to be checked for group access
     */
    public static final String[] siteLevelPermissions = {
        AssignmentConstants.PERMISSION_ALL_GROUPS
    };
    
    // The following 3 groups should represent all of the permissions except for
    // "All Groups".  No permission should be in more than one of these 3 categories:
    /**
     * 
     * Assignment permissions that require the user to either
     * have {@link AssignmentConstants#PERMISSION_ALL_GROUPS} or they must be
     * a member of every group associated with the assignment to have permission.
     * If the assignment is not restricted to groups, the user will not have
     * permission without the "all groups" permission.
     */
    public static final String[] permissionsThatRequireAllGroups = {
            AssignmentConstants.PERMISSION_ADD_ASSIGNMENTS, 
            AssignmentConstants.PERMISSION_EDIT_ASSIGNMENTS, 
            AssignmentConstants.PERMISSION_REMOVE_ASSIGNMENTS};
    
    /**
     * Assignment permissions that require the user to either
     * have {@link AssignmentConstants#PERMISSION_ALL_GROUPS} or they must be
     * a member of at least one of the groups associated with the assignment
     * to have permission. If the assignment is not restricted to groups, the
     * user will not have permission without the "all groups" permission.
     */
    public static final String[] permissionsThatRequireOneGroup = {
            AssignmentConstants.PERMISSION_MANAGE_SUBMISSIONS};
    
    /**
     * Assignment permissions that, if assignment is restricted to groups, require the user to either
     * have {@link AssignmentConstants#PERMISSION_ALL_GROUPS} or they must be
     * a member of at least one of the groups associated with the assignment
     * to have permission. If the assignment is not restricted to groups, the
     * user WILL have permission without the "all groups" permission.
     */
    public static final String[] permissionsForAtLeastOneOrNoGroups = {
            AssignmentConstants.PERMISSION_SUBMIT,
            AssignmentConstants.PERMISSION_VIEW_ASSIGNMENTS};
    
    //////  END GROUPINGS
    
    /**
     * 
     * @param contextId
     * @return true if the current user's role has the "add assignments" permission
     */
    public boolean userHasAddPermission(String contextId);
    
    /**
     * 
     * @param contextId
     * @return true if the current user's role has the "edit assignments" permission
     */
    public boolean userHasEditPermission(String contextId);
    
    /**
     * 
     * @param contextId
     * @return true if the current user's role has the "delete assignments" permission
     */
    public boolean userHasDeletePermission(String contextId);
    
    /**
     * 
     * @param contextId
     * @return true if the current user's role permissions are for all groups
     */
    public boolean userHasAllGroupsPermission(String contextId);
    
    /**
     * @param userId
     * @param contextId
     * @return true if the given user's role permissions are for all groups
     */
    public boolean userHasAllGroupsPermission(String userId, String contextId);
    
    /**
     * 
     * @param contextId
     * @return true if the current user's role has permission to submit assignments
     */
    public boolean userHasSubmitPermission(String contextId);
    
    /**
     * 
     * @param contextId
     * @return true if the current user's role has permission to manage submissions
     * (ie view, provide feedback, etc)
     */
    public boolean userHasManageSubmissionsPermission(String contextId);
    
    /**
     * 
     * @param contextId
     * @param permission sakai realm permission such as {@link AssignmentConstants#PERMISSION_ADD_ASSIGNMENTS}
     * @return true if the current user's role has the given permission in the given contextId
     */
    public boolean userHasPermission(String contextId, String permission);
    
    /**
     * 
     * @param userId
     * @param contextId
     * @param permission sakai realm permission such as {@link AssignmentConstants#PERMISSION_ADD_ASSIGNMENTS}
     * @return true if the given user's role has the given permission in the given contextId
     */
    public boolean userHasPermission(String userId, String contextId, String permission);
    
    /**
     * 
     * @return a list of the permissions that are considered assignment-level permissions and
     * should be checked for group access
     */
    public List<String> getAssignmentLevelPermissions();
    
    /**
     * 
     * @return a list of the permissions that are considered site-level and do
     * not need to be checked for group access
     */
    public List<String> getSiteLevelPermissions();

    /**
     * 
     * @return a list of assignment permissions that require the user to either
     * have {@link AssignmentConstants#PERMISSION_ALL_GROUPS} or they must be
     * a member of every group associated with the assignment to have permission.
     * If the assignment is not restricted to groups, the user will not have
     * permission without the "all groups" permission.
     */
    public List<String> getPermissionsThatRequireAllGroups();
    
    /**
     * 
     * @return a list of assignment permissions that require the user to either
     * have {@link AssignmentConstants#PERMISSION_ALL_GROUPS} or they must be
     * a member of at least one of the groups associated with the assignment
     * to have permission. If the assignment is not restricted to groups, the
     * user will not have permission without the "all groups" permission.
     */
    public List<String> getPermissionsThatRequireOneGroup();
    
    /**
     * 
     * @return a list of assignment permissions that, if assignment is restricted to groups, 
     * require the user to either have {@link AssignmentConstants#PERMISSION_ALL_GROUPS} or they must be
     * a member of at least one of the groups associated with the assignment
     * to have permission. If the assignment is not restricted to groups, the
     * user WILL have permission without the "all groups" permission.
     */
    public List<String> getPermissionsForAtLeastOneOrNoGroups();
}