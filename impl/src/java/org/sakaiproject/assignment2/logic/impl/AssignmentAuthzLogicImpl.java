/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/branches/ASNN-617/impl/src/java/org/sakaiproject/assignment2/logic/impl/AssignmentBundleLogicImpl.java $
 * $Id: AssignmentBundleLogicImpl.java 61481 2009-06-29 18:47:43Z swgithen@mtu.edu $
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

package org.sakaiproject.assignment2.logic.impl;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.AssignmentAuthzLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.authz.api.FunctionManager;
import org.sakaiproject.authz.api.SecurityService;
import org.sakaiproject.site.api.SiteService;
import org.sakaiproject.user.api.User;

/**
 * Used for Assignment2-specific authorization based upon
 * the fine-grained site-scoped Sakai permissions. This is solely used for
 * answering questions based upon these Sakai permissions and is not meant
 * for use outside the logic layer. For more situation-specific
 * permission answers, use the {@link AssignmentPermissionLogic}
 */
public class AssignmentAuthzLogicImpl implements AssignmentAuthzLogic
{
    private static Log log = LogFactory.getLog(AssignmentAuthzLogicImpl.class);
    
    private FunctionManager functionManager;
    private SecurityService securityService;
    private SiteService siteService;

    public void init() {
        if (log.isDebugEnabled()) log.debug("init");
        
        registerPermissions();
    }
    
    /**
     * Register the assignment2 permissions
     */
    protected void registerPermissions() {
        // register Sakai permissions for this tool
        functionManager.registerFunction(AssignmentConstants.PERMISSION_ADD_ASSIGNMENTS);
        functionManager.registerFunction(AssignmentConstants.PERMISSION_EDIT_ASSIGNMENTS);
        functionManager.registerFunction(AssignmentConstants.PERMISSION_SUBMIT);
        functionManager.registerFunction(AssignmentConstants.PERMISSION_ALL_GROUPS);
        functionManager.registerFunction(AssignmentConstants.PERMISSION_MANAGE_SUBMISSIONS);
        functionManager.registerFunction(AssignmentConstants.PERMISSION_VIEW_ASSIGNMENTS);
        functionManager.registerFunction(AssignmentConstants.PERMISSION_REMOVE_ASSIGNMENTS);
    }
    
    public boolean userHasAddPermission(String userId, String contextId) {
        String permission = AssignmentConstants.PERMISSION_ADD_ASSIGNMENTS;
        if (userId == null) {
            return userHasPermission(contextId, permission);
        } else {
            return userHasPermission(userId, contextId, permission);
        }
    }
    
    public boolean userHasEditPermission(String userId, String contextId) {
        String permission = AssignmentConstants.PERMISSION_EDIT_ASSIGNMENTS;
        if (userId == null) {
            return userHasPermission(contextId, permission);
        } else {
            return userHasPermission(userId, contextId, permission);
        }
    }
    
    public boolean userHasDeletePermission(String userId, String contextId) {
        String permission = AssignmentConstants.PERMISSION_REMOVE_ASSIGNMENTS;
        if (userId == null) {
            return userHasPermission(contextId, permission);
        } else {
            return userHasPermission(userId, contextId, permission);
        }
    }
    
    public boolean userHasAllGroupsPermission(String userId, String contextId) {
        String permission = AssignmentConstants.PERMISSION_ALL_GROUPS;
        if (userId == null) {
            return userHasPermission(contextId, permission);
        } else {
            return userHasPermission(userId, contextId, permission);
        }
    }
    
    public boolean userHasSubmitPermission(String userId, String contextId) {
        String permission = AssignmentConstants.PERMISSION_SUBMIT;
        if (userId == null) {
            return userHasPermission(contextId, permission);
        } else {
            return userHasPermission(userId, contextId, permission);
        }
    }
    
    public boolean userHasManageSubmissionsPermission(String userId, String contextId) {
        String permission = AssignmentConstants.PERMISSION_MANAGE_SUBMISSIONS;
        if (userId == null) {
            return userHasPermission(contextId, permission);
        } else {
            return userHasPermission(userId, contextId, permission);
        }
    }
    
    public boolean userHasViewAssignmentPermission(String userId, String contextId) {
        String permission = AssignmentConstants.PERMISSION_VIEW_ASSIGNMENTS;
        if (userId == null) {
            return userHasPermission(contextId, permission);
        } else {
            return userHasPermission(userId, contextId, permission);
        }
    }
    
    public boolean userHasPermission(String contextId, String permission) {
        return securityService.unlock(permission, siteService.siteReference(contextId));
    }
    
    public boolean userHasPermission(String userId, String contextId, String permission) {
        return securityService.unlock(userId, permission, siteService.siteReference(contextId));
    }
    
    public List<String> getAssignmentLevelPermissions() {
        List<String> assignmentPerms = new ArrayList<String>();
        for (int i=0; i < assignmentLevelPermissions.length; i++) {
            assignmentPerms.add(assignmentLevelPermissions[i]);
        }
        
        return assignmentPerms;
    }
    
    public List<String> getSiteLevelPermissions() {
        List<String> sitePerms = new ArrayList<String>();
        for (int i=0; i < siteLevelPermissions.length; i++) {
            sitePerms.add(siteLevelPermissions[i]);
        }
        
        return sitePerms;
    }
    
    public List<String> getPermissionsThatRequireAllGroups() {
        List<String> permsWithAllGroups = new ArrayList<String>();
        for (int i=0; i < permissionsThatRequireAllGroups.length; i++) {
            permsWithAllGroups.add(permissionsThatRequireAllGroups[i]);
        }
        
        return permsWithAllGroups;
    }
    
    public List<String> getPermissionsThatRequireOneGroup() {
        List<String> permsWithOneGroup = new ArrayList<String>();
        for (int i=0; i < permissionsThatRequireOneGroup.length; i++) {
            permsWithOneGroup.add(permissionsThatRequireOneGroup[i]);
        }
        return permsWithOneGroup;
    }
    
    public List<String> getPermissionsForAtLeastOneOrNoGroups() {
        List<String> oneOrNoGroups = new ArrayList<String>();
        for (int i=0; i < permissionsForAtLeastOneOrNoGroups.length; i++) {
            oneOrNoGroups.add(permissionsForAtLeastOneOrNoGroups[i]);
        }
        
        return oneOrNoGroups;
    }
    
    public List<String> getUsersWithPermission(String contextId, String permission) {
        if (contextId == null || permission == null) {
            throw new IllegalArgumentException("Null contextId (" + contextId + ") or permission" +
                    " ("+ permission + ") passed to getUsersWithPermission");
        }
        
        List<String> userIds = new ArrayList<String>();
        
        List<User> usersWithPerm = securityService.unlockUsers(permission, siteService.siteReference(contextId));
        if (usersWithPerm != null) {
            for (User user : usersWithPerm) {
                userIds.add(user.getId());
            }
        }
        
        return userIds;
    }

    
    /* Dependencies */

    public void setFunctionManager(FunctionManager functionManager) {
        this.functionManager = functionManager;
    }
    
    public void setSecurityService(SecurityService securityService) {
        this.securityService = securityService;
    }
    
    public void setSiteService(SiteService siteService) {
        this.siteService = siteService;
    }
}