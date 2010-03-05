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

package org.sakaiproject.assignment2.logic.test.stubs;

import java.util.ArrayList;
import java.util.List;

import org.sakaiproject.assignment2.logic.AssignmentAuthzLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.test.AssignmentTestDataLoad;


/**
 * Stub class used for answering security questions for assignment2 testing
 */
public class AssignmentAuthzLogicStub implements AssignmentAuthzLogic
{

    
    public boolean userHasAddPermission(String userId, String contextId) {
        return userHasPermission(userId, contextId, AssignmentConstants.PERMISSION_ADD_ASSIGNMENTS);
    }
    
    public boolean userHasEditPermission(String userId, String contextId) {
        return userHasPermission(userId, contextId, AssignmentConstants.PERMISSION_EDIT_ASSIGNMENTS);
    }
    
    public boolean userHasDeletePermission(String userId, String contextId) {
        return userHasPermission(userId, contextId, AssignmentConstants.PERMISSION_REMOVE_ASSIGNMENTS);
    }
    
    public boolean userHasAllGroupsPermission(String userId, String contextId) {
        return userHasPermission(userId, contextId, AssignmentConstants.PERMISSION_ALL_GROUPS);
    }
    
    public boolean userHasSubmitPermission(String userId, String contextId) {
        return userHasPermission(userId, contextId, AssignmentConstants.PERMISSION_SUBMIT);
    }
    
    public boolean userHasManageSubmissionsPermission(String userId, String contextId)
    {
        return userHasPermission(userId, contextId, AssignmentConstants.PERMISSION_MANAGE_SUBMISSIONS);
    }
    
    public boolean userHasViewAssignmentPermission(String userId, String contextId)
    {
        return userHasPermission(userId, contextId, AssignmentConstants.PERMISSION_VIEW_ASSIGNMENTS);
    }
    
    public boolean userHasPermission(String contextId, String permission) {
        return unlock(externalLogic.getCurrentUserId(), permission);
    }
    
    public boolean userHasPermission(String userId, String contextId, String permission) {
        return unlock(userId, permission);
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
    
    private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }
    
    
    private boolean unlock(String userId, String permission) {
        if (userId == null) {
            userId = externalLogic.getCurrentUserId();
        }
        if (userId.equals(AssignmentTestDataLoad.INSTRUCTOR_UID)) {
            if (permission.equals(AssignmentConstants.PERMISSION_SUBMIT)) {
                return false;
            } else {
                return true;
            }
        } else if (userId.equals(AssignmentTestDataLoad.STUDENT1_UID) || 
                userId.equals(AssignmentTestDataLoad.STUDENT2_UID) ||
                userId.equals(AssignmentTestDataLoad.STUDENT3_UID)) {
            if (permission.equals(AssignmentConstants.PERMISSION_SUBMIT) || 
                    permission.equals(AssignmentConstants.PERMISSION_VIEW_ASSIGNMENTS)) {
                return true;
            } else {
                return false;
            }
        } else if (userId.equals(AssignmentTestDataLoad.TA_UID)) {
            if (permission.equals(AssignmentConstants.PERMISSION_SUBMIT) ||
                    permission.equals(AssignmentConstants.PERMISSION_ALL_GROUPS)) {
                return false;
            } else {
                return true;
            }
        }
        
        return false;
    }

    public List<String> getUsersWithPermission(String contextId, String permission)
    {
        List<String> allUsers = new ArrayList<String>();
        allUsers.add(AssignmentTestDataLoad.INSTRUCTOR_UID);
        allUsers.add(AssignmentTestDataLoad.TA_UID);
        allUsers.add(AssignmentTestDataLoad.STUDENT1_UID);
        allUsers.add(AssignmentTestDataLoad.STUDENT2_UID);
        allUsers.add(AssignmentTestDataLoad.STUDENT3_UID);
        
        List<String> usersWithPermission = new ArrayList<String>();
        for (String user : allUsers) {
            if (userHasPermission(user, AssignmentTestDataLoad.CONTEXT_ID, permission)) {
                usersWithPermission.add(user);
            }
        }
        
        return usersWithPermission;
    }

}