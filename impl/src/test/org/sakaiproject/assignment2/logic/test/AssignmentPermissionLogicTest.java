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
package org.sakaiproject.assignment2.logic.test;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.test.Assignment2TestBase;
import org.sakaiproject.assignment2.test.AssignmentTestDataLoad;
import org.sakaiproject.site.api.Group;


public class AssignmentPermissionLogicTest extends Assignment2TestBase {

    /**
     * @see org.springframework.test.AbstractTransactionalSpringContextTests#onSetUpInTransaction()
     */
    protected void onSetUpInTransaction() throws Exception {
        super.onSetUpInTransaction();
        
        // refresh the assign data before you begin the tests.  sometimes it gets cranky if you don't
        dao.evictObject(testData.a1);
        testData.a1 = dao.getAssignmentByIdWithGroupsAndAttachments(testData.a1Id);
        
        dao.evictObject(testData.a2);
        testData.a2 = dao.getAssignmentByIdWithGroupsAndAttachments(testData.a2Id);
        
        dao.evictObject(testData.a3);
        testData.a3 = dao.getAssignmentByIdWithGroupsAndAttachments(testData.a3Id);
        
        dao.evictObject(testData.a4);
        testData.a4 = dao.getAssignmentByIdWithGroupsAndAttachments(testData.a4Id);
    }
    
    public void testGetPermissionsForSite() {
        try {
            permissionLogic.getPermissionsForSite(null, null);
            fail("Did not catch null contextId passed to getPermissionsForSite");
        } catch (IllegalArgumentException iae) {}
        
        List<String> allSitePerms = authz.getSiteLevelPermissions();
        
        // if permissions is null, double check that they are all returned
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        Map<String, Boolean> sitePerms = permissionLogic.getPermissionsForSite(AssignmentTestDataLoad.CONTEXT_ID, null);
        assertEquals(allSitePerms.size(), sitePerms.size());
        for (String perm : allSitePerms) {
            Boolean hasPerm = sitePerms.get(perm);
            if (perm.equals(AssignmentConstants.PERMISSION_ALL_GROUPS)) {
                assertTrue(hasPerm);
            } else {
                fail("Unknown permission returned from getPermissionsForSite");
            }
        }
        
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        sitePerms = permissionLogic.getPermissionsForSite(AssignmentTestDataLoad.CONTEXT_ID, null);
        assertEquals(allSitePerms.size(), sitePerms.size());
        for (String perm : allSitePerms) {
            Boolean hasPerm = sitePerms.get(perm);
            if (perm.equals(AssignmentConstants.PERMISSION_ALL_GROUPS)) {
                assertFalse(hasPerm);
            } else {
                fail("Unknown permission returned from getPermissionsForSite");
            }
        }
        
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        sitePerms = permissionLogic.getPermissionsForSite(AssignmentTestDataLoad.CONTEXT_ID, null);
        assertEquals(allSitePerms.size(), sitePerms.size());
        for (String perm : allSitePerms) {
            Boolean hasPerm = sitePerms.get(perm);
            if (perm.equals(AssignmentConstants.PERMISSION_ALL_GROUPS)) {
                assertFalse(hasPerm);
            } else {
                fail("Unknown permission returned from getPermissionsForSite");
            }
        }
        
        // now let's double check that it handles individual permission properly
        List<String> permsToCheck = new ArrayList<String>();
        permsToCheck.add(AssignmentConstants.PERMISSION_ALL_GROUPS);
        
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        sitePerms = permissionLogic.getPermissionsForSite(AssignmentTestDataLoad.CONTEXT_ID, permsToCheck);
        assertEquals(1, sitePerms.size());
        assertTrue(sitePerms.containsKey(AssignmentConstants.PERMISSION_ALL_GROUPS));
        assertTrue(sitePerms.get(AssignmentConstants.PERMISSION_ALL_GROUPS));
        
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        sitePerms = permissionLogic.getPermissionsForSite(AssignmentTestDataLoad.CONTEXT_ID, permsToCheck);
        assertEquals(1, sitePerms.size());
        assertTrue(sitePerms.containsKey(AssignmentConstants.PERMISSION_ALL_GROUPS));
        assertFalse(sitePerms.get(AssignmentConstants.PERMISSION_ALL_GROUPS));
        
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        sitePerms = permissionLogic.getPermissionsForSite(AssignmentTestDataLoad.CONTEXT_ID, permsToCheck);
        assertEquals(1, sitePerms.size());
        assertTrue(sitePerms.containsKey(AssignmentConstants.PERMISSION_ALL_GROUPS));
        assertFalse(sitePerms.get(AssignmentConstants.PERMISSION_ALL_GROUPS));
        
        // try perms that aren't site-level
        permsToCheck = authz.getAssignmentLevelPermissions();
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        sitePerms = permissionLogic.getPermissionsForSite(AssignmentTestDataLoad.CONTEXT_ID, permsToCheck);
        assertEquals(0, sitePerms.size());
        
    }
    
    public void testGetPermissionsForAssignments() {
        
        Map<Long, Map<String,Boolean>> assignPermsMap = permissionLogic.getPermissionsForAssignments(null, null);
        assertEquals(0, assignPermsMap.size());
        
        List<String> allAssignPerms = authz.getAssignmentLevelPermissions();
        
        List<Assignment2> allAssigns = new ArrayList<Assignment2>();
        allAssigns.add(testData.a1);
        allAssigns.add(testData.a2);
        allAssigns.add(testData.a3);
        allAssigns.add(testData.a4);
        
        // if permissions is null, double check that they are all returned
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assignPermsMap = permissionLogic.getPermissionsForAssignments(allAssigns, null);
        assertEquals(assignPermsMap.size(), 4);
        for (Long assignId : assignPermsMap.keySet()) {
            Map<String, Boolean> permMap = assignPermsMap.get(assignId);
            assertEquals(allAssignPerms.size(), permMap.size());
            for (String perm : permMap.keySet()) {
                Boolean hasPerm = permMap.get(perm);
                if (perm.equals(AssignmentConstants.PERMISSION_ADD_ASSIGNMENTS)) {
                    assertTrue(hasPerm);
                } else if (perm.equals(AssignmentConstants.PERMISSION_EDIT_ASSIGNMENTS)) {
                    assertTrue(hasPerm);
                } else if (perm.equals(AssignmentConstants.PERMISSION_MANAGE_SUBMISSIONS)) {
                    assertTrue(hasPerm);
                } else if (perm.equals(AssignmentConstants.PERMISSION_REMOVE_ASSIGNMENTS)) {
                    assertTrue(hasPerm);
                } else if (perm.equals(AssignmentConstants.PERMISSION_SUBMIT)) {
                    assertFalse(hasPerm);
                } else if (perm.equals(AssignmentConstants.PERMISSION_VIEW_ASSIGNMENTS)) {
                    assertTrue(hasPerm);
                } else {
                    fail("Unknown permission returned by getPermissionsForAssignments");
                }
            }
        }
        
        // try the TA. These permissions will depend on the actual assignment. TA's can't
        // add/edit/delete assignments that aren't solely associated with the user's groups.
        // They can manage submissions, though, if the assignment has at least one of their groups
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        assignPermsMap = permissionLogic.getPermissionsForAssignments(allAssigns, null);
        assertEquals(assignPermsMap.size(), 4);
        for (Long assignId : assignPermsMap.keySet()) {
            Map<String, Boolean> permMap = assignPermsMap.get(assignId);
            assertEquals(allAssignPerms.size(), permMap.size());
            for (String perm : permMap.keySet()) {
                Boolean hasPerm = permMap.get(perm);
                if (perm.equals(AssignmentConstants.PERMISSION_ADD_ASSIGNMENTS)) {
                    if (assignId.equals(testData.a1Id)) {
                        assertFalse(hasPerm);
                    } else if (assignId.equals(testData.a2Id)) {
                        assertFalse(hasPerm);
                    } else if (assignId.equals(testData.a3Id)) {
                        assertFalse(hasPerm);
                    } else if (assignId.equals(testData.a4Id)) {
                        assertFalse(hasPerm);
                    } else {
                        fail("Unknown assignment returned by getPermissionsForAssignments");
                    }
                } else if (perm.equals(AssignmentConstants.PERMISSION_EDIT_ASSIGNMENTS)) {
                    if (assignId.equals(testData.a1Id)) {
                        assertFalse(hasPerm);
                    } else if (assignId.equals(testData.a2Id)) {
                        assertFalse(hasPerm);
                    } else if (assignId.equals(testData.a3Id)) {
                        assertFalse(hasPerm);
                    } else if (assignId.equals(testData.a4Id)) {
                        assertFalse(hasPerm);
                    } else {
                        fail("Unknown assignment returned by getPermissionsForAssignments");
                    }
                } else if (perm.equals(AssignmentConstants.PERMISSION_MANAGE_SUBMISSIONS)) {
                    if (assignId.equals(testData.a1Id)) {
                        assertTrue(hasPerm);
                    } else if (assignId.equals(testData.a2Id)) {
                        assertFalse(hasPerm);
                    } else if (assignId.equals(testData.a3Id)) {
                        assertFalse(hasPerm);
                    } else if (assignId.equals(testData.a4Id)) {
                        assertFalse(hasPerm);
                    } else {
                        fail("Unknown assignment returned by getPermissionsForAssignments");
                    }
                } else if (perm.equals(AssignmentConstants.PERMISSION_REMOVE_ASSIGNMENTS)) {
                    if (assignId.equals(testData.a1Id)) {
                        assertFalse(hasPerm);
                    } else if (assignId.equals(testData.a2Id)) {
                        assertFalse(hasPerm);
                    } else if (assignId.equals(testData.a3Id)) {
                        assertFalse(hasPerm);
                    } else if (assignId.equals(testData.a4Id)) {
                        assertFalse(hasPerm);
                    } else {
                        fail("Unknown assignment returned by getPermissionsForAssignments");
                    }
                } else if (perm.equals(AssignmentConstants.PERMISSION_SUBMIT)) {
                    if (assignId.equals(testData.a1Id)) {
                        assertFalse(hasPerm);
                    } else if (assignId.equals(testData.a2Id)) {
                        assertFalse(hasPerm);
                    } else if (assignId.equals(testData.a3Id)) {
                        assertFalse(hasPerm);
                    } else if (assignId.equals(testData.a4Id)) {
                        assertFalse(hasPerm);
                    } else {
                        fail("Unknown assignment returned by getPermissionsForAssignments");
                    }
                } else if (perm.equals(AssignmentConstants.PERMISSION_VIEW_ASSIGNMENTS)) {
                    if (assignId.equals(testData.a1Id)) {
                        assertTrue(hasPerm);
                    } else if (assignId.equals(testData.a2Id)) {
                        assertTrue(hasPerm);
                    } else if (assignId.equals(testData.a3Id)) {
                        assertTrue(hasPerm);
                    } else if (assignId.equals(testData.a4Id)) {
                        assertFalse(hasPerm);
                    } else {
                        fail("Unknown assignment returned by getPermissionsForAssignments");
                    }
                } else {
                    fail("Unknown permission returned by getPermissionsForAssignments");
                }
            }
        }
        
        // student 1 is a member of group 1
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        assignPermsMap = permissionLogic.getPermissionsForAssignments(allAssigns, null);
        assertEquals(assignPermsMap.size(), 4);
        for (Long assignId : assignPermsMap.keySet()) {
            Map<String, Boolean> permMap = assignPermsMap.get(assignId);
            assertEquals(allAssignPerms.size(), permMap.size());
            for (String perm : permMap.keySet()) {
                Boolean hasPerm = permMap.get(perm);
                if (perm.equals(AssignmentConstants.PERMISSION_ADD_ASSIGNMENTS)) {                    
                    assertFalse(hasPerm);
                } else if (perm.equals(AssignmentConstants.PERMISSION_EDIT_ASSIGNMENTS)) {
                    assertFalse(hasPerm);
                } else if (perm.equals(AssignmentConstants.PERMISSION_MANAGE_SUBMISSIONS)) {
                    assertFalse(hasPerm);
                } else if (perm.equals(AssignmentConstants.PERMISSION_REMOVE_ASSIGNMENTS)) {
                    assertFalse(hasPerm);
                } else if (perm.equals(AssignmentConstants.PERMISSION_SUBMIT)) {
                    if (assignId.equals(testData.a1Id)) {
                        assertTrue(hasPerm); // restricted to my group
                    } else if (assignId.equals(testData.a2Id)) {
                        assertTrue(hasPerm); // no group restrictions
                    } else if (assignId.equals(testData.a3Id)) {
                        assertTrue(hasPerm); // no group restrictions
                    } else if (assignId.equals(testData.a4Id)) {
                        assertFalse(hasPerm);  // i'm not in the restricted group
                    } else {
                        fail("Unknown assignment returned by getPermissionsForAssignments");
                    }
                } else if (perm.equals(AssignmentConstants.PERMISSION_VIEW_ASSIGNMENTS)) {
                    if (assignId.equals(testData.a1Id)) {
                        assertTrue(hasPerm); // restricted to my group
                    } else if (assignId.equals(testData.a2Id)) {
                        assertTrue(hasPerm); // no group restrictions
                    } else if (assignId.equals(testData.a3Id)) {
                        assertTrue(hasPerm); // no group restrictions
                    } else if (assignId.equals(testData.a4Id)) {
                        assertFalse(hasPerm);  // i'm not in the restricted group
                    } else {
                        fail("Unknown assignment returned by getPermissionsForAssignments");
                    }
                } else {
                    fail("Unknown permission returned by getPermissionsForAssignments");
                }
            }
        }
        
        // student 2 is a member of group 3
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT2_UID);
        assignPermsMap = permissionLogic.getPermissionsForAssignments(allAssigns, null);
        assertEquals(assignPermsMap.size(), 4);
        for (Long assignId : assignPermsMap.keySet()) {
            Map<String, Boolean> permMap = assignPermsMap.get(assignId);
            assertEquals(allAssignPerms.size(), permMap.size());
            for (String perm : permMap.keySet()) {
                Boolean hasPerm = permMap.get(perm);
                if (perm.equals(AssignmentConstants.PERMISSION_ADD_ASSIGNMENTS)) {                    
                    assertFalse(hasPerm);
                } else if (perm.equals(AssignmentConstants.PERMISSION_EDIT_ASSIGNMENTS)) {
                    assertFalse(hasPerm);
                } else if (perm.equals(AssignmentConstants.PERMISSION_MANAGE_SUBMISSIONS)) {
                    assertFalse(hasPerm);
                } else if (perm.equals(AssignmentConstants.PERMISSION_REMOVE_ASSIGNMENTS)) {
                    assertFalse(hasPerm);
                } else if (perm.equals(AssignmentConstants.PERMISSION_SUBMIT)) {
                    if (assignId.equals(testData.a1Id)) { // restricted to group 1 and 3
                        assertTrue(hasPerm); // restricted to my group
                    } else if (assignId.equals(testData.a2Id)) {
                        assertTrue(hasPerm); // no group restrictions
                    } else if (assignId.equals(testData.a3Id)) {
                        assertTrue(hasPerm); // no group restrictions
                    } else if (assignId.equals(testData.a4Id)) {
                        assertTrue(hasPerm);  // restricted to my group
                    } else {
                        fail("Unknown assignment returned by getPermissionsForAssignments");
                    }
                } else if (perm.equals(AssignmentConstants.PERMISSION_VIEW_ASSIGNMENTS)) {
                    if (assignId.equals(testData.a1Id)) {
                        assertTrue(hasPerm); // restricted to my group
                    } else if (assignId.equals(testData.a2Id)) {
                        assertTrue(hasPerm); // no group restrictions
                    } else if (assignId.equals(testData.a3Id)) {
                        assertTrue(hasPerm); // no group restrictions
                    } else if (assignId.equals(testData.a4Id)) {
                        assertTrue(hasPerm);  // restricted to my group
                    } else {
                        fail("Unknown assignment returned by getPermissionsForAssignments");
                    }
                } else {
                    fail("Unknown permission returned by getPermissionsForAssignments");
                }
            }
        }
        
        // student 3 is not in any groups
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT3_UID);
        assignPermsMap = permissionLogic.getPermissionsForAssignments(allAssigns, null);
        assertEquals(assignPermsMap.size(), 4);
        for (Long assignId : assignPermsMap.keySet()) {
            Map<String, Boolean> permMap = assignPermsMap.get(assignId);
            assertEquals(allAssignPerms.size(), permMap.size());
            for (String perm : permMap.keySet()) {
                Boolean hasPerm = permMap.get(perm);
                if (perm.equals(AssignmentConstants.PERMISSION_ADD_ASSIGNMENTS)) {                    
                    assertFalse(hasPerm);
                } else if (perm.equals(AssignmentConstants.PERMISSION_EDIT_ASSIGNMENTS)) {
                    assertFalse(hasPerm);
                } else if (perm.equals(AssignmentConstants.PERMISSION_MANAGE_SUBMISSIONS)) {
                    assertFalse(hasPerm);
                } else if (perm.equals(AssignmentConstants.PERMISSION_REMOVE_ASSIGNMENTS)) {
                    assertFalse(hasPerm);
                } else if (perm.equals(AssignmentConstants.PERMISSION_SUBMIT)) {
                    if (assignId.equals(testData.a1Id)) {
                        assertFalse(hasPerm); // i'm not in the restricted group
                    } else if (assignId.equals(testData.a2Id)) {
                        assertTrue(hasPerm); // no group restrictions
                    } else if (assignId.equals(testData.a3Id)) {
                        assertTrue(hasPerm); // no group restrictions
                    } else if (assignId.equals(testData.a4Id)) {
                        assertFalse(hasPerm);  // i'm not in the restricted group
                    } else {
                        fail("Unknown assignment returned by getPermissionsForAssignments");
                    }
                } else if (perm.equals(AssignmentConstants.PERMISSION_VIEW_ASSIGNMENTS)) {
                    if (assignId.equals(testData.a1Id)) {
                        assertFalse(hasPerm); // i'm not in the restricted group
                    } else if (assignId.equals(testData.a2Id)) {
                        assertTrue(hasPerm); // no group restrictions
                    } else if (assignId.equals(testData.a3Id)) {
                        assertTrue(hasPerm); // no group restrictions
                    } else if (assignId.equals(testData.a4Id)) {
                        assertFalse(hasPerm);  // i'm not in the restricted group
                    } else {
                        fail("Unknown assignment returned by getPermissionsForAssignments");
                    }
                } else {
                    fail("Unknown permission returned by getPermissionsForAssignments");
                }
            }
        }
        
    }

    public void testIsUserAllowedToEditAssignment() {
        // try passing a null assignment
        try {
            permissionLogic.isUserAllowedToEditAssignment(null);
            fail("did not catch null assignment passed to isUserAllowedToEditAssignment");
        } catch (IllegalArgumentException iae) {}

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);

        // only instructors should be able to edit assignments
        assertTrue(permissionLogic.isUserAllowedToEditAssignment(testData.a1));

        // TAs can only edit assignments that are only restricted to their own group(s)
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        assertFalse(permissionLogic.isUserAllowedToEditAssignment(testData.a1));
        // assignment2 doesn't have groups, so TA shouldn't have permission
        assertFalse(permissionLogic.isUserAllowedToEditAssignment(testData.a2));

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        assertFalse(permissionLogic.isUserAllowedToEditAssignment(testData.a1));
    }
    
    public void testIsUserAllowedToEditAllAssignments() {
        try {
            permissionLogic.isUserAllowedToEditAllAssignments(null);
            fail("Did not catch null contextId passed to isUserAllowedToEditAllAssignments");
        } catch (IllegalArgumentException iae) {}
        
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assertTrue(permissionLogic.isUserAllowedToEditAllAssignments(AssignmentTestDataLoad.CONTEXT_ID));
        
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        assertFalse(permissionLogic.isUserAllowedToEditAllAssignments(AssignmentTestDataLoad.CONTEXT_ID));
        
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        assertFalse(permissionLogic.isUserAllowedToEditAllAssignments(AssignmentTestDataLoad.CONTEXT_ID));
    }
    
    public void testIsUserAllowedToAddAssignments() {
        // try passing a null contextId
        try {
            permissionLogic.isUserAllowedToAddAssignments(null);
            fail("did not catch null contextId passed to isUserAllowedToAddAssignments");
        } catch (IllegalArgumentException iae) {}

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assertTrue(permissionLogic.isUserAllowedToAddAssignments(AssignmentTestDataLoad.CONTEXT_ID));

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        assertTrue(permissionLogic.isUserAllowedToAddAssignments(AssignmentTestDataLoad.CONTEXT_ID));

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        assertFalse(permissionLogic.isUserAllowedToAddAssignments(AssignmentTestDataLoad.CONTEXT_ID));
    }
    
    public void testIsUserAllowedToAddAssignment() {
        // try passing a null contextId
        try {
            permissionLogic.isUserAllowedToAddAssignment(null);
            fail("did not catch null assignment passed to isUserAllowedToAddAssignment");
        } catch (IllegalArgumentException iae) {}

        // test out all of the group restriction scenarios
        Assignment2 assignWithNoGroups = new Assignment2();
        
        Assignment2 assignWithExtraGroups = new Assignment2();
        Set<AssignmentGroup> groups1and2 = new HashSet<AssignmentGroup>();
        groups1and2.add(new AssignmentGroup(assignWithExtraGroups, AssignmentTestDataLoad.GROUP1_NAME));
        groups1and2.add(new AssignmentGroup(assignWithExtraGroups, AssignmentTestDataLoad.GROUP2_NAME));
        assignWithExtraGroups.setAssignmentGroupSet(groups1and2);
        
        Assignment2 assignWithGroup1 = new Assignment2();
        Set<AssignmentGroup> group1 = new HashSet<AssignmentGroup>();
        group1.add(new AssignmentGroup(assignWithExtraGroups, AssignmentTestDataLoad.GROUP1_NAME));
        assignWithGroup1.setAssignmentGroupSet(group1);
        
        // instructors should have access to everything
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assertTrue(permissionLogic.isUserAllowedToAddAssignment(assignWithNoGroups));
        assertTrue(permissionLogic.isUserAllowedToAddAssignment(assignWithExtraGroups));
        assertTrue(permissionLogic.isUserAllowedToAddAssignment(assignWithGroup1));
        
        // TAs should only be able to add an assignment restricted to only group 1
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        assertFalse(permissionLogic.isUserAllowedToAddAssignment(assignWithNoGroups));
        assertFalse(permissionLogic.isUserAllowedToAddAssignment(assignWithExtraGroups));
        assertTrue(permissionLogic.isUserAllowedToAddAssignment(assignWithGroup1));
        
        // students can't add at all
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        assertFalse(permissionLogic.isUserAllowedToAddAssignment(assignWithNoGroups));
        assertFalse(permissionLogic.isUserAllowedToAddAssignment(assignWithExtraGroups));
        assertFalse(permissionLogic.isUserAllowedToAddAssignment(assignWithGroup1));
    }
    
    public void testIsUserAllowedToDeleteAssignment() {
        // try passing a null assignment
        try {
            permissionLogic.isUserAllowedToDeleteAssignment(null);
            fail("did not catch null assignment passed to isUserAllowedToDeleteAssignment");
        } catch (IllegalArgumentException iae) {}

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assertTrue(permissionLogic.isUserAllowedToDeleteAssignment(testData.a1));

        // TAs can only delete assignments that are only available to his/her group(s)
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        assertFalse(permissionLogic.isUserAllowedToDeleteAssignment(testData.a1));
        // assignment2 doesn't have groups, so TA shouldn't have permission
        assertFalse(permissionLogic.isUserAllowedToDeleteAssignment(testData.a2));

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        assertFalse(permissionLogic.isUserAllowedToDeleteAssignment(testData.a1));
    }

    public void testIsUserAbleToViewStudentSubmissionForAssignment() {
        // try passing a null studentId
        try {
            permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(null, 12345L);
            fail("Did not catch null student passed to isUserAbleToViewStudentSubmissionForAssignment");
        } catch (IllegalArgumentException iae) {}

        // try passing a null assignment
        try {
            permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(AssignmentTestDataLoad.STUDENT1_UID, null);
            fail("Did not catch null assignment passed to isUserAbleToViewStudentSubmissionForAssignment");
        } catch (IllegalArgumentException iae) {}

        // Assignment 1 restricted to group 1 and group 2
        // Assignment 2 has no restrictions
        // Assignment 3 has no restrictions
        // Assignment 4 restricted to group 3

        // ta in group 1
        // student1 member of group 1
        // student 2 member of group 3
        // student 3 not in a group

        // let's start with an ungraded assignment 
        // instructor should be able to view any student
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
                AssignmentTestDataLoad.STUDENT1_UID, testData.a2Id));
        assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
                AssignmentTestDataLoad.STUDENT2_UID, testData.a2Id));
        assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
                AssignmentTestDataLoad.STUDENT3_UID, testData.a2Id));
        // switch to TA
        // ta may only view members in his/her group
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
                AssignmentTestDataLoad.STUDENT1_UID, testData.a2Id));
        assertFalse(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
                AssignmentTestDataLoad.STUDENT2_UID, testData.a2Id));
        assertFalse(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
                AssignmentTestDataLoad.STUDENT3_UID, testData.a2Id));

        // now consider a graded assignment. with no grader perms, the same rules
        // as above apply
        // instructor should be able to view any student
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
                AssignmentTestDataLoad.STUDENT1_UID, testData.a3Id));
        assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
                AssignmentTestDataLoad.STUDENT2_UID, testData.a3Id));
        assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
                AssignmentTestDataLoad.STUDENT3_UID, testData.a3Id));
        // switch to TA
        // ta may only view members in his/her group
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
                AssignmentTestDataLoad.STUDENT1_UID, testData.a3Id));
        assertFalse(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
                AssignmentTestDataLoad.STUDENT2_UID, testData.a3Id));
        assertFalse(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
                AssignmentTestDataLoad.STUDENT3_UID, testData.a3Id));

        // TODO - check grader permissions!!

        // user may view their own submission
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(AssignmentTestDataLoad.STUDENT1_UID, testData.a1Id));
        // but they may not view others
        assertFalse(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(AssignmentTestDataLoad.STUDENT2_UID, testData.a1Id));
    }

    public void testIsUserAbleToProvideFeedbackForStudentForAssignment() {
        // pass null studentId
        try {
            permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(null, new Assignment2());
            fail("did not catch null studentId passed to isUserAbleToProvideFeedbackForStudentForAssignment");
        } catch(IllegalArgumentException iae) {}
        // pass null assignment
        try {
            permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(AssignmentTestDataLoad.STUDENT1_UID, null);
            fail("did not catch null assignment passed to isUserAbleToProvideFeedbackForStudentForAssignment");
        } catch(IllegalArgumentException iae) {}

        // start with an ungraded item
        // instructor should be able to submit feedback for any student
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assertTrue(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
                AssignmentTestDataLoad.STUDENT1_UID, testData.a2));
        assertTrue(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
                AssignmentTestDataLoad.STUDENT2_UID, testData.a2));
        assertTrue(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
                AssignmentTestDataLoad.STUDENT3_UID, testData.a2));
        // switch to TA
        // ta may only submit feedback for students in his/her group
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        assertTrue(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
                AssignmentTestDataLoad.STUDENT1_UID, testData.a2));
        assertFalse(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
                AssignmentTestDataLoad.STUDENT2_UID, testData.a2));
        assertFalse(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
                AssignmentTestDataLoad.STUDENT3_UID, testData.a2));

        // now consider a graded assignment. with no grader perms, the same rules
        // as above apply
        // instructor should be able to submit feedback for any student
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assertTrue(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
                AssignmentTestDataLoad.STUDENT1_UID, testData.a3));
        assertTrue(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
                AssignmentTestDataLoad.STUDENT2_UID, testData.a3));
        assertTrue(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
                AssignmentTestDataLoad.STUDENT3_UID, testData.a3));
        // switch to TA
        // ta may only submit feedback for members in his/her group
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        assertTrue(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
                AssignmentTestDataLoad.STUDENT1_UID, testData.a3));
        assertFalse(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
                AssignmentTestDataLoad.STUDENT2_UID, testData.a3));
        assertFalse(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
                AssignmentTestDataLoad.STUDENT3_UID, testData.a3));

        // TODO check a gb assignment with grader perms. use one with View only perm

        // students should not be able to submit feedback at all
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        assertFalse(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(AssignmentTestDataLoad.STUDENT1_UID, testData.a1));
        assertFalse(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(AssignmentTestDataLoad.STUDENT2_UID, testData.a1));
    }

    public void testIsUserAbleToProvideFeedbackForSubmission() {
        // pass null submissionId
        try {
            permissionLogic.isUserAbleToProvideFeedbackForSubmission(null);
            fail("did not catch null submissionId passed to isUserAbleToProvideFeedbackForSubmission");
        } catch(IllegalArgumentException iae) {}

        // start with an ungraded item
        // instructor should be able to submit feedback for any student
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assertTrue(permissionLogic.isUserAbleToProvideFeedbackForSubmission(
                testData.st1a1Submission.getId()));
        assertTrue(permissionLogic.isUserAbleToProvideFeedbackForSubmission(
                testData.st2a1Submission.getId()));

        // switch to TA
        // ta may only submit feedback for students in his/her Group
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        assertTrue(permissionLogic.isUserAbleToProvideFeedbackForSubmission(
                testData.st1a1Submission.getId()));
        assertFalse(permissionLogic.isUserAbleToProvideFeedbackForSubmission(
                testData.st2a1Submission.getId()));

        // now consider a graded assignment. with no grader perms, the same rules
        // as above apply
        // instructor should be able to submit feedback for any student
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assertTrue(permissionLogic.isUserAbleToProvideFeedbackForSubmission(
                testData.st1a3Submission.getId()));
        assertTrue(permissionLogic.isUserAbleToProvideFeedbackForSubmission(
                testData.st2a3Submission.getId()));

        // switch to TA
        // ta may only submit feedback for members in his/her group
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        assertTrue(permissionLogic.isUserAbleToProvideFeedbackForSubmission(
                testData.st1a3Submission.getId()));
        assertFalse(permissionLogic.isUserAbleToProvideFeedbackForSubmission(
                testData.st2a3Submission.getId()));

        // TODO check a gb assignment with grader perms. use one with View only perm

        // students should not be able to submit feedback at all
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        assertFalse(permissionLogic.isUserAbleToProvideFeedbackForSubmission(testData.st1a1Submission.getId()));
        assertFalse(permissionLogic.isUserAbleToProvideFeedbackForSubmission(testData.st2a1Submission.getId()));
    }



    public void testFilterViewableAssignments() {
        // try passing a null contextId
        try {
            permissionLogic.filterViewableAssignments(null, new ArrayList<Assignment2>());
            fail("Did not catch null contextId passed to filterViewableAssignments");
        } catch (IllegalArgumentException iae) {}

        // try passing an assignment with a diff contextId
        Assignment2 assign = new Assignment2();
        assign.setContextId(AssignmentTestDataLoad.BAD_CONTEXT);
        List<Assignment2> assignList = new ArrayList<Assignment2>();
        assignList.add(assign);
        try {
            permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
            fail("did not catch assignment in the list with a different contextId");
        } catch (IllegalArgumentException iae) {}

        // instructors should be able to view all assignments 
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        
        assignList = new ArrayList<Assignment2>();
        assignList.add(testData.a1);
        assignList.add(testData.a2);

        List<Assignment2> filteredAssigns = permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(2, filteredAssigns.size());


        // TA should only be able to see assignments that he/she is a member of if restricted
        // otherwise, should see all

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        // a1 is restricted to a group that ta is a member of
        // a2 is not restricted, so should be ok
        filteredAssigns = permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(2, filteredAssigns.size());

        // Students will see assignments available to site and those available to groups they
        // are a member of
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        // a1: student is a member of a restricted group, so ok
        // a2: is not restricted, so should be ok
        filteredAssigns = permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(2, filteredAssigns.size());

        // student 3 should only have access to assign 2 b/c of group memberships
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT3_UID);
        filteredAssigns = permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(1, filteredAssigns.size());
        assertEquals(testData.a2Id, filteredAssigns.get(0).getId());

        // let's set the open date to the future.  student shouldn't be able to view anymore
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        Calendar cal = Calendar.getInstance();
        cal.set(2025, 10, 01);

        testData.a1.setOpenDate(cal.getTime());
        filteredAssigns = permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(1, filteredAssigns.size());
        assertEquals(testData.a2Id, filteredAssigns.get(0).getId());

        // let's set the assignment to draft. only instructor should be able to view
        testData.a1.setDraft(true);
        // try student
        filteredAssigns = permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(1, filteredAssigns.size());
        assertEquals(testData.a2Id, filteredAssigns.get(0).getId());

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        filteredAssigns = permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(1, filteredAssigns.size());
        assertEquals(testData.a2Id, filteredAssigns.get(0).getId());

        // instructor should be allowed
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        filteredAssigns = permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(2, filteredAssigns.size());

        // let's try out some graded assignment logic

        // gb item 1 is not released - assoc with a3
        // a4 is restricted to group 3

        // instructors should be able to view all assignments 
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assignList = new ArrayList<Assignment2>();
        assignList.add(testData.a3);
        assignList.add(testData.a4);
        filteredAssigns = permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(2, filteredAssigns.size());

        // TAs can view all gb items since there are no grader perms, but he/she can't view
        // assignments that are restricted to groups they are not in. ta can't view assign 4

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        filteredAssigns = permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(1, filteredAssigns.size());
        assertEquals(testData.a3Id, filteredAssigns.get(0).getId());

        // Students will see assignments available to site and those available to groups they
        // are a member of. assign must be open
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        // a4: student is not a member of the restricted group, so cannot view
        // a3: this gb item hasn't been released yet, but the student can still view it
        filteredAssigns = permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(1, filteredAssigns.size());
        assertEquals(testData.a3Id, filteredAssigns.get(0).getId());

        // switch to student who is a member of group 3
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT2_UID);
        filteredAssigns = permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(2, filteredAssigns.size());

        // let's set the open date to the future.  student shouldn't be able to view anymore
        cal.set(2025, 10, 01);

        testData.a4.setOpenDate(cal.getTime());
        filteredAssigns = permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(1, filteredAssigns.size());
        assertEquals(testData.a3Id, filteredAssigns.get(0).getId());

        // now set a1 as removed and remove the group restrictions to test this. 
        // only a student w/ a submission should be able to view. st1 has a submission
        // but st3 does not
        testData.a4.setRemoved(true);
        testData.a4.setAssignmentGroupSet(new HashSet<AssignmentGroup>());

        assignList = new ArrayList<Assignment2>();
        assignList.add(testData.a4);

        // instructor
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        filteredAssigns = permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(0, filteredAssigns.size());

        // ta
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        filteredAssigns = permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(0, filteredAssigns.size());

        // student w/o sub
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        filteredAssigns = permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(0, filteredAssigns.size());

        // student with sub
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT2_UID);
        filteredAssigns = permissionLogic.filterViewableAssignments(AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(1, filteredAssigns.size());

        testData.a1.setDraft(false);
        testData.a1.setOpenDate(new Date());
        testData.a4.setRemoved(false);
    }

    public void testIsUserAbleToViewAssignment() {

        // try passing a null assignmentId
        try {
            permissionLogic.isUserAbleToViewAssignment(null);
            fail("Did not catch null assignmentId passed to isUserAbleToViewAssignment");
        } catch (IllegalArgumentException iae) {}

        // first try ungraded assignments

        // instructors should be able to view all assignments 
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assertTrue(permissionLogic.isUserAbleToViewAssignment(testData.a1Id));
        assertTrue(permissionLogic.isUserAbleToViewAssignment(testData.a2Id));

        // TA should only be able to see assignments that he/she is a member of if restricted
        // otherwise, should see all

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);   
        // try one that is restricted to a group that ta is a member of
        assertTrue(permissionLogic.isUserAbleToViewAssignment(testData.a1Id));
        // this one is not restricted, so should be ok
        assertTrue(permissionLogic.isUserAbleToViewAssignment(testData.a2Id));

        // Students will see assignments available to site and those available to groups they
        // are a member of
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        // student is a member of a restricted group, so ok
        assertTrue(permissionLogic.isUserAbleToViewAssignment(testData.a1Id));
        // this one is not restricted, so should be ok
        assertTrue(permissionLogic.isUserAbleToViewAssignment(testData.a2Id));

        // let's set the open date to the future.  student shouldn't be able to view anymore
        Calendar cal = Calendar.getInstance();
        cal.set(2025, 10, 01);

        // re-retrieve this assignment
        testData.a1 = (Assignment2)dao.findById(Assignment2.class, testData.a1Id);
        testData.a1.setOpenDate(cal.getTime());
        dao.save(testData.a1);
        assertFalse(permissionLogic.isUserAbleToViewAssignment(testData.a1Id));

        // now test some graded assignments
        // gb item 1 is not released - assoc with a3
        // a4 is restricted to group 3

        // instructors should be able to view all assignments 
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assertTrue(permissionLogic.isUserAbleToViewAssignment(testData.a3Id));
        assertTrue(permissionLogic.isUserAbleToViewAssignment(testData.a4Id));

        // TAs can view all since there are no grader perms

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        assertTrue(permissionLogic.isUserAbleToViewAssignment(testData.a3Id));
        assertFalse(permissionLogic.isUserAbleToViewAssignment(testData.a4Id));

        // Students will see assignments available to site and those available to groups they
        // are a member of. assign must be open
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        // student is not a member of the restricted group, so cannot view
        assertFalse(permissionLogic.isUserAbleToViewAssignment(testData.a4Id));
        // this gb item hasn't been released yet, but student may still view it
        assertTrue(permissionLogic.isUserAbleToViewAssignment(testData.a3Id));

        // switch to student who is a member of group 3
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT2_UID);
        assertTrue(permissionLogic.isUserAbleToViewAssignment(testData.a4Id));
    }

    public void testIsUserAMemberOfARestrictedGroup() {
        List<String> groupMembershipIds = null;
        List<AssignmentGroup> assignmentGroupSet = null;
        // try with both null
        assertFalse(permissionLogic.isUserAMemberOfARestrictedGroup(groupMembershipIds, assignmentGroupSet));

        // add a group to groupMembershipIds - should still be false
        groupMembershipIds = new ArrayList<String>();
        groupMembershipIds.add(AssignmentTestDataLoad.GROUP1_NAME);
        assertFalse(permissionLogic.isUserAMemberOfARestrictedGroup(groupMembershipIds, assignmentGroupSet));

        // add a different AssignmentGroup to the assignmentGroups
        assignmentGroupSet = new ArrayList<AssignmentGroup>();
        assignmentGroupSet.add(new AssignmentGroup(null, AssignmentTestDataLoad.GROUP2_NAME));
        assertFalse(permissionLogic.isUserAMemberOfARestrictedGroup(groupMembershipIds, assignmentGroupSet));

        // now add an overlapping group to group membership
        groupMembershipIds.add(AssignmentTestDataLoad.GROUP2_NAME);
        assertTrue(permissionLogic.isUserAMemberOfARestrictedGroup(groupMembershipIds, assignmentGroupSet));

    }

    public void testIsUserAbleToAccessInstructorView() {
        // pass in a null contextId
        try {
            permissionLogic.isUserAbleToAccessInstructorView(null);
            fail("Did not catch null contextId passed to isUserAbleToAccessInstructorView");
        } catch (IllegalArgumentException iae) {

        }
        // only instructors and tas should have access to the non-student view
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assertTrue(permissionLogic.isUserAbleToAccessInstructorView(AssignmentTestDataLoad.CONTEXT_ID));

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        assertTrue(permissionLogic.isUserAbleToAccessInstructorView(AssignmentTestDataLoad.CONTEXT_ID));

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        assertFalse(permissionLogic.isUserAbleToAccessInstructorView(AssignmentTestDataLoad.CONTEXT_ID));
    }

    public void testGetViewableStudentsForUserForItem() {
        // try a null userI
        try {
            permissionLogic.getViewableStudentsForUserForItem(null, testData.a1);
            fail("did not catch null assignment passed to getViewableStudentsForUserForItem");
        } catch(IllegalArgumentException iae) {}

        // try a null assignment
        try {
            permissionLogic.getViewableStudentsForUserForItem(AssignmentTestDataLoad.STUDENT1_UID, null);
            fail("did not catch null assignment passed to getViewableStudentsForUserForItem");
        } catch(IllegalArgumentException iae) {}

        // this method should return 0 if a student calls it

        List<String> viewableStudents = permissionLogic.getViewableStudentsForUserForItem(AssignmentTestDataLoad.STUDENT1_UID, testData.a1);
        assertEquals(0, viewableStudents.size());

        // Let's start with an ungraded item
        // instructor should get all students who have the assignment
        // a1 is restricted to groups, so will return all students in those groups
        viewableStudents = permissionLogic.getViewableStudentsForUserForItem(AssignmentTestDataLoad.INSTRUCTOR_UID, testData.a1);
        assertEquals(2, viewableStudents.size());
        // this one is not restricted
        viewableStudents = permissionLogic.getViewableStudentsForUserForItem(AssignmentTestDataLoad.INSTRUCTOR_UID, testData.a2);
        assertEquals(3, viewableStudents.size());

        // the ta should have restrictions on a1
        // should only get student 1 b/c may only see students in his/her group
        viewableStudents = permissionLogic.getViewableStudentsForUserForItem(AssignmentTestDataLoad.TA_UID, testData.a1);
        assertEquals(1, viewableStudents.size());
        // should still get 1 for a2
        viewableStudents = permissionLogic.getViewableStudentsForUserForItem(AssignmentTestDataLoad.TA_UID, testData.a2);
        assertEquals(1, viewableStudents.size());
        // let's add a group restriction to a2 and make sure no students are returned
        AssignmentGroup groupFora2 = new AssignmentGroup(testData.a2, AssignmentTestDataLoad.GROUP3_NAME);
        dao.save(groupFora2);
        // shouldn't get any student back now
        viewableStudents = permissionLogic.getViewableStudentsForUserForItem(AssignmentTestDataLoad.TA_UID, testData.a2);
        assertEquals(0, viewableStudents.size());

        // now we will consider a graded item
        // switch back to instructor
        // a3 is not restricted, so will return all students
        viewableStudents = permissionLogic.getViewableStudentsForUserForItem(AssignmentTestDataLoad.INSTRUCTOR_UID, testData.a3);
        assertEquals(3, viewableStudents.size());
        // a4 is restricted to group 3
        viewableStudents = permissionLogic.getViewableStudentsForUserForItem(AssignmentTestDataLoad.INSTRUCTOR_UID, testData.a4);
        assertEquals(1, viewableStudents.size());

        // now switch to the ta
        // TODO - GRADER PERMISSIONS!!
        // a3 should return all students in ta's groups
        viewableStudents = permissionLogic.getViewableStudentsForUserForItem(AssignmentTestDataLoad.TA_UID, testData.a3);
        assertEquals(1, viewableStudents.size());
        // a4 should not return any
        viewableStudents = permissionLogic.getViewableStudentsForUserForItem(AssignmentTestDataLoad.TA_UID, testData.a4);
        assertTrue(viewableStudents.isEmpty());
    }

    public void testGetViewableStudentsForUserForAssignments() {
        // try a null userId
        try {
            List<Assignment2> assignList = new ArrayList<Assignment2>();
            assignList.add(testData.a1);
            permissionLogic.getViewableStudentsForUserForAssignments(null, AssignmentTestDataLoad.CONTEXT_ID, assignList);
            fail("did not catch null assignment passed to getViewableStudentsForUserForAssignments");
        } catch(IllegalArgumentException iae) {}

        // try a null contextId
        try {
            permissionLogic.getViewableStudentsForUserForAssignments(AssignmentTestDataLoad.TA_UID, null, new ArrayList<Assignment2>());
            fail("Did not catch null contextId passed to getViewableStudentsForUserForAssignments");
        } catch (IllegalArgumentException iae) {}

        // try a null assignment list - should return an empty map
        Map<Assignment2, List<String>> assignmentToStudentListMap = 
            permissionLogic.getViewableStudentsForUserForAssignments(AssignmentTestDataLoad.STUDENT1_UID, AssignmentTestDataLoad.CONTEXT_ID, null);

        assertEquals(0, assignmentToStudentListMap.size());

        // this method should return all assignments with empty lists
        List<Assignment2> assignList = new ArrayList<Assignment2>();
        assignList.add(testData.a1);
        assignList.add(testData.a2);
        assignList.add(testData.a3);
        assignList.add(testData.a4);
        assignmentToStudentListMap = permissionLogic.getViewableStudentsForUserForAssignments(AssignmentTestDataLoad.STUDENT1_UID, AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(4, assignmentToStudentListMap.size());
        for (Map.Entry<Assignment2, List<String>> entry : assignmentToStudentListMap.entrySet()) {
            Assignment2 assign = entry.getKey();
            List<String> viewableStudents = entry.getValue();
            if (assign.equals(testData.a1)) {
                assertEquals(0, viewableStudents.size());
            } else if (assign.equals(testData.a2)) {
                assertEquals(0, viewableStudents.size());
            } else if (assign.equals(testData.a3)) {
                assertEquals(0, viewableStudents.size());
            } else if (assign.equals(testData.a4)) {  
                assertEquals(0, viewableStudents.size());
            } else {
                fail("Unknown assignment returned by getViewableStudentsForUserForAssignments");
            }
        }

        // Let's start with ungraded items
        // instructor should get all students who have the assignment
        // a1 is restricted to groups, so will return all students in those groups. a2 is not restricted
        // a3 (graded) is not restricted, so will return all students
        // a4 (graded) is restricted to group 3
        assignmentToStudentListMap = permissionLogic.getViewableStudentsForUserForAssignments(AssignmentTestDataLoad.INSTRUCTOR_UID, AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(4, assignmentToStudentListMap.size());
        for (Map.Entry<Assignment2, List<String>> entry : assignmentToStudentListMap.entrySet()) {
            Assignment2 assign = entry.getKey();
            List<String> viewableStudents = entry.getValue();
            if (assign.equals(testData.a1)) {
                assertEquals(2, viewableStudents.size());
            } else if (assign.equals(testData.a2)) {
                assertEquals(3, viewableStudents.size());
            } else if (assign.equals(testData.a3)) {
                assertEquals(3, viewableStudents.size());
            } else if (assign.equals(testData.a4)) {  
                assertEquals(1, viewableStudents.size());
            } else {
                fail("Unknown assignment returned by getViewableStudentsForUserForAssignments");
            }
        }

        // the ta should have restrictions on a1
        // should only get student 1 b/c may only see students in his/her group
        // should still get 1 for a2
        assignmentToStudentListMap = permissionLogic.getViewableStudentsForUserForAssignments(AssignmentTestDataLoad.TA_UID, AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(4, assignmentToStudentListMap.size());
        for (Map.Entry<Assignment2, List<String>> entry : assignmentToStudentListMap.entrySet()) {
            Assignment2 assign = entry.getKey();
            List<String> viewableStudents = entry.getValue();
            if (assign.equals(testData.a1)) {
                assertEquals(1, viewableStudents.size());
            } else if (assign.equals(testData.a2)) {
                assertEquals(1, viewableStudents.size());
            } else if (assign.equals(testData.a3)) {
                assertEquals(1, viewableStudents.size());
            } else if (assign.equals(testData.a4)) {  
                assertEquals(0, viewableStudents.size());
            } else {
                fail("Unknown assignment returned by getViewableStudentsForUserForAssignments");
            }
        }

        // let's add a group restriction to a2 and make sure no students are returned
        AssignmentGroup groupFora2 = new AssignmentGroup(testData.a2, AssignmentTestDataLoad.GROUP3_NAME);
        dao.save(groupFora2);
        // shouldn't get any student back now for a2
        assignmentToStudentListMap = permissionLogic.getViewableStudentsForUserForAssignments(AssignmentTestDataLoad.TA_UID, AssignmentTestDataLoad.CONTEXT_ID, assignList);
        assertEquals(4, assignmentToStudentListMap.size());
        for (Map.Entry<Assignment2, List<String>> entry : assignmentToStudentListMap.entrySet()) {
            Assignment2 assign = entry.getKey();
            List<String> viewableStudents = entry.getValue();
            if (assign.equals(testData.a1)) {
                assertEquals(1, viewableStudents.size());
            } else if (assign.equals(testData.a2)) {
                assertEquals(0, viewableStudents.size());
            } else if (assign.equals(testData.a3)) {
                assertEquals(1, viewableStudents.size());
            } else if (assign.equals(testData.a4)) {  
                assertEquals(0, viewableStudents.size());
            } else {
                fail("Unknown assignment returned by getViewableStudentsForUserForAssignments");
            }
        }
    }

    public void testGetGradableStudentsForUserForItem() {
        // try passing a null userId
        try {
            permissionLogic.getGradableStudentsForUserForItem(null, testData.a1);
            fail("did not catch null userId passed to getGradableStudentsForUserForItem");
        } catch(IllegalArgumentException iae) {}

        // try passing a null assignment
        try {
            permissionLogic.getGradableStudentsForUserForItem(AssignmentTestDataLoad.STUDENT1_UID, null);
            fail("did not catch null assignment passed to getGradableStudentsForUserForItem");
        } catch(IllegalArgumentException iae) {}

        // this method should return 0 if a student calls it

        List<String> gradableStudents = permissionLogic.getGradableStudentsForUserForItem(AssignmentTestDataLoad.STUDENT1_UID, testData.a1);
        assertEquals(0, gradableStudents.size());

        // this method is exactly the same as getViewableStudentsForItem except
        // if there are grader permission involved. this allows the instructor
        // to restrict ta's to view-only instead of view and grade
        // TODO - we must integrate grader permissions for this test to be accurate   

        // Let's start with an ungraded item

        // instructor should get all students who have the assignment
        // a1 is restricted to groups, so will return all students in those groups
        gradableStudents = permissionLogic.getGradableStudentsForUserForItem(AssignmentTestDataLoad.INSTRUCTOR_UID, testData.a1);
        assertEquals(2, gradableStudents.size());
        // this one is not restricted
        gradableStudents = permissionLogic.getGradableStudentsForUserForItem(AssignmentTestDataLoad.INSTRUCTOR_UID, testData.a2);
        assertEquals(3, gradableStudents.size());

        // the ta should have restrictions on a1
        // should only get student 1 b/c may only see students in his/her group
        gradableStudents = permissionLogic.getGradableStudentsForUserForItem(AssignmentTestDataLoad.TA_UID, testData.a1);
        assertEquals(1, gradableStudents.size());
        // should still get 1 for a2
        gradableStudents = permissionLogic.getGradableStudentsForUserForItem(AssignmentTestDataLoad.TA_UID, testData.a2);
        assertEquals(1, gradableStudents.size());
        // let's add a group restriction to a2 and make sure no students are returned
        AssignmentGroup groupFora2 = new AssignmentGroup(testData.a2, AssignmentTestDataLoad.GROUP3_NAME);
        dao.save(groupFora2);
        // shouldn't get any student back now
        gradableStudents = permissionLogic.getGradableStudentsForUserForItem(AssignmentTestDataLoad.TA_UID, testData.a2);
        assertEquals(0, gradableStudents.size());

        // now we will consider a graded item
        // switch back to instructor
        // a3 is not restricted, so will return all students
        gradableStudents = permissionLogic.getGradableStudentsForUserForItem(AssignmentTestDataLoad.INSTRUCTOR_UID, testData.a3);
        assertEquals(3, gradableStudents.size());
        // a4 is restricted to group 3
        gradableStudents = permissionLogic.getGradableStudentsForUserForItem(AssignmentTestDataLoad.INSTRUCTOR_UID, testData.a4);
        assertEquals(1, gradableStudents.size());

        // now switch to the ta
        // TODO - GRADER PERMISSIONS!!

        // a3 should return all students in ta's groups
        gradableStudents = permissionLogic.getGradableStudentsForUserForItem(AssignmentTestDataLoad.TA_UID, testData.a3);
        assertEquals(1, gradableStudents.size());
        // a4 should not return any
        gradableStudents = permissionLogic.getGradableStudentsForUserForItem(AssignmentTestDataLoad.TA_UID, testData.a4);
        assertEquals(0, gradableStudents.size());
    }

    public void testIsUserAbleToMakeSubmissionForAssignment() {

        // try passing a null assignment
        try {
            permissionLogic.isUserAbleToMakeSubmissionForAssignment(null);
            fail("did not catch null assignment passed to isUserAbleToMakeSubmissionForAssignment");
        } catch (IllegalArgumentException iae) {}

        // try passing an empty assignment
        try {
            permissionLogic.isUserAbleToMakeSubmissionForAssignment(new Assignment2());
            fail("did not catch null fields assoc with assignment passed to isUserAbleToMakeSubmissionForAssignment");
        } catch (IllegalArgumentException iae) {}


        // TODO - we need to define "who" can submit, so will need to update the tests
        // currently there is no check on whether you are a student, guest, instructor, etc
        // let's just test students for now
        // student 1 is a member of group 1
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        // should be able to submit for a1, a2, a3
        assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(testData.a1));
        assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(testData.a2));
        assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(testData.a3));
        assertFalse(permissionLogic.isUserAbleToMakeSubmissionForAssignment(testData.a4));

        // student 2 is a member of group 3
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT2_UID);
        // should be able to submit for a1, a2, a3, a4
        assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(testData.a1));
        assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(testData.a2));
        assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(testData.a3));
        assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(testData.a4));

        // student 3 is not a member of any groups
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT3_UID);
        // should only be able to submit to 2,3
        assertFalse(permissionLogic.isUserAbleToMakeSubmissionForAssignment(testData.a1));
        assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(testData.a2));
        assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(testData.a3));
        assertFalse(permissionLogic.isUserAbleToMakeSubmissionForAssignment(testData.a4));
    }

    public void testIsUserAllowedToReleaseFeedbackForAssignment() {
        // try passing a null assignment
        try {
            permissionLogic.isUserAllowedToProvideFeedbackForAssignment(null);
            fail("Null assignment passed to isUserAllowedToReleaseFeedbackForAssignment was not caught");
        } catch (IllegalArgumentException iae) {}

        // instructor should be true for all
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assertTrue(permissionLogic.isUserAllowedToProvideFeedbackForAssignment(testData.a1));
        assertTrue(permissionLogic.isUserAllowedToProvideFeedbackForAssignment(testData.a2));
        assertTrue(permissionLogic.isUserAllowedToProvideFeedbackForAssignment(testData.a3));
        assertTrue(permissionLogic.isUserAllowedToProvideFeedbackForAssignment(testData.a4));

        // ta should be true for a1, a2, a3 - not auth to grade any students for a4
        // b/c only avail to students in group3 and doesn't have grading perm for
        // this group
        // TODO grader permissions
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        assertTrue(permissionLogic.isUserAllowedToProvideFeedbackForAssignment(testData.a1));
        assertTrue(permissionLogic.isUserAllowedToProvideFeedbackForAssignment(testData.a2));
        assertTrue(permissionLogic.isUserAllowedToProvideFeedbackForAssignment(testData.a3));
        assertFalse(permissionLogic.isUserAllowedToProvideFeedbackForAssignment(testData.a4));

        // double check that students are all false
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        assertFalse(permissionLogic.isUserAllowedToProvideFeedbackForAssignment(testData.a1));
        assertFalse(permissionLogic.isUserAllowedToProvideFeedbackForAssignment(testData.a2));
        assertFalse(permissionLogic.isUserAllowedToProvideFeedbackForAssignment(testData.a3));
        assertFalse(permissionLogic.isUserAllowedToProvideFeedbackForAssignment(testData.a4));
    }

    public void testIsCurrentUserAbleToSubmit() {
        // currently, only students defined by the gb may submit
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assertFalse(permissionLogic.isCurrentUserAbleToSubmit(AssignmentTestDataLoad.CONTEXT_ID));
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        assertFalse(permissionLogic.isCurrentUserAbleToSubmit(AssignmentTestDataLoad.CONTEXT_ID));

        // now try the students
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        assertTrue(permissionLogic.isCurrentUserAbleToSubmit(AssignmentTestDataLoad.CONTEXT_ID));
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT2_UID);
        assertTrue(permissionLogic.isCurrentUserAbleToSubmit(AssignmentTestDataLoad.CONTEXT_ID));
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT3_UID);
        assertTrue(permissionLogic.isCurrentUserAbleToSubmit(AssignmentTestDataLoad.CONTEXT_ID));

        // try a bogus context
        assertFalse(permissionLogic.isCurrentUserAbleToSubmit(AssignmentTestDataLoad.BAD_CONTEXT));
    }

    public void testGetUsersAllowedToViewStudentForAssignment() {
        // try some null params
        try {
            permissionLogic.getUsersAllowedToViewStudentForAssignment(null, testData.a1);
            fail("Did not catch null studentId passed to getUsersAllowedToViewStudentForAssignment");
        } catch (IllegalArgumentException iae) {}

        try {
            permissionLogic.getUsersAllowedToViewStudentForAssignment(AssignmentTestDataLoad.TA_UID, null);
            fail("Did not catch null assignment passed to getUsersAllowedToViewStudentForAssignment");
        } catch (IllegalArgumentException iae) {}

        // instructor and ta passed as a student should return nothing
        List<String> usersAllowedToView = permissionLogic.getUsersAllowedToViewStudentForAssignment(AssignmentTestDataLoad.INSTRUCTOR_UID, testData.a1);
        assertEquals(0, usersAllowedToView.size());

        usersAllowedToView = permissionLogic.getUsersAllowedToViewStudentForAssignment(AssignmentTestDataLoad.TA_UID, testData.a1);
        assertEquals(0, usersAllowedToView.size());

        // ta only has access to group 1 - student1
        // STUDENT 1 should have inst and ta
        usersAllowedToView = permissionLogic.getUsersAllowedToViewStudentForAssignment(AssignmentTestDataLoad.STUDENT1_UID, testData.a1);
        assertEquals(2, usersAllowedToView.size());
        assertTrue(usersAllowedToView.contains(AssignmentTestDataLoad.TA_UID));
        assertTrue(usersAllowedToView.contains(AssignmentTestDataLoad.INSTRUCTOR_UID));

        // student 2 should only have instructor
        usersAllowedToView = permissionLogic.getUsersAllowedToViewStudentForAssignment(AssignmentTestDataLoad.STUDENT2_UID, testData.a1);
        assertEquals(1, usersAllowedToView.size());
        assertTrue(usersAllowedToView.contains(AssignmentTestDataLoad.INSTRUCTOR_UID));

        // student 3 does not have access to assign 1
        usersAllowedToView = permissionLogic.getUsersAllowedToViewStudentForAssignment(AssignmentTestDataLoad.STUDENT3_UID, testData.a1);
        assertEquals(0, usersAllowedToView.size());

        // all of the students can access assign 3
        usersAllowedToView = permissionLogic.getUsersAllowedToViewStudentForAssignment(AssignmentTestDataLoad.STUDENT1_UID, testData.a3);
        assertEquals(2, usersAllowedToView.size());
        assertTrue(usersAllowedToView.contains(AssignmentTestDataLoad.TA_UID));
        assertTrue(usersAllowedToView.contains(AssignmentTestDataLoad.INSTRUCTOR_UID));

        // student 2 should only have instructor
        usersAllowedToView = permissionLogic.getUsersAllowedToViewStudentForAssignment(AssignmentTestDataLoad.STUDENT2_UID, testData.a3);
        assertEquals(1, usersAllowedToView.size());
        assertTrue(usersAllowedToView.contains(AssignmentTestDataLoad.INSTRUCTOR_UID));

        // student 3 should only have instructor
        usersAllowedToView = permissionLogic.getUsersAllowedToViewStudentForAssignment(AssignmentTestDataLoad.STUDENT3_UID, testData.a3);
        assertEquals(1, usersAllowedToView.size());
        assertTrue(usersAllowedToView.contains(AssignmentTestDataLoad.INSTRUCTOR_UID));
    }

    public void testGetViewableGroupsForCurrentUser() {
        try {
            permissionLogic.getViewableGroupsForCurrentUser(null);
            fail("Did not catch null contextId passed to getViewableGroupsForCurrentUser");
        } catch (IllegalArgumentException iae) {}


    }

    public void testGetViewableGroupsForCurrUserForAssignment() {
        // try passing a null
        try {
            permissionLogic.getViewableGroupsForCurrUserForAssignment(null);
            fail("did not catch null assignmentId passed to getViewableGroupsForCurrUserForAssignment");
        } catch (IllegalArgumentException iae) {}

        // Assign 1 is restricted to groups 1 and 3
        // Assign 2 is not restricted
        // there are 3 groups defined 

        // start as instructor - should be allowed to view all available groups
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        List<Group> viewableGroups = permissionLogic.getViewableGroupsForCurrUserForAssignment(testData.a1Id);
        assertEquals(2, viewableGroups.size());  // restricted to group 1 and 3
        for (Group group : viewableGroups) {
            if (!group.getId().equals(AssignmentTestDataLoad.GROUP1_NAME) &&
                    !group.getId().equals(AssignmentTestDataLoad.GROUP3_NAME)) {
                fail("Unknown group returned by getViewableGroupsForCurrUserForAssignment");
            }
        }


        viewableGroups = permissionLogic.getViewableGroupsForCurrUserForAssignment(testData.a2Id);
        assertEquals(3, viewableGroups.size());
        for (Group group : viewableGroups) {
            if (!group.getId().equals(AssignmentTestDataLoad.GROUP1_NAME) &&
                    !group.getId().equals(AssignmentTestDataLoad.GROUP2_NAME) &&
                    !group.getId().equals(AssignmentTestDataLoad.GROUP3_NAME)) {
                fail("Unknown group returned by getViewableGroupsForCurrUserForAssignment");
            }
        }

        // now test TA - should only be able to view group 1
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        viewableGroups = permissionLogic.getViewableGroupsForCurrUserForAssignment(testData.a1Id);
        assertEquals(1, viewableGroups.size());  // restricted to group 1 and 3 and only 1 is viewable
        for (Group group : viewableGroups) {
            if (!group.getId().equals(AssignmentTestDataLoad.GROUP1_NAME)) {
                fail("Unknown group returned by getViewableGroupsForCurrUserForAssignment");
            }
        }

        viewableGroups = permissionLogic.getViewableGroupsForCurrUserForAssignment(testData.a2Id);
        assertEquals(1, viewableGroups.size());
        for (Group group : viewableGroups) {
            if (!group.getId().equals(AssignmentTestDataLoad.GROUP1_NAME)) {
                fail("Unknown group returned by getViewableGroupsForCurrUserForAssignment");
            }
        }

        // students should get their own group memberships
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        viewableGroups = permissionLogic.getViewableGroupsForCurrUserForAssignment(testData.a1Id);
        assertEquals(1, viewableGroups.size());  

        viewableGroups = permissionLogic.getViewableGroupsForCurrUserForAssignment(testData.a2Id);
        assertEquals(1, viewableGroups.size());

    }

    public void testIsUserAbleToProvideFeedbackForAllStudents() {
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
        assertTrue(permissionLogic.isUserAbleToProvideFeedbackForAllStudents(AssignmentTestDataLoad.CONTEXT_ID));

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        assertFalse(permissionLogic.isUserAbleToProvideFeedbackForAllStudents(AssignmentTestDataLoad.CONTEXT_ID));

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
        assertFalse(permissionLogic.isUserAbleToProvideFeedbackForAllStudents(AssignmentTestDataLoad.CONTEXT_ID));

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT2_UID);
        assertFalse(permissionLogic.isUserAbleToProvideFeedbackForAllStudents(AssignmentTestDataLoad.CONTEXT_ID));

        externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT3_UID);
        assertFalse(permissionLogic.isUserAbleToProvideFeedbackForAllStudents(AssignmentTestDataLoad.CONTEXT_ID));

    }
}
