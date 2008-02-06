/**********************************************************************************
*
* $Id$
*
***********************************************************************************
*
* Copyright (c) 2005 The Regents of the University of California, The MIT Corporation
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
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;

import junit.framework.Assert;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.test.AssignmentTestDataLoad;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.exception.ConflictingAssignmentNameException;

import org.sakaiproject.section.api.facade.Role;


/**
 * Uses spring's mock-objects to test the gradebook service without modifying the database
 *
 * @author <a href="mailto:jholtzman@berkeley.edu">Josh Holtzman</a>
 */
public class AssignmentLogicTest extends Assignment2TestBase {

    private static final Log log = LogFactory.getLog(AssignmentLogicTest.class);
    
    private Long gbItem1Id;
    private Long gbItem2Id;

    /**
     * @see org.springframework.test.AbstractTransactionalSpringContextTests#onSetUpInTransaction()
     */
    protected void onSetUpInTransaction() throws Exception {
        super.onSetUpInTransaction();
        
        // set up the users
        userManager.createUser(AssignmentTestDataLoad.INSTRUCTOR_UID, null, null, null);
        userManager.createUser(AssignmentTestDataLoad.TA_UID, null, null, null);
        userManager.createUser(AssignmentTestDataLoad.STUDENT1_UID, null, null, null);
        userManager.createUser(AssignmentTestDataLoad.STUDENT2_UID, null, null, null);
        
        // set up the course
        integrationSupport.createCourse(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.CONTEXT_ID, false, false, false);
        integrationSupport.addSiteMembership(AssignmentTestDataLoad.INSTRUCTOR_UID, AssignmentTestDataLoad.CONTEXT_ID, Role.INSTRUCTOR);
        integrationSupport.addSiteMembership(AssignmentTestDataLoad.TA_UID, AssignmentTestDataLoad.CONTEXT_ID, Role.TA);
        integrationSupport.addSiteMembership(AssignmentTestDataLoad.STUDENT1_UID, AssignmentTestDataLoad.CONTEXT_ID, Role.STUDENT);
        integrationSupport.addSiteMembership(AssignmentTestDataLoad.STUDENT2_UID, AssignmentTestDataLoad.CONTEXT_ID, Role.STUDENT);
        
        // set up the gradebook
        gradebookFrameworkService.addGradebook(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.CONTEXT_ID);
        
        // switch to the instructor role
        authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
        // add some gb assignments
        org.sakaiproject.service.gradebook.shared.Assignment gbAssign1 = 
        	new org.sakaiproject.service.gradebook.shared.Assignment();
        gbAssign1.setName(GB_ITEM1_NAME);
        gbAssign1.setPoints(GB_ITEM1_PTS);
        gbAssign1.setDueDate(GB_ITEM1_DUE);
        gradebookService.addAssignment(AssignmentTestDataLoad.CONTEXT_ID, gbAssign1);
        org.sakaiproject.service.gradebook.shared.Assignment item1 = 
        	gradebookService.getAssignment(AssignmentTestDataLoad.CONTEXT_ID, GB_ITEM1_NAME);
        gbItem1Id = item1.getId();
        
        org.sakaiproject.service.gradebook.shared.Assignment gbAssign2 = 
        	new org.sakaiproject.service.gradebook.shared.Assignment();
        gbAssign2.setName(GB_ITEM2_NAME);
        gbAssign2.setPoints(GB_ITEM2_PTS);
        gbAssign2.setDueDate(GB_ITEM2_DUE);
        gradebookService.addAssignment(AssignmentTestDataLoad.CONTEXT_ID, gbAssign2);
        org.sakaiproject.service.gradebook.shared.Assignment item2 = 
        	gradebookService.getAssignment(AssignmentTestDataLoad.CONTEXT_ID, GB_ITEM2_NAME);
        gbItem2Id = item2.getId();
    }


    public void testGetAssignmentById() throws Exception {
    	// try a null id
    	try {
    		assignmentLogic.getAssignmentById(null);
    		fail("did not catch null id passed to getAssignmentById");
    	} catch (IllegalArgumentException iae) {
    	}
    	
    	// try a bogus id
    	Assignment2 assignment = assignmentLogic.getAssignmentById(new Long(12345));
    	assertNull(assignment);
    	
    	// grab assignment 1
    	assignment = assignmentLogic.getAssignmentById(testData.a1Id);
    	assertNotNull(assignment);
    	assertTrue(assignment.getTitle().equals(AssignmentTestDataLoad.ASSIGN1_TITLE));
    	assertTrue(assignment.getAttachmentSet().size() == 2);
    	assertTrue(assignment.getAssignmentGroupSet().size() == 2);
    }
    
    public void testSaveAssignment() throws Exception {
    	// try null param
    	try {
    		assignmentLogic.saveAssignment(null);
    		fail("did not catch null assignment passed to saveAssignment");
    	} catch (IllegalArgumentException iae) {
    	}
    	
    	Assignment2 newAssign = new Assignment2();
    	
    	// let's make sure users without the edit perm can't save
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	try {
    		assignmentLogic.saveAssignment(newAssign);
    		fail("SecurityException was not thrown even though user does NOT have permission to save an assignment");
    	} catch(SecurityException se) {
    	}
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	try {
    		assignmentLogic.saveAssignment(newAssign);
    		fail("SecurityException was not thrown even though user does NOT have permission to save an assignment");
    	} catch(SecurityException se) {
    	}
    	
    	// switch to a user with edit perm
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	
    	// let's start by creating a new assignment
    	newAssign.setAllowResubmit(false);
    	newAssign.setContextId(AssignmentTestDataLoad.CONTEXT_ID);
    	newAssign.setDraft(false);
    	newAssign.setHasAnnouncement(false);
    	newAssign.setHonorPledge(false);
    	newAssign.setInstructions("Complete this by friday");
    	newAssign.setNotificationType(AssignmentConstants.NOTIFY_NONE);
    	newAssign.setOpenTime(new Date());
    	newAssign.setTitle(AssignmentTestDataLoad.ASSIGN1_TITLE);
    	newAssign.setUngraded(true);
    	
    	// start with 1 group and 2 attachments
    	Set<AssignmentGroup> assignGroupSet = new HashSet();
    	AssignmentGroup group1 = new AssignmentGroup(newAssign, "group1Ref");
    	assignGroupSet.add(group1);
    	
    	Set<AssignmentAttachment> attachSet = new HashSet();
    	AssignmentAttachment attach1 = new AssignmentAttachment(newAssign, "attach1Ref");
    	attachSet.add(attach1);
    	AssignmentAttachment attach2 = new AssignmentAttachment(newAssign, "attach2Ref");
    	attachSet.add(attach2);
    	
    	newAssign.setAssignmentGroupSet(assignGroupSet);
    	newAssign.setAttachmentSet(attachSet);
    	
    	// first, try to hit the duplicate title exception
    	try {
    		assignmentLogic.saveAssignment(newAssign);
    		fail("Did not catch a duplicate name exception!");
    	} catch (ConflictingAssignmentNameException cane) {
    	}
    	
    	// change the name
    	String newTitle = "New assignment";
    	newAssign.setTitle(newTitle);
    	assignmentLogic.saveAssignment(newAssign);
    	
    	// double check that it was saved
    	List assignList = dao.findByProperties(Assignment2.class, new String[] {"title"}, new Object[] {newTitle});
    	assertNotNull(assignList);
    	assertTrue(assignList.size() == 1);
    	newAssign = (Assignment2)assignList.get(0);
    	// check that groups and attach were saved, as well
    	assertTrue(newAssign.getAssignmentGroupSet().size() == 1);
    	assertTrue(newAssign.getAttachmentSet().size() == 2);
    	
    	// let's retrieve this one to update
    	Assignment2 updatedAssign = dao.getAssignmentByIdWithGroupsAndAttachments(newAssign.getId());
    	String revisedTitle = "revised title";
    	updatedAssign.setTitle(revisedTitle);
    	
    	// add a group
    	updatedAssign.getAssignmentGroupSet().add(new AssignmentGroup(updatedAssign, "newGroupRef"));
    	// remove the attachments
    	updatedAssign.setAttachmentSet(null);
    	
    	assignmentLogic.saveAssignment(updatedAssign);
    	
    	Assignment2 another = dao.getAssignmentByIdWithGroups(updatedAssign.getId());
    	assertNotNull(another);
    	assertTrue(another.getTitle().equals(revisedTitle));

    	// check that groups and attach were saved, as well
    	assertTrue(updatedAssign.getAssignmentGroupSet().size() == 2);
    	assertNull(updatedAssign.getAttachmentSet());
    }
    
    public void testDeleteAssignment() throws Exception {
    	// pass a null assignment
    	try {
    		assignmentLogic.deleteAssignment(null);
    		fail("Null assignment passed to deleteAssignment was not caught");
    	} catch (IllegalArgumentException iae) {}
    	
    	// pass an assignment w/o an id
    	try {
    		assignmentLogic.deleteAssignment(new Assignment2());
    		fail("Did not catch attempt to delete assignment without an id");
    	} catch (IllegalArgumentException iae) {}
    	
    	// now let's check the security 
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	try {
    		assignmentLogic.deleteAssignment(testData.a1);
    		fail("SecurityException was not thrown even though user does NOT have permission to delete an assignment");
    	} catch(SecurityException se) {
    	}
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	try {
    		assignmentLogic.deleteAssignment(testData.a1);
    		fail("SecurityException was not thrown even though user does NOT have permission to delete an assignment");
    	} catch(SecurityException se) {
    	}
    	
    	// let's actually delete an assignment
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	assignmentLogic.deleteAssignment(testData.a1);
    	
    	// let's double check that it still exists. it should just have removed = true now
    	Assignment2 deletedAssign = assignmentLogic.getAssignmentById(testData.a1Id);
    	assertNotNull(deletedAssign);
    	assertTrue(deletedAssign.isRemoved());
    	
    	//TODO - somehow check the deletion of announcements?
    }
    
    public void testGetViewableAssignments() throws Exception {
    	// this method will return different results based upon the user
    	// and section memberships
    	// TODO add scenario with grader permissions
    	
    	// let's start with instructor. he/she should get all of the assignments
    	
    }

}
