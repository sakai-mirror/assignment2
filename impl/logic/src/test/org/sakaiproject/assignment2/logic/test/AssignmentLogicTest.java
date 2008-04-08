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

import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.HashSet;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.test.AssignmentTestDataLoad;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.exception.NoGradebookItemForGradedAssignmentException;

import org.sakaiproject.section.api.coursemanagement.CourseSection;
import org.sakaiproject.section.api.coursemanagement.Course;
import org.sakaiproject.section.api.facade.Role;


public class AssignmentLogicTest extends Assignment2TestBase {

    private static final Log log = LogFactory.getLog(AssignmentLogicTest.class);

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
        userManager.createUser(AssignmentTestDataLoad.STUDENT3_UID, null, null, null);
        
        // set up the course
        Course site = integrationSupport.createCourse(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.CONTEXT_ID, false, false, false);
        integrationSupport.addSiteMembership(AssignmentTestDataLoad.INSTRUCTOR_UID, AssignmentTestDataLoad.CONTEXT_ID, Role.INSTRUCTOR);
        integrationSupport.addSiteMembership(AssignmentTestDataLoad.TA_UID, AssignmentTestDataLoad.CONTEXT_ID, Role.TA);
        integrationSupport.addSiteMembership(AssignmentTestDataLoad.STUDENT1_UID, AssignmentTestDataLoad.CONTEXT_ID, Role.STUDENT);
        integrationSupport.addSiteMembership(AssignmentTestDataLoad.STUDENT2_UID, AssignmentTestDataLoad.CONTEXT_ID, Role.STUDENT);
        integrationSupport.addSiteMembership(AssignmentTestDataLoad.STUDENT3_UID, AssignmentTestDataLoad.CONTEXT_ID, Role.STUDENT);
        
        // create some sections
        List sectionCategories = sectionAwareness.getSectionCategories(AssignmentTestDataLoad.CONTEXT_ID);
        CourseSection section1 = integrationSupport.createSection(site.getUuid(), AssignmentTestDataLoad.GROUP1_NAME,
				(String)sectionCategories.get(0),
				new Integer(40), null, null, null, true, false, true,  false, false, false, false);
		section1Uid = section1.getUuid();

		CourseSection section2 = integrationSupport.createSection(site.getUuid(), AssignmentTestDataLoad.GROUP2_NAME,
				(String)sectionCategories.get(0),
				new Integer(40), null, null, null, true, false, true,  false, false, false, false);
		section2Uid = section2.getUuid();
		CourseSection section3 = integrationSupport.createSection(site.getUuid(), AssignmentTestDataLoad.GROUP3_NAME,
				(String)sectionCategories.get(0),
				new Integer(40), null, null, null, true, false, true,  false, false, false, false);
		section3Uid = section3.getUuid();
		
		
		// put some users in the sections
		// add TA and STUDENT1 to section 1
		integrationSupport.addSectionMembership(AssignmentTestDataLoad.STUDENT1_UID, section1Uid, Role.STUDENT);
		integrationSupport.addSectionMembership(AssignmentTestDataLoad.TA_UID, section1Uid, Role.TA);
		// add STUDENT2 to section 3
		integrationSupport.addSectionMembership(AssignmentTestDataLoad.STUDENT2_UID, section3Uid, Role.STUDENT);
        
		// refresh the testData vars
		testData.a1 = dao.getAssignmentByIdWithGroupsAndAttachments(testData.a1Id);
		testData.a2 = dao.getAssignmentByIdWithGroupsAndAttachments(testData.a2Id);
		testData.a3 = dao.getAssignmentByIdWithGroupsAndAttachments(testData.a3Id);
		testData.a4 = dao.getAssignmentByIdWithGroupsAndAttachments(testData.a4Id);
		
		// now that we have sections defined, let's update the testData AssignmentGroups to be 
		// consistent. we can't do it from the beginning b/c i couldn't figure out how to inject
		// the sections stuff into the PreloadTestData bean...
		updateAssignmentGroupId(testData.a1.getAssignmentGroupSet());
		updateAssignmentGroupId(testData.a2.getAssignmentGroupSet());
		updateAssignmentGroupId(testData.a3.getAssignmentGroupSet());
		updateAssignmentGroupId(testData.a4.getAssignmentGroupSet());
		
        // set up the gradebook
        gradebookFrameworkService.addGradebook(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.CONTEXT_ID);
        
        // switch to the instructor role
        authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
        // add some gb items
        org.sakaiproject.service.gradebook.shared.Assignment gbItem1 = 
        	new org.sakaiproject.service.gradebook.shared.Assignment();
        gbItem1.setName(GB_ITEM1_NAME);
        gbItem1.setPoints(GB_ITEM1_PTS);
        gbItem1.setDueDate(GB_ITEM1_DUE);
        gradebookService.addAssignment(AssignmentTestDataLoad.CONTEXT_ID, gbItem1);
        org.sakaiproject.service.gradebook.shared.Assignment item1 = 
        	gradebookService.getAssignment(AssignmentTestDataLoad.CONTEXT_ID, GB_ITEM1_NAME);
        gbItem1Id = item1.getId();
        
        org.sakaiproject.service.gradebook.shared.Assignment gbItem2 = 
        	new org.sakaiproject.service.gradebook.shared.Assignment();
        gbItem2.setName(GB_ITEM2_NAME);
        gbItem2.setPoints(GB_ITEM2_PTS);
        gbItem2.setDueDate(GB_ITEM2_DUE);
        gradebookService.addAssignment(AssignmentTestDataLoad.CONTEXT_ID, gbItem2);
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
    }
    
    public void testSaveAssignment() throws Exception {
    	// try null param
    	try {
    		assignmentLogic.saveAssignment(null);
    		fail("did not catch null assignment passed to saveAssignment");
    	} catch (IllegalArgumentException iae) {
    	}
    	
    	Assignment2 newAssign = new Assignment2();
    	// let's start by creating a new assignment
    	newAssign.setContextId(AssignmentTestDataLoad.CONTEXT_ID);
    	newAssign.setDraft(false);
    	newAssign.setHasAnnouncement(false);
    	newAssign.setHonorPledge(false);
    	newAssign.setInstructions("Complete this by friday");
    	newAssign.setNotificationType(AssignmentConstants.NOTIFY_NONE);
    	newAssign.setOpenTime(new Date());
    	newAssign.setTitle(AssignmentTestDataLoad.ASSIGN1_TITLE); //we're using a title that already exists
    	newAssign.setUngraded(true);
    	
    	// start with 1 group and 2 attachments
    	Set<AssignmentGroup> assignGroupSet = new HashSet<AssignmentGroup>();
    	AssignmentGroup group1 = new AssignmentGroup(newAssign, "group1Ref");
    	assignGroupSet.add(group1);
    	
    	Set<AssignmentAttachment> attachSet = new HashSet<AssignmentAttachment>();
    	AssignmentAttachment attach1 = new AssignmentAttachment(newAssign, "attach1Ref");
    	attachSet.add(attach1);
    	AssignmentAttachment attach2 = new AssignmentAttachment(newAssign, "attach2Ref");
    	attachSet.add(attach2);
    	
    	newAssign.setAssignmentGroupSet(assignGroupSet);
    	newAssign.setAttachmentSet(attachSet);
    	
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
    	
    	// change the name
    	String newTitle = "New assignment";
    	newAssign.setTitle(newTitle);
    	
    	// now set this item to graded but don't populate gradableObjectId
    	newAssign.setUngraded(false);
    	
    	try {
    		assignmentLogic.saveAssignment(newAssign);
    		fail("Did not catch null gradableObjectId for graded assignment");
    	} catch(NoGradebookItemForGradedAssignmentException ge) {}
    	
    	// set the gradableObjectId
    	newAssign.setGradableObjectId(gbItem1Id);
    	
    	assignmentLogic.saveAssignment(newAssign);
    	
    	// double check that it was saved
    	List<Assignment2> assignList = dao.findByProperties(Assignment2.class, new String[] {"title"}, new Object[] {newTitle});
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
    	
    	// TODO - try adding an announcement
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
    	// TODO add scenario with grader permissions for TA
    	
    	// let's make assignment3 and assignment4 graded
    	testData.a3.setUngraded(false);
    	testData.a3.setGradableObjectId(gbItem1Id);
    	dao.save(testData.a3);
    	
    	testData.a4.setUngraded(false);
    	testData.a4.setGradableObjectId(gbItem2Id);
    	dao.save(testData.a4);
    	
    	// assign1 is restricted to group 1 and 3
    	// assign2 is not restricted
    	// graded assign 3 is not restricted
    	// graded assign 4 is restricted to group 3
    	
    	// let's start with instructor. he/she should get all of the assignments
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	List<Assignment2> assignList = assignmentLogic.getViewableAssignments();
    	assertNotNull(assignList);
    	assert(assignList.size() == 4);
    	
    	// let's try the ta. he/she should only be able to view assignments if
    	// 1) it is ungraded and it is open to the site
    	// 2) it is ungraded and he/she is a member of a restricted group
    	// 3) it is graded and he/she is authorized to view/grade the associated
    	//		gb item in the gb

    	// our ta is a member of group 1. assign1 is restricted to group 1 and 2
    	// for a4 (graded), it is restricted to group, but the ta may view the
    	// assignment if he/she is authorized to view the corresponding gb item

    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	// should return assignment 1, 2, 3, 4
    	assignList = assignmentLogic.getViewableAssignments();
    	assertNotNull(assignList);
    	assertTrue(assignList.size() == 4);
    	// let's make sure that these are the right assign & gb info was populated
    	for (Assignment2 assign : assignList) {
    		if (assign.getId().equals(testData.a1Id) || assign.getId().equals(testData.a2Id)) {
    			
    		} else if (assign.getId().equals(testData.a3Id)) {
    			// this one is graded, so check that the gb info was populated
    			assertTrue(assign.getDueDate() == null);
    			assertTrue(assign.getPointsPossible().equals(GB_ITEM1_PTS));
    		} else if (assign.getId().equals(testData.a4Id)) {
    			// this one is graded, so check that the gb info was populated
    			assertTrue(assign.getDueDate().equals(GB_ITEM2_DUE));
    			assertTrue(assign.getPointsPossible().equals(GB_ITEM2_PTS));
    		} else {
    			fail("Invalid assignment returned for TA via getViewableAssignments");
    		}
    	}
    	
    	// switch to student 1
    	// member of section 1
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	// should return assignment 1, 2, 3
    	assignList = assignmentLogic.getViewableAssignments();
    	assertNotNull(assignList);
    	assertEquals(3, assignList.size());
    	for (Assignment2 assign : assignList) {
    		if (assign.getId().equals(testData.a1Id) || assign.getId().equals(testData.a2Id)) {
    			
    		} else if (assign.getId().equals(testData.a3Id)) {
    			// this one is graded, so check that the gb info was populated
    			assertTrue(assign.getDueDate() == null);
    			assertTrue(assign.getPointsPossible().equals(GB_ITEM1_PTS));
    		} else {
    			fail("Invalid assignment returned for STUDENT1 via getViewableAssignments");
    		}
    	}
    	
    	// switch to student 2
    	// member of section 3
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT2_UID);
    	// should return 1, 2, 3, 4
    	assignList = assignmentLogic.getViewableAssignments();
    	assertNotNull(assignList);
    	assertTrue(assignList.size() == 4);
    	// let's make sure that these are the right assign & gb info was populated
    	for (Assignment2 assign : assignList) {
    		if (assign.getId().equals(testData.a2Id) || assign.getId().equals(testData.a1Id)) {
    			
    		} else if (assign.getId().equals(testData.a3Id)) {
    			// this one is graded, so check that the gb info was populated
    			assertTrue(assign.getDueDate() == null);
    			assertTrue(assign.getPointsPossible().equals(GB_ITEM1_PTS));
    		} else if (assign.getId().equals(testData.a4Id)) {
    			// this one is graded, so check that the gb info was populated
    			assertTrue(assign.getDueDate().equals(GB_ITEM2_DUE));
    			assertTrue(assign.getPointsPossible().equals(GB_ITEM2_PTS));
    		} else {
    			fail("Invalid assignment returned for STUDENT2 via getViewableAssignments");
    		}
    	}
    	
    	// switch to student 3
    	// not a member of any sections
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT3_UID);
    	// should return 2 and 3
    	assignList = assignmentLogic.getViewableAssignments();
    	assertNotNull(assignList);
    	assertTrue(assignList.size() == 2);
    	// let's make sure that these are the right assign & gb info was populated
    	for (Assignment2 assign : assignList) {
    		if (assign.getId().equals(testData.a2Id)) {
    			
    		} else if (assign.getId().equals(testData.a3Id)) {
    			// this one is graded, so check that the gb info was populated
    			assertTrue(assign.getDueDate() == null);
    			assertTrue(assign.getPointsPossible().equals(GB_ITEM1_PTS));
    		} else {
    			fail("Invalid assignment returned for STUDENT3 via getViewableAssignments");
    		}
    	}
    }
    
    public void testSetAssignmentSortIndexes() throws Exception {
    	// this method is used for reordering assignments
    	// try passing a null even though there are 4 assigns in site
    	try {
    		assignmentLogic.setAssignmentSortIndexes(null);
    		fail("Did not catch null list passed to setAssignmentSortIndexes when there are 4 assign in site");
    	} catch (IllegalArgumentException iae) {}
    	// try passing a list w/ a diff # of values than # assign in site
    	try {
    		assignmentLogic.setAssignmentSortIndexes(new Long[] {new Long(3),new Long(2),new Long(1)});
    		fail("Did not catch list w/ 3 passed to setAssignmentSortIndexes when there are 4 assign in site");
    	} catch (IllegalArgumentException iae) {}
    	
    	// right now they are in order assign 1 - 4
    	// let's put assign 4 first
    	assignmentLogic.setAssignmentSortIndexes(new Long[] {testData.a4Id,testData.a1Id,testData.a2Id,testData.a3Id});
    	// double check that they were updated
    	List<Assignment2> allAssigns = dao.findByProperties(Assignment2.class, new String[] {"contextId","removed"}, new Object[] {AssignmentTestDataLoad.CONTEXT_ID, false});
    	for (Assignment2 assign : allAssigns) {
    		if (assign.getId().equals(testData.a1Id)) {
    			assertTrue(assign.getSortIndex() == 1);
    		} else if (assign.getId().equals(testData.a2Id)) {
    			assertTrue(assign.getSortIndex() == 2);
    		} else if (assign.getId().equals(testData.a3Id)) {
    			assertTrue(assign.getSortIndex() == 3);
    		} else if (assign.getId().equals(testData.a4Id)) {
    			assertTrue(assign.getSortIndex() == 0);
    		} else {
    			fail("Invalid assignment returned!");
    		}
    	}
    }
    
    public void testGetAssignmentByIdWithAssociatedData() throws Exception {
    	// try passing a null id
    	try {
    		assignmentLogic.getAssignmentByIdWithAssociatedData(null);
    		fail("Did not catch null assignment id passed to getAssignmentByIdWithAssociatedData");
    	} catch (IllegalArgumentException iae) {}
    	
    	// try passing an id that doesn't exist - should be null
    	Assignment2 assign = assignmentLogic.getAssignmentByIdWithAssociatedData(new Long(12345));
    	assertNull(assign);
    	
    	// let's make assignment4 graded
    	testData.a4.setUngraded(false);
    	testData.a4.setGradableObjectId(gbItem2Id);
    	dao.save(testData.a4);
    	
    	// let's try to retrieve this graded item now
    	assign = assignmentLogic.getAssignmentByIdWithAssociatedData(testData.a4Id);
    	assertNotNull(assign);
    	assertTrue(assign.getId().equals(testData.a4Id));
    	// the gradebook info should be populated
    	assertTrue(assign.getDueDate().equals(GB_ITEM2_DUE));
    	assertTrue(assign.getPointsPossible().equals(GB_ITEM2_PTS));
    	// double check groups and attach
    	assertTrue(assign.getAssignmentGroupSet().size() == 1);
    	assertTrue(assign.getAttachmentSet().isEmpty());
    	
    	// try an ungraded item
    	assign = assignmentLogic.getAssignmentByIdWithAssociatedData(testData.a1Id);
    	assertTrue(assign.getId().equals(testData.a1Id));
    	assertTrue(assign.getAssignmentGroupSet().size() == 2);
    	assertTrue(assign.getAttachmentSet().size() == 2); 	
    }
    
    public void testGetAssignmentByIdWithGroups() throws Exception {
    	// try passing a null id
    	try {
    		assignmentLogic.getAssignmentByIdWithGroups(null);
    		fail("Did not catch null assignment id passed to getAssignmentByIdWithGroups");
    	} catch (IllegalArgumentException iae) {}
    	
    	// try passing an id that doesn't exist - should be null
    	Assignment2 assign = assignmentLogic.getAssignmentByIdWithGroups(new Long(12345));
    	assertNull(assign);
    	
    	// try a valid item
    	assign = assignmentLogic.getAssignmentByIdWithGroups(testData.a1Id);
    	assertTrue(assign.getId().equals(testData.a1Id));
    	assertTrue(assign.getAssignmentGroupSet().size() == 2);	
    }
    
    public void testGetAssignmentByIdWithGroupsAndAttachments() throws Exception {
    	// try passing a null id
    	try {
    		assignmentLogic.getAssignmentByIdWithGroupsAndAttachments(null);
    		fail("Did not catch null assignment id passed to getAssignmentByIdWithGroupsAndAttachments");
    	} catch (IllegalArgumentException iae) {}
    	
    	// try passing an id that doesn't exist - should be null
    	Assignment2 assign = assignmentLogic.getAssignmentByIdWithGroupsAndAttachments(new Long(12345));
    	assertNull(assign);
    	
    	// try a valid item
    	assign = assignmentLogic.getAssignmentByIdWithGroupsAndAttachments(testData.a1Id);
    	assertTrue(assign.getId().equals(testData.a1Id));
    	assertTrue(assign.getAssignmentGroupSet().size() == 2);	
    	assertTrue(assign.getAttachmentSet().size() == 2);
    }
    
    public void testGetStatusForAssignment() {
    	// try a null assignment
    	try {
    		assignmentLogic.getStatusForAssignment(null);
    		fail("Did not catch null assignment passed to getStatusforAssignment");
    	} catch (IllegalArgumentException iae) {}
    	
    	Assignment2 assignment = new Assignment2();
    	// start out with non-draft, ungraded
    	assignment.setDraft(false);
    	assignment.setUngraded(true);
    	
    	// first, leave all of nullable fields null:
    	// 	acceptUntilTime, dueDateForUngraded, dueDate
    	
    	// start with an open date in the past
    	Calendar cal = Calendar.getInstance();
    	cal.set(2005, 10, 01);
    	assignment.setOpenTime(cal.getTime());
    	
    	// this should be open
    	assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_OPEN);
    	
    	// set the open date in the future
    	cal.set(2020, 10, 01);
    	assignment.setOpenTime(cal.getTime());
    	// should be not open
    	assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_NOT_OPEN);
    	
    	// set it to draft - should be draft
    	assignment.setDraft(true);
    	assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_DRAFT);
    	
    	// set it back to draft=false, open date in the past, and make it due in the future
    	assignment.setDraft(false);
    	cal.set(2005, 10, 01);
    	assignment.setOpenTime(cal.getTime());
    	cal.set(2020, 10, 01);
    	assignment.setDueDateForUngraded(cal.getTime());
    	assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_OPEN);
    	// now make it due
    	cal.set(2007, 10, 01);
    	assignment.setDueDateForUngraded(cal.getTime());
    	assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_DUE);
    	// add an accept until date in the future
    	cal.set(2020, 10, 01);
    	assignment.setAcceptUntilTime(cal.getTime());
    	assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_DUE);
    	// now make the accept until date in the past
    	cal.set(2007, 10, 01);
    	assignment.setAcceptUntilTime(cal.getTime());
    	assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_CLOSED);
    	
    	// now make this graded with accept until in past
    	assignment.setUngraded(false);
    	assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_CLOSED);
    	
    	// now check with accept until in future and no due date
    	cal.set(2020, 10, 01);
    	assignment.setAcceptUntilTime(cal.getTime());
    	assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_OPEN);
    	
    	// add a due date in the past
    	cal.set(2007, 10, 01);
    	assignment.setDueDate(cal.getTime());
    	assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_DUE);
    	
    	// double check that open date overrides other dates
    	cal.set(2020, 10, 01);
    	assignment.setOpenTime(cal.getTime());
    	assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_NOT_OPEN);
    	
    	// double check that draft overrides everything
    	assignment.setDraft(true);
    	assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_DRAFT);
    }
    
    public void testSaveAssignmentAnnouncement() throws Exception {
    	// try passing a null updatedAssignment
    	try {
    		assignmentLogic.saveAssignmentAnnouncement(new Assignment2(), null);
    		fail("did not catch null updatedAssignment passed to saveAssignmentAnnouncement");
    	} catch(IllegalArgumentException iae) {}
    	
    	// try passing an updatedAssignment without an id
    	try {
    		assignmentLogic.saveAssignmentAnnouncement(new Assignment2(), new Assignment2());
    		fail("did not catch updatedAssignment without an id passed to saveAssignmentAnnouncement");
    	} catch(IllegalArgumentException iae) {}

    	
    	//TODO - not sure how to access announcements from the test!!!!
    }

}
