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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.exception.AssignmentNotFoundException;
import org.sakaiproject.assignment2.exception.NoGradebookItemForGradedAssignmentException;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.test.AssignmentTestDataLoad;


public class AssignmentLogicTest extends Assignment2TestBase {

	private static final Log log = LogFactory.getLog(AssignmentLogicTest.class);

	/**
	 * @see org.springframework.test.AbstractTransactionalSpringContextTests#onSetUpInTransaction()
	 */
	protected void onSetUpInTransaction() throws Exception {
		super.onSetUpInTransaction();
	}


	public void testGetAssignmentById() throws Exception {
		// try a null id
		try {
			assignmentLogic.getAssignmentById(null);
			fail("did not catch null id passed to getAssignmentById");
		} catch (IllegalArgumentException iae) {
		}

		// try a bogus id
		try {
			assignmentLogic.getAssignmentById(12345L);
			fail("did not catch bogus assignmenId passed to getAssignmentById");
		} catch (AssignmentNotFoundException anfe) {}

		// grab assignment 1
		Assignment2 assignment = assignmentLogic.getAssignmentById(testData.a1Id);
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
		newAssign.setAddedToSchedule(false);
		newAssign.setHonorPledge(false);
		newAssign.setInstructions("Complete this by friday");
		newAssign.setSendSubmissionNotifications(false);
		newAssign.setOpenDate(new Date());
		newAssign.setTitle(AssignmentTestDataLoad.ASSIGN1_TITLE); //we're using a title that already exists
		newAssign.setGraded(false);

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
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
		try {
			assignmentLogic.saveAssignment(newAssign);
			fail("SecurityException was not thrown even though user does NOT have permission to save an assignment");
		} catch(SecurityException se) {
		}
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
		try {
			assignmentLogic.saveAssignment(newAssign);
			fail("SecurityException was not thrown even though user does NOT have permission to save an assignment");
		} catch(SecurityException se) {
		}

		// switch to a user with edit perm
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);

		// change the name
		String newTitle = "New assignment";
		newAssign.setTitle(newTitle);

		// now set this item to graded but don't populate gradableObjectId
		newAssign.setGraded(true);

		try {
			assignmentLogic.saveAssignment(newAssign);
			fail("Did not catch null gradableObjectId for graded assignment");
		} catch(NoGradebookItemForGradedAssignmentException ge) {}

		// set the gradableObjectId
		newAssign.setGradableObjectId(AssignmentTestDataLoad.GB_ITEM1_ID);

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
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
		try {
			assignmentLogic.deleteAssignment(testData.a1);
			fail("SecurityException was not thrown even though user does NOT have permission to delete an assignment");
		} catch(SecurityException se) {
		}
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
		try {
			assignmentLogic.deleteAssignment(testData.a1);
			fail("SecurityException was not thrown even though user does NOT have permission to delete an assignment");
		} catch(SecurityException se) {
		}

		// let's actually delete an assignment
		// TODO - this will crash the unit tests b/c of the call to TaggingManager
		// we need to fix this so the test works
		
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	assignmentLogic.deleteAssignment(testData.a1);

    	// let's double check that it still exists. it should just have removed = true now
    	Assignment2 deletedAssign = assignmentLogic.getAssignmentById(testData.a1Id);
    	assertNotNull(deletedAssign);
    	assertTrue(deletedAssign.isRemoved());

		//TODO - somehow check the deletion of announcements?
	}

	public void testGetViewableAssignments() throws Exception {
		// this method will return different results based upon the user
		// and group memberships
		// TODO add scenario with grader permissions for TA

		// let's make assignment3 and assignment4 graded
		testData.a3.setGraded(true);
		testData.a3.setGradableObjectId(AssignmentTestDataLoad.GB_ITEM1_ID);
		dao.save(testData.a3);

		testData.a4.setGraded(true);
		testData.a4.setGradableObjectId(AssignmentTestDataLoad.GB_ITEM2_ID);
		dao.save(testData.a4);

		// assign1 is restricted to group 1 and 3
		// assign2 is not restricted
		// graded assign 3 is not restricted
		// graded assign 4 is restricted to group 3

		// let's start with instructor. he/she should get all of the assignments
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
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

		externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
		// should return assignment 1, 2, 3, 4
		assignList = assignmentLogic.getViewableAssignments();
		assertNotNull(assignList);
		assertTrue(assignList.size() == 4);
		// let's make sure that these are the right assign
		for (Assignment2 assign : assignList) {
			if (assign.getId().equals(testData.a1Id) || assign.getId().equals(testData.a2Id) || 
					assign.getId().equals(testData.a3Id) || assign.getId().equals(testData.a4Id)) { 
			} else {
				fail("Invalid assignment returned for TA via getViewableAssignments");
			}
		}

		// switch to student 1
		// member of group 1
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
		// should return assignment 1, 2, 3
		assignList = assignmentLogic.getViewableAssignments();

		assertNotNull(assignList);
		assertEquals(3, assignList.size());
		for (Assignment2 assign : assignList) {
			if (assign.getId().equals(testData.a1Id) || assign.getId().equals(testData.a2Id) ||
					assign.getId().equals(testData.a3Id)) {
				// valid
			}
			else {
				fail("Invalid assignment returned for STUDENT1 via getViewableAssignments");
			}
		}

		// switch to student 2
		// member of group 3
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT2_UID);
		// should return 1, 2, 3, 4
		assignList = assignmentLogic.getViewableAssignments();
		assertNotNull(assignList);
		assertTrue(assignList.size() == 4);
		// let's make sure that these are the right assign
		for (Assignment2 assign : assignList) {
			if (assign.getId().equals(testData.a2Id) || assign.getId().equals(testData.a1Id) ||
					assign.getId().equals(testData.a3Id) || assign.getId().equals(testData.a4Id)) {
				// valid
			} else {
				fail("Invalid assignment returned for STUDENT2 via getViewableAssignments");
			}
		}

		// switch to student 3
		// not a member of any groups
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT3_UID);
		// should return 2 and 3
		assignList = assignmentLogic.getViewableAssignments();
		assertNotNull(assignList);
		assertTrue(assignList.size() == 2);
		// let's make sure that these are the right assign
		for (Assignment2 assign : assignList) {
			if (assign.getId().equals(testData.a2Id)) {

			} else if (assign.getId().equals(testData.a3Id)) {

			} else {
				fail("Invalid assignment returned for STUDENT3 via getViewableAssignments");
			}
		}
	}

	public void testReorderAssignments() throws Exception {
		// this method is used for reordering assignments
	    
	    // try to pass a null list
	    try {
	        assignmentLogic.reorderAssignments(null);
	        fail("did not catch null list passed to reorderAssignments");
	    } catch (IllegalArgumentException iae) {}
	    
	    // try to reorder assign as TA
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
        try {
            assignmentLogic.reorderAssignments(new Long[] {});
            fail("Did not catch user trying to reorder assignments w/o permission!");
        } catch (SecurityException se){}
        
        // switch to someone who can reorder
        externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
		
		// try passing a list w/ a diff # of values than # assign in site
		try {
			assignmentLogic.reorderAssignments(new Long[] {3L, 2L, 1L});
			fail("Did not catch list w/ 3 passed to setAssignmentSortIndexes when " +
					"there are 4 assign in site");
		} catch (IllegalArgumentException iae) {}
		
		// try passing a list w/ a duplicate id that results in less than total
		// # assign in site
		try {
            assignmentLogic.reorderAssignments(new Long[] {3L, 2L, 1L, 1L});
            fail("Did not catch list w/ 3 distinct ids (list had a duplicate) passed " +
            		"to setAssignmentSortIndexes when there are 4 assign in site");
        } catch (IllegalArgumentException iae) {}
		
		// try passing a list containing a nonexistent id
		try {
            assignmentLogic.reorderAssignments(new Long[] {3L, 2L, 125L, 1L});
            fail("Did not catch list w/ nonexistent assignment id");
        } catch (IllegalArgumentException iae) {}
        
		// right now they are in order assign 1 - 4
		// let's put assign 4 first
		assignmentLogic.reorderAssignments(new Long[] {testData.a4Id,testData.a1Id,testData.a2Id,testData.a3Id});
		// double check that they were updated
		List<Assignment2> allAssigns = dao.findByProperties(Assignment2.class, new String[] {"contextId","removed"}, new Object[] {AssignmentTestDataLoad.CONTEXT_ID, false});
		for (Assignment2 assign : allAssigns) {
			if (assign.getId().equals(testData.a1Id)) {
				assertEquals(1, assign.getSortIndex());
			} else if (assign.getId().equals(testData.a2Id)) {
			    assertEquals(2, assign.getSortIndex());
			} else if (assign.getId().equals(testData.a3Id)) {
			    assertEquals(3, assign.getSortIndex());
			} else if (assign.getId().equals(testData.a4Id)) {
			    assertEquals(0, assign.getSortIndex());
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

		// start with instructor
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);

		// try passing an id that doesn't exist 
		try {
			assignmentLogic.getAssignmentByIdWithAssociatedData(12345L);
			fail("did not catch non-existent assignmentId passed to getAssignmentByIdWithAssociatedData");
		}
		catch (AssignmentNotFoundException anfe) {}

		// let's try to retrieve a graded item now
		Assignment2 assign = assignmentLogic.getAssignmentByIdWithAssociatedData(testData.a4Id);
		assertNotNull(assign);
		assertTrue(assign.getId().equals(testData.a4Id));

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

		// try passing an id that doesn't exist 
		try {
			assignmentLogic.getAssignmentByIdWithGroups(12345L);
			fail("did not catch non-existent assignmentId passed to getAssignmentByIdWithGroups");
		}
		catch (AssignmentNotFoundException anfe) {}

		// try a valid item
		Assignment2 assign = assignmentLogic.getAssignmentByIdWithGroups(testData.a1Id);
		assertTrue(assign.getId().equals(testData.a1Id));
		assertTrue(assign.getAssignmentGroupSet().size() == 2);	
	}

	public void testGetAssignmentByIdWithGroupsAndAttachments() throws Exception {
		// try passing a null id
		try {
			assignmentLogic.getAssignmentByIdWithGroupsAndAttachments(null);
			fail("Did not catch null assignment id passed to getAssignmentByIdWithGroupsAndAttachments");
		} catch (IllegalArgumentException iae) {}

		// try passing an id that doesn't exist 
		try {
			Assignment2 assign = assignmentLogic.getAssignmentByIdWithGroupsAndAttachments(12345L);
			fail("did not catch non-existent assignmentId passed to getAssignmentByIdWithGroupsAndAttachments");
		} catch (AssignmentNotFoundException anfe) {}

		// try a valid item
		Assignment2 assign = assignmentLogic.getAssignmentByIdWithGroupsAndAttachments(testData.a1Id);
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
		assignment.setGraded(false);

		// first, leave all of nullable fields null:
		// 	acceptUntilTime, dueDate

		// start with an open date in the past
		Calendar cal = Calendar.getInstance();
		cal.set(2005, 10, 01);
		assignment.setOpenDate(cal.getTime());

		// this should be open
		assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_OPEN);

		// set the open date in the future
		cal.set(2020, 10, 01);
		assignment.setOpenDate(cal.getTime());
		// should be not open
		assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_NOT_OPEN);

		// set it to draft - should be draft
		assignment.setDraft(true);
		assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_DRAFT);

		// set it back to draft=false, open date in the past, and make it due in the future
		assignment.setDraft(false);
		cal.set(2005, 10, 01);
		assignment.setOpenDate(cal.getTime());
		cal.set(2020, 10, 01);
		assignment.setDueDate(cal.getTime());
		assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_OPEN);
		// now make it due
		cal.set(2007, 10, 01);
		assignment.setDueDate(cal.getTime());
		assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_DUE);
		// add an accept until date in the future
		cal.set(2020, 10, 01);
		assignment.setAcceptUntilDate(cal.getTime());
		assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_DUE);
		// now make the accept until date in the past
		cal.set(2007, 10, 01);
		assignment.setAcceptUntilDate(cal.getTime());
		assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_CLOSED);

		// now make this graded with accept until in past
		assignment.setGraded(true);
		assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_CLOSED);

		// now check with accept until in future and no due date
		cal.set(2020, 10, 01);
		assignment.setAcceptUntilDate(cal.getTime());
		assignment.setDueDate(null);
		assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_OPEN);

		// add a due date in the past
		cal.set(2007, 10, 01);
		assignment.setDueDate(cal.getTime());
		assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_DUE);

		// double check that open date overrides other dates
		cal.set(2020, 10, 01);
		assignment.setOpenDate(cal.getTime());
		assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_NOT_OPEN);

		// double check that draft overrides everything
		assignment.setDraft(true);
		assertEquals(assignmentLogic.getStatusForAssignment(assignment), AssignmentConstants.STATUS_DRAFT);
	}

}
