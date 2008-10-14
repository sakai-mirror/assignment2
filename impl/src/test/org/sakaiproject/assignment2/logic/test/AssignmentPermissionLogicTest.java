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
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.test.AssignmentTestDataLoad;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentGroup;


public class AssignmentPermissionLogicTest extends Assignment2TestBase {

    private static final Log log = LogFactory.getLog(AssignmentPermissionLogicTest.class);

    /**
     * @see org.springframework.test.AbstractTransactionalSpringContextTests#onSetUpInTransaction()
     */
    protected void onSetUpInTransaction() throws Exception {
        super.onSetUpInTransaction();
    }

    public void testIsCurrentUserAbleToEditAssignments() {
    	// try passing a null contextId
    	try {
    		permissionLogic.isCurrentUserAbleToEditAssignments(null);
    		fail("did not catch null contextId passed to isCurrentUserAbleToEditAssignments");
    	} catch (IllegalArgumentException iae) {}
    	
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);

    	// try a context that doesn't exist
    	assertFalse(permissionLogic.isCurrentUserAbleToEditAssignments(AssignmentTestDataLoad.BAD_CONTEXT));
    	
    	// only instructors should be able to edit assignments
    	assertTrue(permissionLogic.isCurrentUserAbleToEditAssignments(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
    	assertFalse(permissionLogic.isCurrentUserAbleToEditAssignments(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
    	assertFalse(permissionLogic.isCurrentUserAbleToEditAssignments(AssignmentTestDataLoad.CONTEXT_ID));
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
	   // ta may only view members in his/her section
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
	   // ta may only view members in his/her section
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
	   // ta may only submit feedback for students in his/her section
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
	   // ta may only submit feedback for members in his/her section
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
	   // ta may only submit feedback for students in his/her section
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
	   // ta may only submit feedback for members in his/her section
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
   
   public void testIsUserAbleToViewUngradedAssignment() {
	   // try passing a null assignment
	   try {
		   permissionLogic.isUserAbleToViewUngradedAssignment(null, new ArrayList<String>());
		   fail("Did not catch null assignment passed to isUserAbleToViewUngradedAssignment");
	   } catch (IllegalArgumentException iae) {}
	   
	   // try passing a graded assignment
	   try {
		   permissionLogic.isUserAbleToViewUngradedAssignment(testData.a4, new ArrayList<String>());
		   fail("Did not catch graded assignment passed to isUserAbleToViewUngradedAssignment");
	   } catch (IllegalArgumentException iae) {}
	   
	   // instructors should be able to view all assignments 
	   externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
	   assertTrue(permissionLogic.isUserAbleToViewUngradedAssignment(testData.a1, null));
	   assertTrue(permissionLogic.isUserAbleToViewUngradedAssignment(testData.a2, null));
	   
	   // TA should only be able to see assignments that he/she is a member of if restricted
	   // otherwise, should see all

	   externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
	   List<String> memberships = externalLogic.getUserMembershipGroupIdList(AssignmentTestDataLoad.TA_UID, AssignmentTestDataLoad.CONTEXT_ID);
	   // try one that is restricted to a group that ta is a member of
	   assertTrue(permissionLogic.isUserAbleToViewUngradedAssignment(testData.a1, memberships));
	   // let's try that same one, but remove the membership
	   assertFalse(permissionLogic.isUserAbleToViewUngradedAssignment(testData.a1, new ArrayList<String>()));
	   // this one is not restricted, so should be ok
	   assertTrue(permissionLogic.isUserAbleToViewUngradedAssignment(testData.a2, memberships));
	   
	   // Students will see assignments available to site and those available to groups they
	   // are a member of
	   externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
	   memberships = externalLogic.getUserMembershipGroupIdList(AssignmentTestDataLoad.STUDENT1_UID, AssignmentTestDataLoad.CONTEXT_ID);
	   // student is a member of a restricted group, so ok
	   assertTrue(permissionLogic.isUserAbleToViewUngradedAssignment(testData.a1, memberships));
	   // let's try that same one, but remove the membership
	   assertFalse(permissionLogic.isUserAbleToViewUngradedAssignment(testData.a1, new ArrayList<String>()));
	   // this one is not restricted, so should be ok
	   assertTrue(permissionLogic.isUserAbleToViewUngradedAssignment(testData.a2, memberships));
	   
	   // let's set the open date to the future.  student shouldn't be able to view anymore
	   Calendar cal = Calendar.getInstance();
	   cal.set(2025, 10, 01);

	   testData.a1.setOpenDate(cal.getTime());
	   assertFalse(permissionLogic.isUserAbleToViewUngradedAssignment(testData.a1, memberships));

   }
   
   public void testIsUserAbleToViewGradedAssignment() {
	   // try passing a null assignment
	   try {
		   permissionLogic.isUserAbleToViewGradedAssignment(null, new ArrayList<String>());
		   fail("Did not catch null assignment passed to isUserAbleToViewGradedAssignment");
	   } catch (IllegalArgumentException iae) {}
	   
	   // try passing an ungraded assignment
	   try {
		   permissionLogic.isUserAbleToViewGradedAssignment(testData.a1, new ArrayList<String>());
		   fail("Did not catch graded assignment passed to isUserAbleToViewGradedAssignment");
	   } catch (IllegalArgumentException iae) {}
	   
	   // gb item 1 is not released - assoc with a3
	   // a4 is restricted to group 3
	   
	   // instructors should be able to view all assignments 
	   externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
	   assertTrue(permissionLogic.isUserAbleToViewGradedAssignment(testData.a3, null));
	   assertTrue(permissionLogic.isUserAbleToViewGradedAssignment(testData.a4, null));
	   
	   // TAs can view all since there are no grader perms

	   externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
	   assertTrue(permissionLogic.isUserAbleToViewGradedAssignment(testData.a3, null));
	   assertTrue(permissionLogic.isUserAbleToViewGradedAssignment(testData.a4, null));
	   
	   // Students will see assignments available to site and those available to groups they
	   // are a member of. assoc gb item must be released and assign open
	   externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
	   List<String> memberships = externalLogic.getUserMembershipGroupIdList(AssignmentTestDataLoad.STUDENT1_UID, AssignmentTestDataLoad.CONTEXT_ID);
	   // student is not a member of the restricted group, so cannot view
	   assertFalse(permissionLogic.isUserAbleToViewGradedAssignment(testData.a4, memberships));
	   // this gb item hasn't been released yet
	   assertFalse(permissionLogic.isUserAbleToViewGradedAssignment(testData.a3, memberships));
	   
	   // switch to student who is a member of group 3
	   externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT2_UID);
	   memberships = externalLogic.getUserMembershipGroupIdList(AssignmentTestDataLoad.STUDENT2_UID, AssignmentTestDataLoad.CONTEXT_ID);
	   
	   assertTrue(permissionLogic.isUserAbleToViewGradedAssignment(testData.a4, memberships));
	   
	   // let's set the open date to the future.  student shouldn't be able to view anymore
	   Calendar cal = Calendar.getInstance();
	   cal.set(2025, 10, 01);

	   testData.a4.setOpenDate(cal.getTime());
	   assertFalse(permissionLogic.isUserAbleToViewGradedAssignment(testData.a4, memberships));

   }
   
   public void testIsUserAbleToViewAssignment() {
	   // try passing a null contextId
	   try {
		   permissionLogic.isUserAbleToViewAssignment(null, 12345L);
		   fail("Did not catch null contextId passed to isUserAbleToViewAssignment");
	   } catch (IllegalArgumentException iae) {}
	   
	   // try passing a null assignmentId
	   try {
		   permissionLogic.isUserAbleToViewAssignment(AssignmentTestDataLoad.CONTEXT_ID, null);
		   fail("Did not catch null assignmentId passed to isUserAbleToViewAssignment");
	   } catch (IllegalArgumentException iae) {}
	   
	   // first try ungraded assignments
	   
	   // instructors should be able to view all assignments 
	   externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
	   assertTrue(permissionLogic.isUserAbleToViewAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a1Id));
	   assertTrue(permissionLogic.isUserAbleToViewAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a2Id));
	   
	   // TA should only be able to see assignments that he/she is a member of if restricted
	   // otherwise, should see all

	   externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);   
	   // try one that is restricted to a group that ta is a member of
	   assertTrue(permissionLogic.isUserAbleToViewAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a1Id));
	   // this one is not restricted, so should be ok
	   assertTrue(permissionLogic.isUserAbleToViewAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a2Id));
	   
	   // Students will see assignments available to site and those available to groups they
	   // are a member of
	   externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
	   // student is a member of a restricted group, so ok
	   assertTrue(permissionLogic.isUserAbleToViewAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a1Id));
	   // this one is not restricted, so should be ok
	   assertTrue(permissionLogic.isUserAbleToViewAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a2Id));
	   
	   // let's set the open date to the future.  student shouldn't be able to view anymore
	   Calendar cal = Calendar.getInstance();
	   cal.set(2025, 10, 01);

	   // re-retrieve this assignment
	   testData.a1 = (Assignment2)dao.findById(Assignment2.class, testData.a1Id);
	   testData.a1.setOpenDate(cal.getTime());
	   dao.save(testData.a1);
	   assertFalse(permissionLogic.isUserAbleToViewAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a1Id));
	   
	   // now test some graded assignments
	   // gb item 1 is not released - assoc with a3
	   // a4 is restricted to group 3
	   
	   // instructors should be able to view all assignments 
	   externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
	   assertTrue(permissionLogic.isUserAbleToViewAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a3Id));
	   assertTrue(permissionLogic.isUserAbleToViewAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a4Id));
	   
	   // TAs can view all since there are no grader perms

	   externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
	   assertTrue(permissionLogic.isUserAbleToViewAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a3Id));
	   assertTrue(permissionLogic.isUserAbleToViewAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a4Id));
	   
	   // Students will see assignments available to site and those available to groups they
	   // are a member of. assoc gb item must be released and assign open
	   externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
	   // student is not a member of the restricted group, so cannot view
	   assertFalse(permissionLogic.isUserAbleToViewAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a4Id));
	   // this gb item hasn't been released yet
	   assertFalse(permissionLogic.isUserAbleToViewAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a3Id));
	   
	   // switch to student who is a member of group 3
	   externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT2_UID);
	   assertTrue(permissionLogic.isUserAbleToViewAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a4Id));
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
	   
	   // now add an overlapping section to group membership
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
	   // should only get student 1 b/c may only see students in his/her section
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
	   // a3 should return all students in ta's sections
	   viewableStudents = permissionLogic.getViewableStudentsForUserForItem(AssignmentTestDataLoad.TA_UID, testData.a3);
	   assertEquals(1, viewableStudents.size());
	   // a4 should not return any
	   viewableStudents = permissionLogic.getViewableStudentsForUserForItem(AssignmentTestDataLoad.TA_UID, testData.a4);
	   assertTrue(viewableStudents.isEmpty());
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
	   // should only get student 1 b/c may only see students in his/her section
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

	   // a3 should return all students in ta's sections
	   gradableStudents = permissionLogic.getGradableStudentsForUserForItem(AssignmentTestDataLoad.TA_UID, testData.a3);
	   assertEquals(1, gradableStudents.size());
	   // a4 should not return any
	   gradableStudents = permissionLogic.getGradableStudentsForUserForItem(AssignmentTestDataLoad.TA_UID, testData.a4);
	   assertEquals(0, gradableStudents.size());
   }
   
   public void testIsUserAbleToMakeSubmissionForAssignment() {
	   // try passing a null contextId
	   try {
		   permissionLogic.isUserAbleToMakeSubmissionForAssignment(null, new Assignment2());
		   fail("did not catch null contextId passed to isUserAbleToMakeSubmissionForAssignment");
	   } catch (IllegalArgumentException iae) {}
	   
	   // try passing a null assignment
	   try {
		   permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, null);
		   fail("did not catch null assignment passed to isUserAbleToMakeSubmissionForAssignment");
	   } catch (IllegalArgumentException iae) {}
	   
	   // try passing an empty assignment
	   try {
		   permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, new Assignment2());
		   fail("did not catch null fields assoc with assignment passed to isUserAbleToMakeSubmissionForAssignment");
	   } catch (IllegalArgumentException iae) {}
	   
	   // TODO - we need to define "who" can submit, so will need to update the tests
	   // currently there is no check on whether you are a student, guest, instructor, etc
	   // let's just test students for now
	   // student 1 is a member of group 1
	   externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
	   // should be able to submit for a1, a2, a3
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a1));
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a2));
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a3));
	   assertFalse(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a4));
	   
	   // student 2 is a member of group 3
	   externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT2_UID);
	   // should be able to submit for a1, a2, a3, a4
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a1));
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a2));
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a3));
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a4));
	   
	   // student 3 is not a member of any sections
	   externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT3_UID);
	   // should only be able to submit to 2,3
	   assertFalse(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a1));
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a2));
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a3));
	   assertFalse(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a4));
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
	   // b/c only avail to students in section3 and doesn't have grading perm for
	   // this section
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
}
