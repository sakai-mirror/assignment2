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
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.test.AssignmentTestDataLoad;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentGroup;

import org.sakaiproject.section.api.coursemanagement.CourseSection;
import org.sakaiproject.section.api.coursemanagement.Course;
import org.sakaiproject.section.api.facade.Role;


public class AssignmentPermissionLogicTest extends Assignment2TestBase {

    private static final Log log = LogFactory.getLog(AssignmentPermissionLogicTest.class);

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
        List<String> sectionCategories = sectionAwareness.getSectionCategories(AssignmentTestDataLoad.CONTEXT_ID);
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
        
        // let's make assignment3 and assignment4 graded
    	testData.a3.setUngraded(false);
    	testData.a3.setGradableObjectId(gbItem1Id);
    	dao.save(testData.a3);
    	
    	testData.a4.setUngraded(false);
    	testData.a4.setGradableObjectId(gbItem2Id);
    	dao.save(testData.a4);
    	
    	// TODO add some grader permissions to a4!!!
    }

    public void testIsCurrentUserAbleToEditAssignments() {
    	// try passing a null contextId
    	try {
    		permissionLogic.isCurrentUserAbleToEditAssignments(null);
    		fail("did not catch null contextId passed to isCurrentUserAbleToEditAssignments");
    	} catch (IllegalArgumentException iae) {}
    	
    	// try a context that doesn't exist
    	assertFalse(permissionLogic.isCurrentUserAbleToEditAssignments(AssignmentTestDataLoad.BAD_CONTEXT));
    	
    	// only instructors should be able to edit assignments
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	assertTrue(permissionLogic.isCurrentUserAbleToEditAssignments(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	assertFalse(permissionLogic.isCurrentUserAbleToEditAssignments(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	assertFalse(permissionLogic.isCurrentUserAbleToEditAssignments(AssignmentTestDataLoad.CONTEXT_ID));
    }

   public void testIsUserAbleToViewStudentSubmissionForAssignment() {
	   // try passing a null studentId
	   try {
		   permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(null, new Assignment2());
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
	   authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
	   assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
			   AssignmentTestDataLoad.STUDENT1_UID, testData.a2));
	   assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
			   AssignmentTestDataLoad.STUDENT2_UID, testData.a2));
	   assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
			   AssignmentTestDataLoad.STUDENT3_UID, testData.a2));
	   // switch to TA
	   // ta may only view members in his/her section
	   authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
	   assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
			   AssignmentTestDataLoad.STUDENT1_UID, testData.a2));
	   assertFalse(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
			   AssignmentTestDataLoad.STUDENT2_UID, testData.a2));
	   assertFalse(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
			   AssignmentTestDataLoad.STUDENT3_UID, testData.a2));
	   
	   // now consider a graded assignment. with no grader perms, the same rules
	   // as above apply
	// instructor should be able to view any student
	   authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
	   assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
			   AssignmentTestDataLoad.STUDENT1_UID, testData.a3));
	   assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
			   AssignmentTestDataLoad.STUDENT2_UID, testData.a3));
	   assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
			   AssignmentTestDataLoad.STUDENT3_UID, testData.a3));
	   // switch to TA
	   // ta may only view members in his/her section
	   authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
	   assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
			   AssignmentTestDataLoad.STUDENT1_UID, testData.a3));
	   assertFalse(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
			   AssignmentTestDataLoad.STUDENT2_UID, testData.a3));
	   assertFalse(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(
			   AssignmentTestDataLoad.STUDENT3_UID, testData.a3));
	   
	   // TODO - check grader permissions!!
	   
	   // user may view their own submission
	   authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
	   assertTrue(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(AssignmentTestDataLoad.STUDENT1_UID, testData.a1));
	   // but they may not view others
	   assertFalse(permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(AssignmentTestDataLoad.STUDENT2_UID, testData.a1));
   }
   
   public void testIsUserAbleToProvideFeedbackForSubmission() {
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
	   authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
	   assertTrue(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
			   AssignmentTestDataLoad.STUDENT1_UID, testData.a2));
	   assertTrue(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
			   AssignmentTestDataLoad.STUDENT2_UID, testData.a2));
	   assertTrue(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
			   AssignmentTestDataLoad.STUDENT3_UID, testData.a2));
	   // switch to TA
	   // ta may only submit feedback for students in his/her section
	   authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
	   assertTrue(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
			   AssignmentTestDataLoad.STUDENT1_UID, testData.a2));
	   assertFalse(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
			   AssignmentTestDataLoad.STUDENT2_UID, testData.a2));
	   assertFalse(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
			   AssignmentTestDataLoad.STUDENT3_UID, testData.a2));
	   
	   // now consider a graded assignment. with no grader perms, the same rules
	   // as above apply
	   // instructor should be able to submit feedback for any student
	   authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
	   assertTrue(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
			   AssignmentTestDataLoad.STUDENT1_UID, testData.a3));
	   assertTrue(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
			   AssignmentTestDataLoad.STUDENT2_UID, testData.a3));
	   assertTrue(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
			   AssignmentTestDataLoad.STUDENT3_UID, testData.a3));
	   // switch to TA
	   // ta may only submit feedback for members in his/her section
	   authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
	   assertTrue(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
			   AssignmentTestDataLoad.STUDENT1_UID, testData.a3));
	   assertFalse(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
			   AssignmentTestDataLoad.STUDENT2_UID, testData.a3));
	   assertFalse(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(
			   AssignmentTestDataLoad.STUDENT3_UID, testData.a3));
	   
	   // TODO check a gb assignment with grader perms. use one with View only perm
	   
	   // students should not be able to submit feedback at all
	   authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
	   assertFalse(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(AssignmentTestDataLoad.STUDENT1_UID, testData.a1));
	   assertFalse(permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(AssignmentTestDataLoad.STUDENT2_UID, testData.a1));
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
	   authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
	   assertTrue(permissionLogic.isUserAbleToViewUngradedAssignment(testData.a1, null));
	   assertTrue(permissionLogic.isUserAbleToViewUngradedAssignment(testData.a2, null));
	   
	   // TA should only be able to see assignments that he/she is a member of if restricted
	   // otherwise, should see all

	   authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
	   List<String> memberships = externalLogic.getUserMembershipGroupIdList(AssignmentTestDataLoad.TA_UID, AssignmentTestDataLoad.CONTEXT_ID);
	   // try one that is restricted to a group that ta is a member of
	   assertTrue(permissionLogic.isUserAbleToViewUngradedAssignment(testData.a1, memberships));
	   // let's try that same one, but remove the membership
	   assertFalse(permissionLogic.isUserAbleToViewUngradedAssignment(testData.a1, new ArrayList<String>()));
	   // this one is not restricted, so should be ok
	   assertTrue(permissionLogic.isUserAbleToViewUngradedAssignment(testData.a2, memberships));
	   
	   // Students will see assignments available to site and those available to groups they
	   // are a member of
	   authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
	   memberships = externalLogic.getUserMembershipGroupIdList(AssignmentTestDataLoad.STUDENT1_UID, AssignmentTestDataLoad.CONTEXT_ID);
	   // student is a member of a restricted group, so ok
	   assertTrue(permissionLogic.isUserAbleToViewUngradedAssignment(testData.a1, memberships));
	   // let's try that same one, but remove the membership
	   assertFalse(permissionLogic.isUserAbleToViewUngradedAssignment(testData.a1, new ArrayList<String>()));
	   // this one is not restricted, so should be ok
	   assertTrue(permissionLogic.isUserAbleToViewUngradedAssignment(testData.a2, memberships));

   }
   
   public void testIsUserAMemberOfARestrictedGroup() {
	   List<String> groupMembershipIds = null;
	   List<AssignmentGroup> assignmentGroupSet = null;
	   // try with both null
	   assertFalse(permissionLogic.isUserAMemberOfARestrictedGroup(groupMembershipIds, assignmentGroupSet));
	   
	   // add a group to groupMembershipIds - should still be false
	   groupMembershipIds = new ArrayList<String>();
	   groupMembershipIds.add(section1Uid);
	   assertFalse(permissionLogic.isUserAMemberOfARestrictedGroup(groupMembershipIds, assignmentGroupSet));
	   
	   // add a different AssignmentGroup to the assignmentGroups
	   assignmentGroupSet = new ArrayList<AssignmentGroup>();
	   assignmentGroupSet.add(new AssignmentGroup(null, section2Uid));
	   assertFalse(permissionLogic.isUserAMemberOfARestrictedGroup(groupMembershipIds, assignmentGroupSet));
	   
	   // now add an overlapping section to group membership
	   groupMembershipIds.add(section2Uid);
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
	   authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
	   assertTrue(permissionLogic.isUserAbleToAccessInstructorView(AssignmentTestDataLoad.CONTEXT_ID));
	   
	   authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
	   assertTrue(permissionLogic.isUserAbleToAccessInstructorView(AssignmentTestDataLoad.CONTEXT_ID));
	   
	   authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
	   assertFalse(permissionLogic.isUserAbleToAccessInstructorView(AssignmentTestDataLoad.CONTEXT_ID));
   }
   
   public void testGetViewableStudentsForUserForItem() {
	   // try a null assignment
	   try {
		   permissionLogic.getViewableStudentsForUserForItem(null);
		   fail("did not catch null assignment passed to getViewableStudentsForUserForItem");
	   } catch(IllegalArgumentException iae) {}
	   
	   // what happens if we pass an assignment with null values?
	   authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
	   try {
		   permissionLogic.getViewableStudentsForUserForItem(new Assignment2());
		   fail("did not catch null values for not-null assignment fields");
	   } catch (IllegalArgumentException iae) {}
	   
	   // this method should throw a securityException if a student calls it
	   authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
	   try {
		   permissionLogic.getViewableStudentsForUserForItem(testData.a1);
		   fail("User without grading privileges was able to access getViewableStudentsForUserForItem!!");
	   } catch (SecurityException se) {}
	   
	   // Let's start with an ungraded item
	   authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
	   // instructor should get all students who have the assignment
	   // a1 is restricted to groups, so will return all students in those groups
	   List<String> viewableStudents = permissionLogic.getViewableStudentsForUserForItem(testData.a1);
	   assertTrue(viewableStudents.size() == 2);
	   // this one is not restricted
	   viewableStudents = permissionLogic.getViewableStudentsForUserForItem(testData.a2);
	   assertTrue(viewableStudents.size() == 3);
	   
	   // the ta should have restrictions on a1
	   authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
	   // should only get student 1 b/c may only see students in his/her section
	   viewableStudents = permissionLogic.getViewableStudentsForUserForItem(testData.a1);
	   assertTrue(viewableStudents.size() == 1);
	   // should still get 1 for a2
	   viewableStudents = permissionLogic.getViewableStudentsForUserForItem(testData.a2);
	   assertTrue(viewableStudents.size() == 1);
	   // let's add a group restriction to a2 and make sure no students are returned
	   AssignmentGroup groupFora2 = new AssignmentGroup(testData.a2, section3Uid);
	   dao.save(groupFora2);
	   // shouldn't get any student back now
	   viewableStudents = permissionLogic.getViewableStudentsForUserForItem(testData.a2);
	   assertTrue(viewableStudents.isEmpty());
	   
	   // now we will consider a graded item
	   // switch back to instructor
	   authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
	   // a3 is not restricted, so will return all students
	   viewableStudents = permissionLogic.getViewableStudentsForUserForItem(testData.a3);
	   assertTrue(viewableStudents.size() == 3);
	   // a4 is restricted to group 3
	   viewableStudents = permissionLogic.getViewableStudentsForUserForItem(testData.a4);
	   assertTrue(viewableStudents.size() == 1);
	   
	   // now switch to the ta
	   // TODO - GRADER PERMISSIONS!!
	   authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
	   // a3 should return all students in ta's sections
	   viewableStudents = permissionLogic.getViewableStudentsForUserForItem(testData.a3);
	   assertTrue(viewableStudents.size() == 1);
	   // a4 should not return any
	   viewableStudents = permissionLogic.getViewableStudentsForUserForItem(testData.a4);
	   assertTrue(viewableStudents.isEmpty());
   }
   
   public void testGetGradableStudentsForUserForItem() {
	   // try passing a null assignment
	   try {
		   permissionLogic.getGradableStudentsForUserForItem(null);
		   fail("did not catch null assignment passed to getGradableStudentsForUserForItem");
	   } catch(IllegalArgumentException iae) {}
	   
	   // this method should throw a securityException if a student calls it
	   authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
	   try {
		   permissionLogic.getGradableStudentsForUserForItem(testData.a1);
		   fail("User without grading privileges was able to access getGradableStudentsForUserForItem!!");
	   } catch (SecurityException se) {}
	   
	   // this method is exactly the same as getViewableStudentsForItem except
	   // if there are grader permission involved. this allows the instructor
	   // to restrict ta's to view-only instead of view and grade
	   // TODO - we must integrate grader permissions for this test to be accurate!
	// try a null assignment
	   try {
		   permissionLogic.getGradableStudentsForUserForItem(null);
		   fail("did not catch null assignment passed to getViewableStudentsForUserForItem");
	   } catch(IllegalArgumentException iae) {}
	   
	   // what happens if we pass an assignment with null values?
	   authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
	   try {
		   permissionLogic.getGradableStudentsForUserForItem(new Assignment2());
		   fail("did not catch null values for not-null assignment fields");
	   } catch (IllegalArgumentException iae) {}
	   
	   // Let's start with an ungraded item
	   authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
	   // instructor should get all students who have the assignment
	   // a1 is restricted to groups, so will return all students in those groups
	   List<String> gradableStudents = permissionLogic.getGradableStudentsForUserForItem(testData.a1);
	   assertTrue(gradableStudents.size() == 2);
	   // this one is not restricted
	   gradableStudents = permissionLogic.getGradableStudentsForUserForItem(testData.a2);
	   assertTrue(gradableStudents.size() == 3);
	   
	   // the ta should have restrictions on a1
	   authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
	   // should only get student 1 b/c may only see students in his/her section
	   gradableStudents = permissionLogic.getGradableStudentsForUserForItem(testData.a1);
	   assertTrue(gradableStudents.size() == 1);
	   // should still get 1 for a2
	   gradableStudents = permissionLogic.getGradableStudentsForUserForItem(testData.a2);
	   assertTrue(gradableStudents.size() == 1);
	   // let's add a group restriction to a2 and make sure no students are returned
	   AssignmentGroup groupFora2 = new AssignmentGroup(testData.a2, section3Uid);
	   dao.save(groupFora2);
	   // shouldn't get any student back now
	   gradableStudents = permissionLogic.getGradableStudentsForUserForItem(testData.a2);
	   assertTrue(gradableStudents.isEmpty());
	   
	   // now we will consider a graded item
	   // switch back to instructor
	   authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
	   // a3 is not restricted, so will return all students
	   gradableStudents = permissionLogic.getGradableStudentsForUserForItem(testData.a3);
	   assertTrue(gradableStudents.size() == 3);
	   // a4 is restricted to group 3
	   gradableStudents = permissionLogic.getGradableStudentsForUserForItem(testData.a4);
	   assertTrue(gradableStudents.size() == 1);
	   
	   // now switch to the ta
	   // TODO - GRADER PERMISSIONS!!
	   authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
	   // a3 should return all students in ta's sections
	   gradableStudents = permissionLogic.getGradableStudentsForUserForItem(testData.a3);
	   assertTrue(gradableStudents.size() == 1);
	   // a4 should not return any
	   gradableStudents = permissionLogic.getGradableStudentsForUserForItem(testData.a4);
	   assertTrue(gradableStudents.isEmpty());
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
	   authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
	   // should be able to submit for a1, a2, a3
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a1));
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a2));
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a3));
	   assertFalse(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a4));
	   
	   // student 2 is a member of group 3
	   authn.setAuthnContext(AssignmentTestDataLoad.STUDENT2_UID);
	   // should be able to submit for a1, a2, a3, a4
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a1));
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a2));
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a3));
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a4));
	   
	   // student 3 is not a member of any sections
	   authn.setAuthnContext(AssignmentTestDataLoad.STUDENT3_UID);
	   // should only be able to submit to 2,3
	   assertFalse(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a1));
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a2));
	   assertTrue(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a3));
	   assertFalse(permissionLogic.isUserAbleToMakeSubmissionForAssignment(AssignmentTestDataLoad.CONTEXT_ID, testData.a4));
   }
   
   public void testIsUserAllowedToReleaseFeedbackForAssignment() {
	   // try passing a null assignment
	   try {
		   permissionLogic.isUserAllowedToReleaseFeedbackForAssignment(null);
		   fail("Null assignment passed to isUserAllowedToReleaseFeedbackForAssignment was not caught");
	   } catch (IllegalArgumentException iae) {}
	   
	   // instructor should be true for all
	   authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
	   assertTrue(permissionLogic.isUserAllowedToReleaseFeedbackForAssignment(testData.a1));
	   assertTrue(permissionLogic.isUserAllowedToReleaseFeedbackForAssignment(testData.a2));
	   assertTrue(permissionLogic.isUserAllowedToReleaseFeedbackForAssignment(testData.a3));
	   assertTrue(permissionLogic.isUserAllowedToReleaseFeedbackForAssignment(testData.a4));
	   
	   // ta should be true for a1, a2, a3 - not auth to grade any students for a4
	   // b/c only avail to students in section3 and doesn't have grading perm for
	   // this section
	   // TODO grader permissions
	   authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
	   assertTrue(permissionLogic.isUserAllowedToReleaseFeedbackForAssignment(testData.a1));
	   assertTrue(permissionLogic.isUserAllowedToReleaseFeedbackForAssignment(testData.a2));
	   assertTrue(permissionLogic.isUserAllowedToReleaseFeedbackForAssignment(testData.a3));
	   assertFalse(permissionLogic.isUserAllowedToReleaseFeedbackForAssignment(testData.a4));
	   
	   // double check that students are all false
	   authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
	   assertFalse(permissionLogic.isUserAllowedToReleaseFeedbackForAssignment(testData.a1));
	   assertFalse(permissionLogic.isUserAllowedToReleaseFeedbackForAssignment(testData.a2));
	   assertFalse(permissionLogic.isUserAllowedToReleaseFeedbackForAssignment(testData.a3));
	   assertFalse(permissionLogic.isUserAllowedToReleaseFeedbackForAssignment(testData.a4));
   }

   public void testIsCurrentUserAbleToSubmit() {
	   // currently, only students defined by the gb may submit
	   authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
	   assertFalse(permissionLogic.isCurrentUserAbleToSubmit(AssignmentTestDataLoad.CONTEXT_ID));
	   authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
	   assertFalse(permissionLogic.isCurrentUserAbleToSubmit(AssignmentTestDataLoad.CONTEXT_ID));
	   
	   // now try the students
	   authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
	   assertTrue(permissionLogic.isCurrentUserAbleToSubmit(AssignmentTestDataLoad.CONTEXT_ID));
	   authn.setAuthnContext(AssignmentTestDataLoad.STUDENT2_UID);
	   assertTrue(permissionLogic.isCurrentUserAbleToSubmit(AssignmentTestDataLoad.CONTEXT_ID));
	   authn.setAuthnContext(AssignmentTestDataLoad.STUDENT3_UID);
	   assertTrue(permissionLogic.isCurrentUserAbleToSubmit(AssignmentTestDataLoad.CONTEXT_ID));
	   
	   // try a bogus context
	   assertFalse(permissionLogic.isCurrentUserAbleToSubmit(AssignmentTestDataLoad.BAD_CONTEXT));
   }
}
