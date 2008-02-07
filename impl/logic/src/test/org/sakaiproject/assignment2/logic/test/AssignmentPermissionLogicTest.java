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
import java.util.Arrays;
import java.util.Calendar;
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
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.exception.ConflictingAssignmentNameException;
import org.sakaiproject.assignment2.exception.NoGradebookItemForGradedAssignmentException;

import org.sakaiproject.section.api.coursemanagement.CourseSection;
import org.sakaiproject.section.api.coursemanagement.Course;
import org.sakaiproject.section.api.facade.Role;
import org.sakaiproject.service.gradebook.shared.Assignment;


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
   
   

}
