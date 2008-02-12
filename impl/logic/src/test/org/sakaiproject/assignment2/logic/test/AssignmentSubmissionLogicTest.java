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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import junit.framework.Assert;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.test.AssignmentTestDataLoad;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.SubmissionAttachment;

import org.sakaiproject.section.api.coursemanagement.CourseSection;
import org.sakaiproject.section.api.coursemanagement.Course;
import org.sakaiproject.section.api.facade.Role;


public class AssignmentSubmissionLogicTest extends Assignment2TestBase {

    private static final Log log = LogFactory.getLog(AssignmentSubmissionLogicTest.class);
    
    private static final Double st2a4Grade = new Double(40);
    private static final String st2a4Comment = "Good work";
    private static final Double st1a3Grade = new Double(25);
    private static final String st1a3Comment = "Mediocre work";
    
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
        // gb item 1 is not released to students yet!
        org.sakaiproject.service.gradebook.shared.Assignment gbItem1 = 
        	new org.sakaiproject.service.gradebook.shared.Assignment();
        gbItem1.setName(GB_ITEM1_NAME);
        gbItem1.setPoints(GB_ITEM1_PTS);
        gbItem1.setDueDate(GB_ITEM1_DUE);
        gbItem1.setReleased(false);
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
        
        // let's add some scores
        gradebookService.setAssignmentScore(AssignmentTestDataLoad.CONTEXT_ID, 
        		gbItem2.getName(), AssignmentTestDataLoad.STUDENT2_UID, st2a4Grade, "testing");
        gradebookService.setAssignmentScoreComment(AssignmentTestDataLoad.CONTEXT_ID, 
        		gbItem2.getName(), AssignmentTestDataLoad.STUDENT2_UID, st2a4Comment);
        gradebookService.setAssignmentScore(AssignmentTestDataLoad.CONTEXT_ID, 
        		gbItem1.getName(), AssignmentTestDataLoad.STUDENT1_UID, st1a3Grade, "testing");
        gradebookService.setAssignmentScoreComment(AssignmentTestDataLoad.CONTEXT_ID, 
        		gbItem1.getName(), AssignmentTestDataLoad.STUDENT1_UID, st1a3Comment);
        
        // let's make assignment3 and assignment4 graded
    	testData.a3.setUngraded(false);
    	testData.a3.setGradableObjectId(gbItem1Id);
    	dao.save(testData.a3);
    	
    	testData.a4.setUngraded(false);
    	testData.a4.setGradableObjectId(gbItem2Id);
    	dao.save(testData.a4);
    	
    	
    	// TODO add some grader permissions to a4!!!
    }

    public void testGetAssignmentSubmissionById() {
    	// try passing a null id
    	try {
    		submissionLogic.getAssignmentSubmissionById(null);
    		fail("did not catch null submissionId passed to getAssignmentSubmissionById");
    	} catch(IllegalArgumentException iae) {}
    	
    	// try passing an id that doesn't exist
    	assertNull(submissionLogic.getAssignmentSubmissionById(new Long(12345)));
    	
    	// the instructor should be able to retrieve any submission
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	AssignmentSubmission submission = submissionLogic.getAssignmentSubmissionById(testData.st1a1Submission.getId());
    	assertTrue(submission.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	submission = submissionLogic.getAssignmentSubmissionById(testData.st2a2SubmissionNoVersions.getId());
    	assertTrue(submission.getUserId().equals(AssignmentTestDataLoad.STUDENT2_UID));
    	submission = submissionLogic.getAssignmentSubmissionById(testData.st2a4Submission.getId());
    	assertTrue(submission.getUserId().equals(AssignmentTestDataLoad.STUDENT2_UID));
    	
    	// will throw SecurityException if currentUser isn't auth to view submission
    	// let's try a TA
    	// should be able to view student 1's submission
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	submission = submissionLogic.getAssignmentSubmissionById(testData.st1a1Submission.getId());
    	assertNotNull(submission);
    	assertTrue(submission.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	// should get a SecurityException trying to get st2
    	try {
    		submission = submissionLogic.getAssignmentSubmissionById(testData.st2a1Submission.getId());
    		fail("did not catch TA was trying to access a submission w/o authorization!");
    	} catch (SecurityException se) {}
    	
    	// student should be able to get their own
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	submission = submissionLogic.getAssignmentSubmissionById(testData.st1a1Submission.getId());
    	assertTrue(submission.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	// but not anyone elses!
    	try {
    		submission = submissionLogic.getAssignmentSubmissionById(testData.st2a1Submission.getId());
    		fail("did not catch a student trying to access another student's submission");
    	} catch (SecurityException se) {}
    }
    
    public void testGetSubmissionVersionById() {
    	// try a null versionId
    	try {
    		submissionLogic.getSubmissionVersionById(null);
    		fail("did not catch null versionId passed to getSubmissionVersionById");
    	} catch (IllegalArgumentException iae) {}
    	
    	// try a versionId that doesn't exist
    	assertNull(submissionLogic.getSubmissionVersionById(new Long(12345)));
    	
    	// instructors should be able to retrieve any version
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	AssignmentSubmissionVersion version = submissionLogic.getSubmissionVersionById(testData.st1a1CurrVersion.getId());
    	assertTrue(version.getAssignmentSubmission().getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	version = submissionLogic.getSubmissionVersionById(testData.st2a4CurrVersion.getId());
    	assertTrue(version.getAssignmentSubmission().getUserId().equals(AssignmentTestDataLoad.STUDENT2_UID));
    	// double check that gb info was populated
    	assertEquals(version.getAssignmentSubmission().getGradebookGrade(), st2a4Grade.toString());
    	assertEquals(version.getAssignmentSubmission().getGradebookComment(), st2a4Comment);
    	
    	// ta should be restricted
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	// let's start with ungraded - should be able to view student 1 but not student 2
    	version = submissionLogic.getSubmissionVersionById(testData.st1a1CurrVersion.getId());
    	assertTrue(version.getAssignmentSubmission().getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	// student 2 should throw SecurityException
    	try {
    		version = submissionLogic.getSubmissionVersionById(testData.st2a4CurrVersion.getId());
    		fail("Did not catch TA accessing st2 version w/o authorization");
    	} catch (SecurityException se) { }
    		
    	// let's try a graded item
    	// TODO grader perms 
    	version = submissionLogic.getSubmissionVersionById(testData.st1a3FirstVersion.getId());
    	assertTrue(version.getAssignmentSubmission().getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	assertTrue(version.getFeedbackAttachSet().isEmpty());
    	assertTrue(version.getSubmissionAttachSet().isEmpty());
    	// double check that gb info was populated
    	assertEquals(version.getAssignmentSubmission().getGradebookGrade(), st1a3Grade.toString());
    	assertEquals(version.getAssignmentSubmission().getGradebookComment(), st1a3Comment);
    	
    	// now let's double check that the ta can't see student submission details when it is
    	// draft...
    	version = submissionLogic.getSubmissionVersionById(testData.st1a3CurrVersion.getId());
    	assertTrue(version.getAssignmentSubmission().getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	assertTrue(version.getFeedbackAttachSet().size() == 1);
    	assertTrue(version.isDraft());
    	// make sure these are empty!
    	assertTrue(version.getSubmissionAttachSet().isEmpty());
    	assertTrue(version.getSubmittedText().equals(""));
    	
    	// double check that gb info was populated
    	assertEquals(version.getAssignmentSubmission().getGradebookGrade(), st1a3Grade.toString());
    	assertEquals(version.getAssignmentSubmission().getGradebookComment(), st1a3Comment);
    	
    	// now make sure ta can't see st 3
    	try {
    		version = submissionLogic.getSubmissionVersionById(testData.st3a3CurrVersion.getId());
    		fail("ta should not be able to access st3's submission for a3!");
    	} catch (SecurityException se) {}
    	
    	// TODO grader permissions
    	
    	// student may see their own submission
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	version = submissionLogic.getSubmissionVersionById(testData.st1a3CurrVersion.getId());
    	assertTrue(version.getAssignmentSubmission().getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	assertTrue(version.getFeedbackAttachSet().size() == 1);
    	// make sure these are populated! other users may not see this info b/c draft status
    	assertTrue(version.getSubmissionAttachSet().size() == 1);
    	assertTrue(!version.getSubmittedText().equals(""));
    	// since grade is not released, students should not see grade info
    	assertNull(version.getAssignmentSubmission().getGradebookComment());
    	assertNull(version.getAssignmentSubmission().getGradebookGrade());
    	
    	// double check that student can't see other users
    	try {
    		version = submissionLogic.getSubmissionVersionById(testData.st2a3CurrVersion.getId());
    		fail("st1 should not be able to access st2's submission for a3!");
    	} catch (SecurityException se) {}
    }
}
