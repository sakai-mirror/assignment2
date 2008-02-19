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
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import junit.framework.Assert;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.test.AssignmentTestDataLoad;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.utils.ComparatorsUtils;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
import org.sakaiproject.assignment2.model.FeedbackAttachment;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;

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
    	submission = submissionLogic.getAssignmentSubmissionById(testData.st1a3Submission.getId());
    	assertNotNull(submission);
    	assertTrue(submission.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	// but shouldn't see submission details b/c still draft
    	assertTrue(submission.getCurrentSubmissionVersion().getSubmittedText().equals(""));
    	assertTrue(submission.getCurrentSubmissionVersion().getSubmissionAttachSet().isEmpty());
    	
    	// should get a SecurityException trying to get st2
    	try {
    		submission = submissionLogic.getAssignmentSubmissionById(testData.st2a1Submission.getId());
    		fail("did not catch TA was trying to access a submission w/o authorization!");
    	} catch (SecurityException se) {}
    	
    	// student should be able to get their own
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	submission = submissionLogic.getAssignmentSubmissionById(testData.st1a3Submission.getId());
    	assertTrue(submission.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	// double check submission info is populated
    	assertTrue(submission.getCurrentSubmissionVersion().getSubmissionAttachSet().size() == 1);
    	assertFalse(submission.getCurrentSubmissionVersion().getSubmittedText().equals(""));
    	
    	// double check feedback is empty since not released yet
    	assertTrue(submission.getCurrentSubmissionVersion().getAnnotatedText().equals(""));
    	assertTrue(submission.getCurrentSubmissionVersion().getFeedbackAttachSet().isEmpty());
    	assertTrue(submission.getCurrentSubmissionVersion().getFeedbackNotes().equals(""));
    	// student should not be able to get other student
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
    	// feedback was not released, so double check that it was not populated
    	assertTrue(version.getFeedbackAttachSet().isEmpty());
    	assertTrue(version.getFeedbackNotes().equals(""));
    	assertTrue(version.getAnnotatedText().equals(""));
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
    
    public void testGetCurrentSubmissionByAssignmentIdAndStudentId() {
    	// try passing a null assignmentId
    	try {
    		submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(null, AssignmentTestDataLoad.STUDENT1_UID);
    		fail("did not handle null assignmentId passed to testGetCurrentSubmissionByAssignmentIdAndStudentId");
    	} catch (IllegalArgumentException iae) {}
    	// try passing a null studentId
    	try {
    		submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(testData.a1Id, null);
    		fail("did not handle null studentId passed to testGetCurrentSubmissionByAssignmentIdAndStudentId");
    	} catch (IllegalArgumentException iae) {}
    	
    	// pass an assignmentId that doesn't exist - should return null
    	AssignmentSubmission submission = submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(
    			new Long(12345), AssignmentTestDataLoad.STUDENT1_UID);
    	assertNull(submission);
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	
    	// try to get one for a student who hasn't made a submission yet
    	submission = submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(testData.a2Id, AssignmentTestDataLoad.STUDENT1_UID);
    	assertTrue(submission.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	assertNull(submission.getId()); // this should be an "empty rec"
    	
    	// try one for a student with multiple versions
    	submission = submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(testData.a1Id, AssignmentTestDataLoad.STUDENT2_UID);
    	assertTrue(submission.getUserId().equals(AssignmentTestDataLoad.STUDENT2_UID));
    	assertTrue(submission.getCurrentSubmissionVersion().getId().equals(testData.st2a1CurrVersion.getId()));
    	
    	// get a currentVersion that is draft and make sure submission info is not populated
    	submission = submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(testData.a3Id, AssignmentTestDataLoad.STUDENT1_UID);
    	assertTrue(submission.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	assertTrue(submission.getCurrentSubmissionVersion().getId().equals(testData.st1a3CurrVersion.getId()));
    	assertTrue(submission.getCurrentSubmissionVersion().getSubmissionAttachSet().isEmpty());
    	assertTrue(submission.getCurrentSubmissionVersion().getSubmittedText().equals(""));
    	// check that grading info is included
    	assertTrue(submission.getGradebookComment().equals(st1a3Comment));
    	assertTrue(submission.getGradebookGrade().equals(st1a3Grade.toString()));
    	
    	// what if the student is not part of the passed assignment?
    	// ie it is restricted to groups that the student is not a member of
    	
    	// now, switch to a ta
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	// should only have access to student 1 b/c in group 1
    	submission = submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(testData.a1Id, AssignmentTestDataLoad.STUDENT1_UID);
    	assertTrue(submission.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	assertTrue(submission.getCurrentSubmissionVersion().getId().equals(testData.st1a1CurrVersion.getId()));
    	
    	try {
    		submission = submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(testData.a1Id, AssignmentTestDataLoad.STUDENT2_UID);
    		fail("Ta should not have authorization to retrieve submission for st2 through getCurrentSubmissionByAssignmentIdAndStudentId");
    	} catch (SecurityException se) {}
    	
    	// graded assignments
    	// get a currentVersion that is draft and make sure submission info is not populated
    	submission = submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(testData.a3Id, AssignmentTestDataLoad.STUDENT1_UID);
    	assertTrue(submission.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	assertTrue(submission.getCurrentSubmissionVersion().getId().equals(testData.st1a3CurrVersion.getId()));
    	assertTrue(submission.getCurrentSubmissionVersion().getSubmissionAttachSet().isEmpty());
    	assertTrue(submission.getCurrentSubmissionVersion().getSubmittedText().equals(""));
    	// check that grading info is included
    	assertTrue(submission.getGradebookComment().equals(st1a3Comment));
    	assertTrue(submission.getGradebookGrade().equals(st1a3Grade.toString()));
    	
    	// TODO grader permissions
    	
    	// now switch to a student
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	// should be able to retrieve own submission
    	submission = submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(testData.a3Id, AssignmentTestDataLoad.STUDENT1_UID);
    	assertTrue(submission.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	assertTrue(submission.getCurrentSubmissionVersion().getId().equals(testData.st1a3CurrVersion.getId()));
    	// this is a draft, so check that the submission info is populated for student
    	assertTrue(submission.getCurrentSubmissionVersion().getSubmissionAttachSet().size() == 1);
    	assertFalse(submission.getCurrentSubmissionVersion().getSubmittedText().equals(""));
    	// since grade is not released, students should not see grade info
    	assertNull(submission.getGradebookComment());
    	assertNull(submission.getGradebookGrade());
    	
    	// double check that student can't view others
    	try {
    		submission = submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(testData.a1Id, AssignmentTestDataLoad.STUDENT2_UID);
    		fail("Did not catch student accessing another student via getCurrentSubmissionByAssignmentIdAndStudentId");
    	} catch (SecurityException se) {}
    }
    
    public void testSaveStudentSubmission() {
    	// try passing a null userId
    	try {
    		submissionLogic.saveStudentSubmission(null, new Assignment2(), true, null, null);
    		fail("Did not catch null userId passed to saveStudentSubmission");
    	} catch (IllegalArgumentException iae) {}
    	
    	// try passing a null assignment
    	try {
    		submissionLogic.saveStudentSubmission(AssignmentTestDataLoad.STUDENT1_UID, null, true, null, null);
    		fail("Did not catch null assignment passed to saveStudentSubmission");
    	} catch (IllegalArgumentException iae) {}
    	
    	// try passing a null draft status
    	try {
    		submissionLogic.saveStudentSubmission(AssignmentTestDataLoad.STUDENT1_UID, 
    				new Assignment2(), null, null, null);
    		fail("Did not catch null draft status passed to saveStudentSubmission");
    	} catch (IllegalArgumentException iae) {}
    	
    	// try passing an empty assignment (with no id)
    	try {
    		submissionLogic.saveStudentSubmission(AssignmentTestDataLoad.STUDENT1_UID, 
    				new Assignment2(), null, null, null);
    	} catch (IllegalArgumentException iae) {}
    	
    	// let's see if an instructor can make a submission for a student
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	try {
    		submissionLogic.saveStudentSubmission(AssignmentTestDataLoad.STUDENT1_UID,
    				testData.a1, false, null, null);
    		fail("did not catch instructor trying to save a student's submission via saveStudentSubmission");
    	} catch (SecurityException se) {}
    	
    	// try the ta
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	try {
    		submissionLogic.saveStudentSubmission(AssignmentTestDataLoad.STUDENT1_UID,
    				testData.a1, false, null, null);
    		fail("did not catch ta trying to save a student's submission via saveStudentSubmission");
    	} catch (SecurityException se) {}
    	
    	// scenarios
    	// student tries to save for assignment that he/she is not part of
    	// student creates a draft
    	// student edits the draft and submits it
    	// student creates new submission where there is no prev submission
    	// student resubmits when ok
    	// student resubmits but is not allowed
    	
    	// student 1 does not have any submission for a2 yet
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	// double check that no submission exists yet
    	List subList = dao.findByProperties(
    			AssignmentSubmission.class, new String[] {"userId", "assignment"}, 
    			new Object[] {AssignmentTestDataLoad.STUDENT1_UID, testData.a2});
    	assertTrue(subList.isEmpty());
    	
    	SubmissionAttachment attach1 = new SubmissionAttachment();
    	attach1.setAttachmentReference("ref1");
    	SubmissionAttachment attach2 = new SubmissionAttachment();
    	attach2.setAttachmentReference("ref2");
    	Set attachSet = new HashSet();
    	attachSet.add(attach1);
    	attachSet.add(attach2);
    	
    	submissionLogic.saveStudentSubmission(AssignmentTestDataLoad.STUDENT1_UID, 
    			testData.a2, true, "this is my text", attachSet);
    	
    	// now check that it exists 
    	subList = dao.findByProperties(
    			AssignmentSubmission.class, new String[] {"userId", "assignment"}, 
    			new Object[] {AssignmentTestDataLoad.STUDENT1_UID, testData.a2});
    	assertFalse(subList.isEmpty());
    	AssignmentSubmission existingSub = (AssignmentSubmission)subList.get(0);
    	Long subId = existingSub.getId();
    	
    	assertNotNull(existingSub);
    	AssignmentSubmissionVersion currVersion = dao.getCurrentSubmissionVersionWithAttachments(existingSub);
    	assertTrue(currVersion.isDraft());
    	//assertTrue(currVersion.getSubmissionAttachSet().size() == 2);
    	
    	// now let's try to edit this version but keep it draft
    	// should not create a new version
    	attachSet.remove(attach2);
    	submissionLogic.saveStudentSubmission(AssignmentTestDataLoad.STUDENT1_UID, 
    			testData.a2, true, "this is my text - revised!", attachSet);
    	// text and attach should have been updated
    	existingSub = (AssignmentSubmission)dao.findById(AssignmentSubmission.class, subId);
    	Set versionHistory = dao.getVersionHistoryForSubmission(existingSub);
    	assertTrue(versionHistory.size() == 1);
    	currVersion = dao.getCurrentSubmissionVersionWithAttachments(existingSub);
    	//assertTrue(currVersion.getSubmissionAttachSet().size() == 1);
    	
    	// now let's actually submit it (make draft = false)
    	submissionLogic.saveStudentSubmission(AssignmentTestDataLoad.STUDENT1_UID, 
    			testData.a2, false, "this is my text - revised!", currVersion.getSubmissionAttachSet());
    	existingSub = (AssignmentSubmission)dao.findById(AssignmentSubmission.class, subId);
    	versionHistory = dao.getVersionHistoryForSubmission(existingSub);
    	assertTrue(versionHistory.size() == 1);
    	
    	// next time, the student should get an error b/c not allowed to resubmit
    	try {
    		submissionLogic.saveStudentSubmission(AssignmentTestDataLoad.STUDENT1_UID, 
        			testData.a2, false, "this is my text - revised!", currVersion.getSubmissionAttachSet());
    		fail("submission saved even though not allowed to resubmit!");
    	} catch (SecurityException se) {}
    	
    	// allow student to submit one more time
    	existingSub.setNumSubmissionsAllowed(new Integer(2));
    	dao.save(existingSub);

    	submissionLogic.saveStudentSubmission(AssignmentTestDataLoad.STUDENT1_UID, 
    			testData.a2, false, "this is my newly submitted version", null);
    	versionHistory = dao.getVersionHistoryForSubmission(existingSub);
    	assertTrue(versionHistory.size() == 2);
    	
    	// what if student is not allowed to submit to a restricted assignment?
    	try {
    		submissionLogic.saveStudentSubmission(AssignmentTestDataLoad.STUDENT1_UID, testData.a4, true, null, null);
    		fail("did not catch student making submission to assignment that is restricted");
    	} catch(SecurityException se) {}
    }
    
    public void testSaveInstructorFeedback() {
    	// try a null studentId
    	try {
    		submissionLogic.saveInstructorFeedback(testData.st1a1CurrVersion.getId(), null, testData.a1, 
    				null, null, null, null, null, null);
    		fail("did not catch null studentId passed to saveInstructorFeedback");
    	} catch (IllegalArgumentException iae) {}
    	// try a null assignment
    	try {
    		submissionLogic.saveInstructorFeedback(testData.st1a1CurrVersion.getId(), 
    				AssignmentTestDataLoad.STUDENT1_UID, null, null, null, null, null, null, null);
    		fail("did not catch null assignment passed to saveInstructorFeedback");
    	} catch (IllegalArgumentException iae) {}
    	
    	// try a versionId that doesn't exist
    	try {
    		submissionLogic.saveInstructorFeedback(new Long(12345), AssignmentTestDataLoad.STUDENT1_UID, 
    				testData.a2, null, null, null, null, null, null);
    		fail("did not catch passed versionId that does not exist to saveInstructorFeedback");
    	} catch (IllegalArgumentException iae) {}
    	
    	// try a studentId not associated with the passed version
    	try {
    		submissionLogic.saveInstructorFeedback(testData.st1a1CurrVersion.getId(), AssignmentTestDataLoad.STUDENT2_UID, 
    				testData.a1, null, null, null, null, null, null);
    		fail("did not catch passed studentId not associated with the given versionId");
    	} catch (IllegalArgumentException iae) {}
    	
    	// try an assignment not associated with the passed version
    	try {
    		submissionLogic.saveInstructorFeedback(testData.st1a1CurrVersion.getId(), AssignmentTestDataLoad.STUDENT1_UID, 
    				testData.a2, null, null, null, null, null, null);
    		fail("did not catch passed assignment not associated with the given versionId");
    	} catch (IllegalArgumentException iae) {}
    	
    	// try a null version when submission already exists
    	try {
    		submissionLogic.saveInstructorFeedback(null, AssignmentTestDataLoad.STUDENT1_UID, 
    				testData.a1, null, null, null, null, null, null);
    		fail("did not catch null versionId even though submission exists for given student and assignment");
    	} catch (IllegalArgumentException iae) {}
    	
    	// start as an instructor
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	
    	// submit feedback for student w/o submission
    	// student 1 has not submitted for a2 yet
    	Set feedbackAttachSet = new HashSet();
    	FeedbackAttachment fb1 = new FeedbackAttachment(null, "fb1");
    	feedbackAttachSet.add(fb1);

    	submissionLogic.saveInstructorFeedback(null, AssignmentTestDataLoad.STUDENT1_UID,
    			testData.a2, 1, null, null, "Please submit this soon!", new Date(), feedbackAttachSet);
    	// try to retrieve it now
    	AssignmentSubmission st1a2Submission = dao.getSubmissionWithVersionHistoryForStudentAndAssignment(AssignmentTestDataLoad.STUDENT1_UID, testData.a2);
    	assertTrue(st1a2Submission.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	Long st1a2SubId = st1a2Submission.getId();
    	assertNotNull(st1a2SubId);
    	
    	AssignmentSubmissionVersion st1a2CurrVersion = dao.getCurrentSubmissionVersionWithAttachments(st1a2Submission);
    	assertNotNull(st1a2CurrVersion);
    	Long st1a2CurrVersionId = st1a2CurrVersion.getId();
    	
    	// let's try to re-save this one without the attachments
    	submissionLogic.saveInstructorFeedback(st1a2CurrVersionId, AssignmentTestDataLoad.STUDENT1_UID, 
    			testData.a2, 1, null, null, "Revised feedback", new Date(), null);
    	st1a2Submission = (AssignmentSubmission)dao.findById(AssignmentSubmission.class, st1a2SubId);
    	assertTrue(st1a2Submission.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	
    	// make sure there is still only one version
    	Set versionHistory = dao.getVersionHistoryForSubmission(st1a2Submission);
    	assertTrue(versionHistory.size() == 1);
    	
    	// try a TA
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	// should not be allowed to grade student 2
    	try {
    		submissionLogic.saveInstructorFeedback(testData.st2a1CurrVersion.getId(), 
    				AssignmentTestDataLoad.STUDENT2_UID, testData.a1, 2, null, null, null, null, null);
    		fail("Did not catch SecurityException when TA attempted to grade unauth student!");
    	} catch (SecurityException se) {}
    	
    	// should be allowed to grade st1 for a1
    	// make sure versions aren't added
    	Date resubmitCloseDate = new Date();
    	submissionLogic.saveInstructorFeedback(testData.st1a1CurrVersion.getId(), 
				AssignmentTestDataLoad.STUDENT1_UID, testData.a1, 2, resubmitCloseDate, "annotated fb", "notes", null, feedbackAttachSet);
    	versionHistory = dao.getVersionHistoryForSubmission(testData.st1a1Submission);
    	assertEquals(versionHistory.size(), 1);
    	AssignmentSubmissionVersion currVersion = dao.getCurrentSubmissionVersionWithAttachments(testData.st1a1Submission);
    	assertTrue(currVersion.getFeedbackNotes().equals("notes"));
    	assertTrue(currVersion.getAssignmentSubmission().getNumSubmissionsAllowed().equals(new Integer(2)));
    	assertTrue(currVersion.getAssignmentSubmission().getResubmitCloseTime().equals(resubmitCloseDate));
    	
    	// student should not be authorized
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	try {
    		submissionLogic.saveInstructorFeedback(testData.st1a1CurrVersion.getId(), 
    				AssignmentTestDataLoad.STUDENT1_UID, testData.a1, 2, null, null, null, null, null);
    		fail("Student was able to saveInstructorFeedback without authorization!!!");
    	} catch (SecurityException se) {}
    }
    
    public void testGetViewableSubmissionsForAssignmentId() {
    	// try a null assignmentId
    	try {
    		submissionLogic.getViewableSubmissionsForAssignmentId(null);
    		fail("did not catch null assignmentId passed to getViewableSubmissionsForAssignmentId");
    	} catch (IllegalArgumentException iae) {}
    	
    	// start as instructor
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	// we should get 2 students back for a1 b/c group restrictions
    	List subList = submissionLogic.getViewableSubmissionsForAssignmentId(testData.a1Id);
    	assertTrue(subList.size() == 2);
    	// we should get 3 for a2 b/c no restrictions
    	subList = submissionLogic.getViewableSubmissionsForAssignmentId(testData.a2Id);
    	assertTrue(subList.size() == 3);
    	// we should get 3 for a3 b/c no restrictions
    	subList = submissionLogic.getViewableSubmissionsForAssignmentId(testData.a3Id);
    	assertTrue(subList.size() == 3);
    	// let's make sure the submission for st1 is restricted b/c draft
    	for (Iterator it=subList.iterator(); it.hasNext();) {
    		AssignmentSubmission sub = (AssignmentSubmission) it.next();
    		if (sub.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID)) {
    			assertEquals(sub.getCurrentSubmissionVersion().getSubmittedText(), "");
    			assertTrue(sub.getCurrentSubmissionVersion().getSubmissionAttachSet().isEmpty());
    		}
    	}
    	// we should get 1 for a4 b/c group restrictions
    	subList = submissionLogic.getViewableSubmissionsForAssignmentId(testData.a4Id);
    	assertTrue(subList.size() == 1);
    	
    	// now become ta
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	// we should get 1 student back for a1 b/c only allowed to view group 1
    	subList = submissionLogic.getViewableSubmissionsForAssignmentId(testData.a1Id);
    	assertTrue(subList.size() == 1);
    	// we should still get 1 for a2 b/c no group restrictions for this assign
    	subList = submissionLogic.getViewableSubmissionsForAssignmentId(testData.a2Id);
    	assertTrue(subList.size() == 1);
    	// we should still get 1 for a2 b/c no group restrictions for this assign
    	subList = submissionLogic.getViewableSubmissionsForAssignmentId(testData.a3Id);
    	assertTrue(subList.size() == 1);
    	//TODO grader permissions
    	// should return no students
    	subList = submissionLogic.getViewableSubmissionsForAssignmentId(testData.a4Id);
    	assertTrue(subList.isEmpty());
    	
    	// students should get SecurityException
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	try {
    		subList = submissionLogic.getViewableSubmissionsForAssignmentId(testData.a1Id);
    		fail("Did not catch student attempting to access submissions via getViewableSubmissionsForAssignmentId");
    	} catch (SecurityException se) {}

    }
    
    public void testSetSubmissionStatusConstantForAssignments() {
    	// try null studentId
    	try {
    		submissionLogic.setSubmissionStatusConstantForAssignments(new ArrayList(), null);
    		fail("Did not catch null studentId passed to setSubmissionStatusForAssignments");
    	} catch(IllegalArgumentException iae) {}
    	
    	// try a null assignment list
    	// should do nothing
    	submissionLogic.setSubmissionStatusConstantForAssignments(null, AssignmentTestDataLoad.STUDENT1_UID);
    	
    	// let's create a list of assignments
    	List assignList = new ArrayList();
    	assignList.add(testData.a1);
    	assignList.add(testData.a2);
    	assignList.add(testData.a3);
    	assignList.add(testData.a4);
    	
    	submissionLogic.setSubmissionStatusConstantForAssignments(assignList, AssignmentTestDataLoad.STUDENT1_UID);
    	Assignment2 assign1 = (Assignment2) assignList.get(0);
    	assertTrue(assign1.getSubmissionStatusConstant().equals(AssignmentConstants.SUBMISSION_SUBMITTED));
    	
    	Assignment2 assign2 = (Assignment2) assignList.get(1);
    	assertTrue(assign2.getSubmissionStatusConstant().equals(AssignmentConstants.SUBMISSION_NOT_STARTED));
    	
    	Assignment2 assign3 = (Assignment2) assignList.get(2);
    	assertTrue(assign3.getSubmissionStatusConstant().equals(AssignmentConstants.SUBMISSION_IN_PROGRESS));
    	
    	Assignment2 assign4 = (Assignment2) assignList.get(3);
    	assertTrue(assign4.getSubmissionStatusConstant().equals(AssignmentConstants.SUBMISSION_NOT_STARTED));
    }
    
    public void testGetSubmissionStatusConstantForCurrentVersion() {
    	
    	// can be in progress, not started, or submitted
    	// start with one that is in progress/draft
    	Integer status = submissionLogic.getSubmissionStatusConstantForCurrentVersion(testData.st1a3CurrVersion);
    	assertTrue(status.equals(AssignmentConstants.SUBMISSION_IN_PROGRESS));
    	// empty submission 
    	status = submissionLogic.getSubmissionStatusConstantForCurrentVersion(new AssignmentSubmissionVersion());
    	assertTrue(status.equals(AssignmentConstants.SUBMISSION_NOT_STARTED));
    	// null submission
    	status = submissionLogic.getSubmissionStatusConstantForCurrentVersion(null);
    	assertTrue(status.equals(AssignmentConstants.SUBMISSION_NOT_STARTED));
    	// let's try one that is submitted
    	status = submissionLogic.getSubmissionStatusConstantForCurrentVersion(testData.st1a1CurrVersion);
    	assertTrue(status.equals(AssignmentConstants.SUBMISSION_SUBMITTED));
    }
    
    public void testSortSubmissions() {
    	// let's set some of the non-persisted fields on the submissions first
    	// these are not necessarily going to jive with the true data - they're just for testing
    	testData.st1a1Submission.setCurrentSubmissionVersion(testData.st1a1CurrVersion);
    	testData.st2a1Submission.setCurrentSubmissionVersion(testData.st2a1CurrVersion);
    	testData.st3a3Submission.setCurrentSubmissionVersion(testData.st3a3CurrVersion);
    	testData.st1a1Submission.setSubmissionStatus("Submitted");
    	testData.st1a1Submission.setGradebookGrade("A");
    	testData.st2a1Submission.setSubmissionStatus("In Progress");
    	testData.st3a3Submission.setSubmissionStatus("Not Started");
    	
    	List subList = new ArrayList();
    	subList.add(testData.st1a1Submission);
    	subList.add(testData.st2a1Submission);
    	subList.add(testData.st3a3Submission);
    	
    	// if list is null, should do nothing
    	submissionLogic.sortSubmissions(null, AssignmentSubmissionLogic.SORT_BY_GRADE, true);
    	// TODO - sorting uses UserDirectoryService, so will need to be re-thought
    	// for test
    	/*
    	// let's try the different sorting mech
    	submissionLogic.sortSubmissions(subList, AssignmentSubmissionLogic.SORT_BY_GRADE, true);
    	AssignmentSubmission sub1 = (AssignmentSubmission) subList.get(0);
    	AssignmentSubmission sub2 = (AssignmentSubmission) subList.get(1);
    	AssignmentSubmission sub3 = (AssignmentSubmission) subList.get(2);
    	assertTrue(sub1.getId().equals(testData.st1a1Submission.getId()));
    	assertTrue(sub2.getId().equals(testData.st2a1Submission.getId()));
    	assertTrue(sub3.getId().equals(testData.st3a3Submission.getId()));

    	submissionLogic.sortSubmissions(subList, AssignmentSubmissionLogic.SORT_BY_GRADE, false);
    	sub1 = (AssignmentSubmission) subList.get(0);
    	sub2 = (AssignmentSubmission) subList.get(1);
    	sub3 = (AssignmentSubmission) subList.get(2);
    	assertTrue(sub1.getId().equals(testData.st1a1Submission.getId()));
    	assertTrue(sub2.getId().equals(testData.st2a1Submission.getId()));
    	assertTrue(sub3.getId().equals(testData.st3a3Submission.getId()));*/
    }
    
	public void testSubmissionIsOpenForStudentForAssignment() {
		// try a null student
		try {
			submissionLogic.submissionIsOpenForStudentForAssignment(null, testData.a1Id);
			fail("did not catch null studentId passed to submissionIsOpenForStudentForAssignment");
		} catch (IllegalArgumentException iae) {}
		
		// try a null assignment
		try {
			submissionLogic.submissionIsOpenForStudentForAssignment(AssignmentTestDataLoad.STUDENT1_UID, null);
			fail("did not catch null assignmentId passed to submissionIsOpenForStudentForAssignment");
		} catch (IllegalArgumentException iae) {}
		
		// try one with a submission already and no resubmission
		boolean open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT1_UID, testData.a1Id);
		assertFalse(open);
		
		// let's allow resubmission on the assignment level for assign1.
		// allow 3 submissions - this means st1 will still be open but not st2
		Assignment2 assign1 = dao.getAssignmentByIdWithGroups(testData.a1Id);
		assign1.setNumSubmissionsAllowed(3);
		assign1.setAcceptUntilTime(null);
		assignmentLogic.saveAssignment(assign1);
		
		// st 1 only has one submission, so still open
		open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT1_UID, testData.a1Id);
		assertTrue(open);
		// st 2 already has 3 submissions, so closed
		open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT2_UID, testData.a1Id);
		assertFalse(open);
		
		// now let's set resubmission on the submission level
		testData.st2a1CurrVersion = dao.getAssignmentSubmissionVersionByIdWithAttachments(testData.st2a1CurrVersion.getId());
		submissionLogic.saveInstructorFeedback(testData.st2a1CurrVersion.getId(),
				AssignmentTestDataLoad.STUDENT2_UID, testData.a1, 4, null,
				"blah", "notes", null, testData.st2a1CurrVersion.getFeedbackAttachSet());
		open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT2_UID, testData.a1Id);
		assertTrue(open);
		// now let's restrict it by date on the submission level
		Calendar cal = Calendar.getInstance();
    	cal.set(2005, 10, 01);
    	Date resubmitCloseTime = cal.getTime();
    	submissionLogic.saveInstructorFeedback(testData.st2a1CurrVersion.getId(),
				AssignmentTestDataLoad.STUDENT2_UID, testData.a1, 4, resubmitCloseTime,
				"blah", "notes", null, testData.st2a1CurrVersion.getFeedbackAttachSet());
    	// should be closed even though num submissions not reached
    	open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT2_UID, testData.a1Id);
		assertFalse(open);
		// should still be open for student1 b/c we haven't changed their submission
		open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT1_UID, testData.a1Id);
		assertTrue(open);
		
		// let's open it up by allowing resubmission on the assignment level
		assign1.setNumSubmissionsAllowed(4);
		assignmentLogic.saveAssignment(assign1);
		// should be open for both
		open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT2_UID, testData.a1Id);
		assertTrue(open);
		open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT1_UID, testData.a1Id);
		assertTrue(open);
		
		// let's make this assignment not open yet
    	cal.set(2020, 10, 01);
    	Date assignOpenTime = cal.getTime();
    	assign1.setOpenTime(assignOpenTime);
    	assignmentLogic.saveAssignment(assign1);
    	// should be closed for both
		open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT2_UID, testData.a1Id);
		assertFalse(open);
		open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT1_UID, testData.a1Id);
		assertFalse(open);
		
		// open it back up
		cal.set(2000, 10, 01);
		assignOpenTime = cal.getTime();
		assign1.setOpenTime(assignOpenTime);
		assignmentLogic.saveAssignment(assign1);
		
		// let's restrict it by date on the assign level
		// neither should be able to submit now
		assign1.setAcceptUntilTime(resubmitCloseTime);
		open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT2_UID, testData.a1Id);
		assertFalse(open);
		open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT1_UID, testData.a1Id);
		assertFalse(open);
	}
	
	public void testIsMostRecentVersionDraft() {
		// try a null submission
		try {
			submissionLogic.isMostRecentVersionDraft(null);
			fail("Did not catch null submission passed to isMostRecentVersionDraft");
		} catch (IllegalArgumentException iae) {}
		
		// try one with a submitted curr version
		boolean draft = submissionLogic.isMostRecentVersionDraft(testData.st1a1Submission);
		assertFalse(draft);
		// try one with a draft curr version
		draft = submissionLogic.isMostRecentVersionDraft(testData.st1a3Submission);
		assertTrue(draft);
		// try one without a current version
		draft = submissionLogic.isMostRecentVersionDraft(testData.st2a2SubmissionNoVersions);
		assertFalse(draft);
	}
	
	public void testReleaseAllFeedbackForAssignment() {
		// try a null assignmentId
		try {
			submissionLogic.releaseAllFeedbackForAssignment(null);
			fail("did not catch null assignmentId passed to releaseAllFeedbackForAssignment");
		} catch (IllegalArgumentException iae) {}
		
		// try an assignmentId that doesn't exist
		try {
			submissionLogic.releaseAllFeedbackForAssignment(new Long(12345));
			fail("did not catch non-existent assignId passed to releaseAllFeedbackForAssignment");
		} catch (IllegalArgumentException iae) {}
		
		// try as a student
		authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
		try {
			submissionLogic.releaseAllFeedbackForAssignment(testData.a1Id);
			fail("Did not catch a student releasing feedback!!");
		} catch (SecurityException se) {}
		
		// try as a TA
		authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
		submissionLogic.releaseAllFeedbackForAssignment(testData.a1Id);
		// should only have updated the one student this TA is allowed to grade!
		Set st1a1History = dao.getVersionHistoryForSubmission(testData.st1a1Submission);
		for (Iterator hIter = st1a1History.iterator(); hIter.hasNext();) {
			AssignmentSubmissionVersion asv = (AssignmentSubmissionVersion)hIter.next();
			assertNotNull(asv.getReleasedTime());
		}
		// nothing should be released for student 2
		Set st2a1History = dao.getVersionHistoryForSubmission(testData.st2a1Submission);
		for (Iterator hIter = st2a1History.iterator(); hIter.hasNext();) {
			AssignmentSubmissionVersion asv = (AssignmentSubmissionVersion)hIter.next();
			assertNull(asv.getReleasedTime());
		}
		
		// instructor should update all
		authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
		submissionLogic.releaseAllFeedbackForAssignment(testData.a3Id);
		
		// every version should be released for all students
		Set st1a3History = dao.getVersionHistoryForSubmission(testData.st1a3Submission);
		for (Iterator hIter = st1a3History.iterator(); hIter.hasNext();) {
			AssignmentSubmissionVersion asv = (AssignmentSubmissionVersion)hIter.next();
			assertNotNull(asv.getReleasedTime());
		}
		Set st2a3History = dao.getVersionHistoryForSubmission(testData.st2a3Submission);
		for (Iterator hIter = st2a3History.iterator(); hIter.hasNext();) {
			AssignmentSubmissionVersion asv = (AssignmentSubmissionVersion)hIter.next();
			assertNotNull(asv.getReleasedTime());
		}
		Set st3a3History = dao.getVersionHistoryForSubmission(testData.st3a3Submission);
		for (Iterator hIter = st3a3History.iterator(); hIter.hasNext();) {
			AssignmentSubmissionVersion asv = (AssignmentSubmissionVersion)hIter.next();
			assertNotNull(asv.getReleasedTime());
		}
	}
	
	public void testReleaseAllFeedbackForSubmission() {
		// try null submissionId
		try {
			submissionLogic.releaseAllFeedbackForSubmission(null);
			fail("Did not catch null submissionId passed to releaseAllFeedbackForSubmission");
		} catch (IllegalArgumentException iae) {}
		
		// try a submissionId that doesn't exist
		try {
			submissionLogic.releaseAllFeedbackForSubmission(new Long(12345));
			fail("Did not catch nonexistent submission passed to releaseAllFeedbackForSubmission");
		} catch (IllegalArgumentException iae) {}
		
		// try as a student - should be security exception
		authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
		try {
			submissionLogic.releaseAllFeedbackForSubmission(testData.st1a1Submission.getId());
			fail("Did not catch student trying to release feedback for a submission!!");
		} catch (SecurityException se) {}
		
		// now try as a TA
		authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
		// should get SecurityException for student he/she can't grade
		try {
			submissionLogic.releaseAllFeedbackForSubmission(testData.st2a1Submission.getId());
			fail("Did not catch TA trying to release feedback for a student he/she is not allowed to grade!!!");
		} catch (SecurityException se) {}
		
		// let's try one the ta is authorized to grade
		submissionLogic.releaseAllFeedbackForSubmission(testData.st1a1Submission.getId());
		Set versionHistory = dao.getVersionHistoryForSubmission(testData.st1a1Submission);
		for (Iterator hIter = versionHistory.iterator(); hIter.hasNext();) {
			AssignmentSubmissionVersion asv = (AssignmentSubmissionVersion) hIter.next();
			assertNotNull(asv.getReleasedTime());
		}
		
		// make sure instructor can release, as well
		authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
		submissionLogic.releaseAllFeedbackForSubmission(testData.st2a1Submission.getId());
		versionHistory = dao.getVersionHistoryForSubmission(testData.st2a1Submission);
		for (Iterator hIter = versionHistory.iterator(); hIter.hasNext();) {
			AssignmentSubmissionVersion asv = (AssignmentSubmissionVersion) hIter.next();
			assertNotNull(asv.getReleasedTime());
		}
	}
	
	public void testReleaseFeedbackForVersion() {
		// try a null versionId
		try {
			submissionLogic.releaseFeedbackForVersion(null);
			fail("did not catch null versionId passed to releaseFeedbackForVersion");
		} catch (IllegalArgumentException iae) {}
		
		// try a versionId that doesn't exist
		try {
			submissionLogic.releaseFeedbackForVersion(new Long(12345));
			fail("did not catch bad versionId passed to releaseFeedbackForVersion");
		} catch (IllegalArgumentException iae) {}
		
		// try as a student - should be security exception
		authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
		try {
			submissionLogic.releaseFeedbackForVersion(testData.st1a1CurrVersion.getId());
			fail("Did not catch student trying to release feedback for a version!!");
		} catch (SecurityException se) {}
		
		// now try as a TA
		authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
		// should get SecurityException for student he/she can't grade
		try {
			submissionLogic.releaseFeedbackForVersion(testData.st2a1CurrVersion.getId());
			fail("Did not catch TA trying to release feedback for a student he/she is not allowed to grade!!!");
		} catch (SecurityException se) {}
		
		// let's try one the ta is authorized to grade
		submissionLogic.releaseFeedbackForVersion(testData.st1a1CurrVersion.getId());
		Set versionHistory = dao.getVersionHistoryForSubmission(testData.st1a1Submission);
		for (Iterator hIter = versionHistory.iterator(); hIter.hasNext();) {
			AssignmentSubmissionVersion asv = (AssignmentSubmissionVersion) hIter.next();
			if (asv.getId().equals(testData.st1a1CurrVersion.getId())) {
				assertNotNull(asv.getReleasedTime());
			} else {
				// make sure no other versions were released
				assertNull(asv.getReleasedTime());
			}
		}
		
		// make sure instructor can release, as well
		authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
		submissionLogic.releaseFeedbackForVersion(testData.st2a1CurrVersion.getId());
		versionHistory = dao.getVersionHistoryForSubmission(testData.st2a1Submission);
		for (Iterator hIter = versionHistory.iterator(); hIter.hasNext();) {
			AssignmentSubmissionVersion asv = (AssignmentSubmissionVersion) hIter.next();
			if (asv.getId().equals(testData.st2a1CurrVersion.getId())) {
				assertNotNull(asv.getReleasedTime());
			} else {
				//make sure no other versions were released
				assertNull(asv.getReleasedTime());
			}
		}
	}
	
	public void testGetVersionHistoryForSubmission() {
		// try a null submission
		try {
			submissionLogic.getVersionHistoryForSubmission(null);
			fail("did not catch null submission passed to getVersionHistoryForSubmission");
		} catch (IllegalArgumentException iae) {}
		
		// student should only be able to retrieve their own history
		authn.setAuthnContext(AssignmentTestDataLoad.STUDENT2_UID);
		try {
			submissionLogic.getVersionHistoryForSubmission(testData.st1a1Submission);
			fail("Did not catch a student retrieving versionHistory for another student!");
		} catch (SecurityException se) {}
		
		List history = submissionLogic.getVersionHistoryForSubmission(testData.st2a1Submission);
		assertEquals(history.size(), 3);
		// check that feedback was restricted b/c none released
		for (Iterator hIter = history.iterator(); hIter.hasNext();) {
			AssignmentSubmissionVersion asv = (AssignmentSubmissionVersion) hIter.next();
			assertTrue(asv.getFeedbackNotes().equals(""));
			assertTrue(asv.getAnnotatedText().equals(""));
			assertTrue(asv.getFeedbackAttachSet().isEmpty());
		}
		
		// switch to ta
		// shouldn't be able to view student2 history
		authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
		try {
			submissionLogic.getVersionHistoryForSubmission(testData.st2a1Submission);
			fail("Did not catch a ta retrieving versionHistory for student not authorized to view");
		} catch (SecurityException se) {}
		
		history = submissionLogic.getVersionHistoryForSubmission(testData.st1a1Submission);
		assertEquals(history.size(), 1);
		
		// switch to instructor
		authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
		history = submissionLogic.getVersionHistoryForSubmission(testData.st1a3Submission);
		assertEquals(history.size(), 2);
		// check that submission info for curr version was restricted b/c draft
		for (Iterator hIter = history.iterator(); hIter.hasNext();) {
			AssignmentSubmissionVersion asv = (AssignmentSubmissionVersion) hIter.next();
			if (asv.getId().equals(testData.st1a3CurrVersion.getId())) {
				assertTrue(asv.getSubmittedText().equals(""));
				assertTrue(asv.getSubmissionAttachSet().isEmpty());
			}
		}
	}
}
