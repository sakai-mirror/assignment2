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

import org.sakaiproject.assignment2.exception.AssignmentNotFoundException;
import org.sakaiproject.assignment2.exception.SubmissionNotFoundException;
import org.sakaiproject.assignment2.exception.VersionNotFoundException;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.FeedbackAttachment;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.test.AssignmentTestDataLoad;


public class AssignmentSubmissionLogicTest extends Assignment2TestBase {
    
    /**
     * @see org.springframework.test.AbstractTransactionalSpringContextTests#onSetUpInTransaction()
     */
    protected void onSetUpInTransaction() throws Exception {
        super.onSetUpInTransaction();
        
    }

    public void testGetAssignmentSubmissionById() {
    	// try passing a null id
    	try {
    		submissionLogic.getAssignmentSubmissionById(null);
    		fail("did not catch null submissionId passed to getAssignmentSubmissionById");
    	} catch(IllegalArgumentException iae) {}
    	
    	// try passing an id that doesn't exist
    	try {
    		submissionLogic.getAssignmentSubmissionById(12345L);
    		fail("did not catch non-existent id passed to getAssignmentSubmissionById");
    	} catch (SubmissionNotFoundException snfe) {}
    	
    	// the instructor should be able to retrieve any submission
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	AssignmentSubmission submission = submissionLogic.getAssignmentSubmissionById(testData.st1a1Submission.getId());
    	assertTrue(submission.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	submission = submissionLogic.getAssignmentSubmissionById(testData.st2a2SubmissionNoVersions.getId());
    	assertTrue(submission.getUserId().equals(AssignmentTestDataLoad.STUDENT2_UID));
    	submission = submissionLogic.getAssignmentSubmissionById(testData.st2a4Submission.getId());
    	assertTrue(submission.getUserId().equals(AssignmentTestDataLoad.STUDENT2_UID));
    	
    	// will throw SecurityException if currentUser isn't auth to view submission
    	// let's try a TA
    	// should be able to view student 1's submission
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
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
    	
    	// clear the session since hsqld won't do it for you and will try to save it
    	// upon re-retrieval
    	dao.clearSession();
    	
    	// student should be able to get their own
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
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
    	try {
    		submissionLogic.getSubmissionVersionById(12345L);
    		fail("did not catch non-existent id passed to getSubmissionVersionById");
    	} catch (VersionNotFoundException vnfe) {}
    	
    	// instructors should be able to retrieve any version
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	AssignmentSubmissionVersion version = submissionLogic.getSubmissionVersionById(testData.st1a1CurrVersion.getId());
    	assertTrue(version.getAssignmentSubmission().getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	version = submissionLogic.getSubmissionVersionById(testData.st2a4CurrVersion.getId());
    	assertTrue(version.getAssignmentSubmission().getUserId().equals(AssignmentTestDataLoad.STUDENT2_UID));
    	
    	// ta should be restricted
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
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
    	
    	// now let's double check that the ta can't see student submission details when it is
    	// draft...
    	version = submissionLogic.getSubmissionVersionById(testData.st1a3CurrVersion.getId());
    	assertTrue(version.getAssignmentSubmission().getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	assertTrue(version.getFeedbackAttachSet().size() == 1);
    	assertTrue(version.isDraft());
    	// make sure these are empty!
    	assertTrue(version.getSubmissionAttachSet().isEmpty());
    	assertTrue(version.getSubmittedText().equals(""));
    	
    	// let's clear out the session b/c hsqldb will try to save the prev
    	// version upon re-retrieval (and some fields were nulled out so will be out of sync)
    	dao.clearSession();

    	// now make sure ta can't see st 3
    	try {
    		version = submissionLogic.getSubmissionVersionById(testData.st3a3CurrVersion.getId());
    		fail("ta should not be able to access st3's submission for a3!");
    	} catch (SecurityException se) {}
    	
    	// TODO grader permissions
    	
    	// student may see their own submission
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
    	version = submissionLogic.getSubmissionVersionById(testData.st1a3CurrVersion.getId());
    	assertTrue(version.getAssignmentSubmission().getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	// feedback was not released, so double check that it was not populated
    	assertTrue(version.getFeedbackAttachSet().isEmpty());
    	assertTrue(version.getFeedbackNotes().equals(""));
    	assertTrue(version.getAnnotatedText().equals(""));
    	// make sure these are populated! other users may not see this info b/c draft status
    	assertTrue(version.getSubmissionAttachSet().size() == 1);
    	assertTrue(!version.getSubmittedText().equals(""));
    	
    	// let's clear out the session b/c hsqldb will try to save the prev
    	// version upon re-retrieval (and some fields were nulled out so will be out of sync)
    	dao.clearSession();
    	
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
    	
    	// pass an assignmentId that doesn't exist
    	try {
    		submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(12345L, AssignmentTestDataLoad.STUDENT1_UID);
    		fail("did not catch non-existent id passed to getCurrentSubmissionByAssignmentIdAndStudentId");
    	} catch (AssignmentNotFoundException anfe) {}
    	
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	
    	// try to get one for a student who hasn't made a submission yet
    	AssignmentSubmission submission = submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(testData.a2Id, AssignmentTestDataLoad.STUDENT1_UID);
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
    	
		// clear the history b/c some fields will be out of sync with the
		// session (in hsqldb) and will throw an error on re-retrieval
		dao.clearSession();
    	
    	// what if the student is not part of the passed assignment?
    	// ie it is restricted to groups that the student is not a member of
    	
    	// now, switch to a ta
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
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
    	
		// clear the history b/c some fields will be out of sync with the
		// session (in hsqldb) and will throw an error on re-retrieval
		dao.clearSession();
		
    	// TODO grader permissions
    	
    	// now switch to a student
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
    	// should be able to retrieve own submission
    	submission = submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(testData.a3Id, AssignmentTestDataLoad.STUDENT1_UID);
    	assertTrue(submission.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
    	assertTrue(submission.getCurrentSubmissionVersion().getId().equals(testData.st1a3CurrVersion.getId()));
    	// this is a draft, so check that the submission info is populated for student
    	assertTrue(submission.getCurrentSubmissionVersion().getSubmissionAttachSet().size() == 1);
    	assertFalse(submission.getCurrentSubmissionVersion().getSubmittedText().equals(""));
    	
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
   
    	// try passing an empty assignment (with no id)
    	try {
    		submissionLogic.saveStudentSubmission(AssignmentTestDataLoad.STUDENT1_UID, 
    				new Assignment2(), false, null, null);
    	} catch (IllegalArgumentException iae) {}
    	
    	// let's see if an instructor can make a submission for a student
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	try {
    		submissionLogic.saveStudentSubmission(AssignmentTestDataLoad.STUDENT1_UID,
    				testData.a1, false, null, null);
    		fail("did not catch instructor trying to save a student's submission via saveStudentSubmission");
    	} catch (SecurityException se) {}
    	
    	// try the ta
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
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
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
    	// double check that no submission exists yet
    	List<AssignmentSubmission> subList = dao.findByProperties(
    			AssignmentSubmission.class, new String[] {"userId", "assignment"}, 
    			new Object[] {AssignmentTestDataLoad.STUDENT1_UID, testData.a2});
    	assertTrue(subList.isEmpty());
    	
    	SubmissionAttachment attach1 = new SubmissionAttachment();
    	attach1.setAttachmentReference("ref1");
    	SubmissionAttachment attach2 = new SubmissionAttachment();
    	attach2.setAttachmentReference("ref2");
    	Set<SubmissionAttachment> attachSet = new HashSet<SubmissionAttachment>();
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
    	Set<AssignmentSubmissionVersion> versionHistory = dao.getVersionHistoryForSubmission(existingSub);
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
    	existingSub.setNumSubmissionsAllowed(2);
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
    	
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	
    	// try a versionId that doesn't exist
    	try {
    		submissionLogic.saveInstructorFeedback(12345L, AssignmentTestDataLoad.STUDENT1_UID, 
    				testData.a2, null, null, null, null, null, null);
    		fail("did not catch passed versionId that does not exist to saveInstructorFeedback");
    	} catch (VersionNotFoundException iae) {}
    	
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
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	
    	// submit feedback for student w/o submission
    	// student 1 has not submitted for a2 yet
    	Set<FeedbackAttachment> feedbackAttachSet = new HashSet<FeedbackAttachment>();
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
    	Set<AssignmentSubmissionVersion> versionHistory = dao.getVersionHistoryForSubmission(st1a2Submission);
    	assertTrue(versionHistory.size() == 1);
    	
    	// try a TA
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
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
    	assertTrue(currVersion.getAssignmentSubmission().getNumSubmissionsAllowed().equals(2));
    	assertTrue(currVersion.getAssignmentSubmission().getResubmitCloseDate().equals(resubmitCloseDate));
    	
    	// student should not be authorized
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
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
    	
    	// try a non-existent assignmentId
    	try {
    		submissionLogic.getViewableSubmissionsForAssignmentId(12345L);
    		fail("did not catch non-existent id passed to getViewableSubmissionsForAssignmentId");
    	} catch (AssignmentNotFoundException anfe) {}
    	
    	// start as instructor
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	// we should get 2 students back for a1 b/c group restrictions
    	List<AssignmentSubmission> subList = submissionLogic.getViewableSubmissionsForAssignmentId(testData.a1Id);
    	assertTrue(subList.size() == 2);
    	// we should get 3 for a2 b/c no restrictions
    	subList = submissionLogic.getViewableSubmissionsForAssignmentId(testData.a2Id);
    	assertTrue(subList.size() == 3);
    	// we should get 3 for a3 b/c no restrictions
    	subList = submissionLogic.getViewableSubmissionsForAssignmentId(testData.a3Id);
    	assertTrue(subList.size() == 3);
    	// let's make sure the submission for st1 is restricted b/c draft
    	for (AssignmentSubmission sub : subList) {
    		if (sub.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID)) {
    			assertEquals(sub.getCurrentSubmissionVersion().getSubmittedText(), "");
    			assertTrue(sub.getCurrentSubmissionVersion().getSubmissionAttachSet().isEmpty());
    		}
    	}
    	
		// clear the history b/c some fields will be out of sync with the
		// session (in hsqldb) and will throw an error on re-retrieval
		dao.clearSession();
		
    	// we should get 1 for a4 b/c group restrictions
    	subList = submissionLogic.getViewableSubmissionsForAssignmentId(testData.a4Id);
    	assertTrue(subList.size() == 1);
    	
    	// now become ta
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
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
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
    	try {
    		subList = submissionLogic.getViewableSubmissionsForAssignmentId(testData.a1Id);
    		fail("Did not catch student attempting to access submissions via getViewableSubmissionsForAssignmentId");
    	} catch (SecurityException se) {}

    }
    
    public void testSetSubmissionStatusConstantForAssignments() {
    	// try null studentId
    	try {
    		submissionLogic.getSubmissionStatusConstantForAssignments(new ArrayList<Assignment2>(), null);
    		fail("Did not catch null studentId passed to setSubmissionStatusForAssignments");
    	} catch(IllegalArgumentException iae) {}
    	
    	externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	
    	// try a null assignment list
    	// should do nothing
    	submissionLogic.getSubmissionStatusConstantForAssignments(null, AssignmentTestDataLoad.STUDENT1_UID);
    	
    	// let's create a list of assignments
    	List<Assignment2> assignList = new ArrayList<Assignment2>();
    	assignList.add(testData.a1);
    	assignList.add(testData.a2);
    	assignList.add(testData.a3);
    	assignList.add(testData.a4);
    	
    	Map<Assignment2, Integer> assignToStatusMap = submissionLogic.getSubmissionStatusConstantForAssignments(assignList, AssignmentTestDataLoad.STUDENT1_UID);
    	Integer status = assignToStatusMap.get(testData.a1);
    	assertTrue(status.equals(AssignmentConstants.SUBMISSION_SUBMITTED));
    	
    	status = assignToStatusMap.get(testData.a2);
    	assertTrue(status.equals(AssignmentConstants.SUBMISSION_NOT_STARTED));
    	
    	status = assignToStatusMap.get(testData.a3);
    	assertTrue(status.equals(AssignmentConstants.SUBMISSION_IN_PROGRESS));
    	
    	status = assignToStatusMap.get(testData.a4);
    	assertTrue(status.equals(AssignmentConstants.SUBMISSION_NOT_STARTED));
    }
    
    public void testGetSubmissionStatusConstantForCurrentVersion() {
    	
    	// can be in progress, not started, or submitted
    	// due date has passed
    	// start with one that is in progress/draft
    	Calendar cal = Calendar.getInstance();
    	cal.set(2005, 10, 01);
    	Date dueDate = cal.getTime();
    	
    	Integer status = submissionLogic.getSubmissionStatusConstantForCurrentVersion(testData.st1a3CurrVersion, dueDate);
    	assertTrue(status.equals(AssignmentConstants.SUBMISSION_IN_PROGRESS));
    	// empty submission 
    	status = submissionLogic.getSubmissionStatusConstantForCurrentVersion(new AssignmentSubmissionVersion(), dueDate);
    	assertTrue(status.equals(AssignmentConstants.SUBMISSION_NOT_STARTED));
    	// null submission
    	status = submissionLogic.getSubmissionStatusConstantForCurrentVersion(null, dueDate);
    	assertTrue(status.equals(AssignmentConstants.SUBMISSION_NOT_STARTED));
    	// let's try one that is submitted
    	status = submissionLogic.getSubmissionStatusConstantForCurrentVersion(testData.st1a1CurrVersion, dueDate);
    	assertTrue(status.equals(AssignmentConstants.SUBMISSION_LATE));
    	
    	// try submitted one with null due Date
    	status = submissionLogic.getSubmissionStatusConstantForCurrentVersion(testData.st1a1CurrVersion, null);
    	assertTrue(status.equals(AssignmentConstants.SUBMISSION_SUBMITTED));
    	
    	// try a due date in the future
    	cal.set(2020, 10, 1);
    	dueDate = cal.getTime();
    	status = submissionLogic.getSubmissionStatusConstantForCurrentVersion(testData.st1a1CurrVersion, dueDate);
    	assertTrue(status.equals(AssignmentConstants.SUBMISSION_SUBMITTED));
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
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
		Assignment2 assign1 = dao.getAssignmentByIdWithGroups(testData.a1Id);
		assign1.setNumSubmissionsAllowed(3);
		assign1.setAcceptUntilDate(null);
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
		
		// let's make a draft submission to double check it is still open
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT2_UID);
		submissionLogic.saveStudentSubmission(AssignmentTestDataLoad.STUDENT2_UID, testData.a1, true, "blah", null);
		open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT2_UID, testData.a1Id);
		assertTrue(open);
		
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
		
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
		
		// let's allow resubmission on the assignment level - should not affect it
		// since submission level trumps
		assign1.setNumSubmissionsAllowed(4);
		assignmentLogic.saveAssignment(assign1);
		// should be open for both
		open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT2_UID, testData.a1Id);
		assertFalse(open);
		open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT1_UID, testData.a1Id);
		assertTrue(open);
		
		// let's make this assignment not open yet
    	cal.set(2020, 10, 01);
    	Date assignOpenTime = cal.getTime();
    	assign1.setOpenDate(assignOpenTime);
    	assignmentLogic.saveAssignment(assign1);
    	// should be closed for both
		open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT2_UID, testData.a1Id);
		assertFalse(open);
		open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT1_UID, testData.a1Id);
		assertFalse(open);
		
		// open it back up
		// let's restrict it by date on the assign level
		cal.set(2000, 10, 01);
		assignOpenTime = cal.getTime();
		assign1.setOpenDate(assignOpenTime);
		assign1.setAcceptUntilDate(resubmitCloseTime);
		assignmentLogic.saveAssignment(assign1);
		

		// let's open up on the submission level for student 2
		submissionLogic.saveInstructorFeedback(testData.st2a1CurrVersion.getId(),
				AssignmentTestDataLoad.STUDENT2_UID, testData.a1, 4, null,
				"blah", "notes", null, testData.st2a1CurrVersion.getFeedbackAttachSet());
		
		// student 1 should not be able to submit b/c of assignment-level restriction
		// but student 2 can
		open = submissionLogic.submissionIsOpenForStudentForAssignment(
				AssignmentTestDataLoad.STUDENT2_UID, testData.a1Id);
		assertTrue(open);
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
		
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
		
		// try an assignmentId that doesn't exist
		try {
			submissionLogic.releaseAllFeedbackForAssignment(12345L);
			fail("did not catch non-existent assignId passed to releaseAllFeedbackForAssignment");
		} catch (AssignmentNotFoundException iae) {}
		
		// try as a student
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
		try {
			submissionLogic.releaseAllFeedbackForAssignment(testData.a1Id);
			fail("Did not catch a student releasing feedback!!");
		} catch (SecurityException se) {}
		
		// try as a TA
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
		submissionLogic.releaseAllFeedbackForAssignment(testData.a1Id);
		// should only have updated the one student this TA is allowed to grade!
		Set<AssignmentSubmissionVersion> st1a1History = dao.getVersionHistoryForSubmission(testData.st1a1Submission);
		for (AssignmentSubmissionVersion asv : st1a1History) {
			assertNotNull(asv.getFeedbackReleasedDate());
		}
		// nothing should be released for student 2
		Set<AssignmentSubmissionVersion> st2a1History = dao.getVersionHistoryForSubmission(testData.st2a1Submission);
		for (AssignmentSubmissionVersion asv : st2a1History) {
			assertNull(asv.getFeedbackReleasedDate());
		}
		
		// instructor should update all
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
		submissionLogic.releaseAllFeedbackForAssignment(testData.a3Id);
		
		// every version should be released for all students
		Set<AssignmentSubmissionVersion> st1a3History = dao.getVersionHistoryForSubmission(testData.st1a3Submission);
		for (AssignmentSubmissionVersion asv : st1a3History) {
			assertNotNull(asv.getFeedbackReleasedDate());
		}
		Set<AssignmentSubmissionVersion> st2a3History = dao.getVersionHistoryForSubmission(testData.st2a3Submission);
		for (AssignmentSubmissionVersion asv : st2a3History) {
			assertNotNull(asv.getFeedbackReleasedDate());
		}
		Set<AssignmentSubmissionVersion> st3a3History = dao.getVersionHistoryForSubmission(testData.st3a3Submission);
		for (AssignmentSubmissionVersion asv : st3a3History) {
			assertNotNull(asv.getFeedbackReleasedDate());
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
			submissionLogic.releaseAllFeedbackForSubmission(12345L);
			fail("Did not catch nonexistent submission passed to releaseAllFeedbackForSubmission");
		} catch (SubmissionNotFoundException iae) {}
		
		// try as a student - should be security exception
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
		try {
			submissionLogic.releaseAllFeedbackForSubmission(testData.st1a1Submission.getId());
			fail("Did not catch student trying to release feedback for a submission!!");
		} catch (SecurityException se) {}
		
		// now try as a TA
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
		// should get SecurityException for student he/she can't grade
		try {
			submissionLogic.releaseAllFeedbackForSubmission(testData.st2a1Submission.getId());
			fail("Did not catch TA trying to release feedback for a student he/she is not allowed to grade!!!");
		} catch (SecurityException se) {}
		
		// let's try one the ta is authorized to grade
		submissionLogic.releaseAllFeedbackForSubmission(testData.st1a1Submission.getId());
		Set<AssignmentSubmissionVersion> versionHistory = dao.getVersionHistoryForSubmission(testData.st1a1Submission);
		for (AssignmentSubmissionVersion asv : versionHistory) {
			assertNotNull(asv.getFeedbackReleasedDate());
		}
		
		// make sure instructor can release, as well
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
		submissionLogic.releaseAllFeedbackForSubmission(testData.st2a1Submission.getId());
		versionHistory = dao.getVersionHistoryForSubmission(testData.st2a1Submission);
		for (AssignmentSubmissionVersion asv : versionHistory) {
			assertNotNull(asv.getFeedbackReleasedDate());
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
			submissionLogic.releaseFeedbackForVersion(12345L);
			fail("did not catch bad versionId passed to releaseFeedbackForVersion");
		} catch (VersionNotFoundException iae) {}
		
		// try as a student - should be security exception
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT1_UID);
		try {
			submissionLogic.releaseFeedbackForVersion(testData.st1a1CurrVersion.getId());
			fail("Did not catch student trying to release feedback for a version!!");
		} catch (SecurityException se) {}
		
		// now try as a TA
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
		// should get SecurityException for student he/she can't grade
		try {
			submissionLogic.releaseFeedbackForVersion(testData.st2a1CurrVersion.getId());
			fail("Did not catch TA trying to release feedback for a student he/she is not allowed to grade!!!");
		} catch (SecurityException se) {}
		
		// let's try one the ta is authorized to grade
		submissionLogic.releaseFeedbackForVersion(testData.st1a1CurrVersion.getId());
		Set<AssignmentSubmissionVersion> versionHistory = dao.getVersionHistoryForSubmission(testData.st1a1Submission);
		for (AssignmentSubmissionVersion asv : versionHistory) {
			if (asv.getId().equals(testData.st1a1CurrVersion.getId())) {
				assertNotNull(asv.getFeedbackReleasedDate());
			} else {
				// make sure no other versions were released
				assertNull(asv.getFeedbackReleasedDate());
			}
		}
		
		// make sure instructor can release, as well
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
		submissionLogic.releaseFeedbackForVersion(testData.st2a1CurrVersion.getId());
		versionHistory = dao.getVersionHistoryForSubmission(testData.st2a1Submission);
		for (AssignmentSubmissionVersion asv : versionHistory) {
			if (asv.getId().equals(testData.st2a1CurrVersion.getId())) {
				assertNotNull(asv.getFeedbackReleasedDate());
			} else {
				//make sure no other versions were released
				assertNull(asv.getFeedbackReleasedDate());
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
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.STUDENT2_UID);
		try {
			submissionLogic.getVersionHistoryForSubmission(testData.st1a1Submission);
			fail("Did not catch a student retrieving versionHistory for another student!");
		} catch (SecurityException se) {}
		
		List<AssignmentSubmissionVersion> history = submissionLogic.getVersionHistoryForSubmission(testData.st2a1Submission);
		assertEquals(history.size(), 3);
		// check that feedback was restricted b/c none released
		for (AssignmentSubmissionVersion asv : history) {
			assertTrue(asv.getFeedbackNotes().equals(""));
			assertTrue(asv.getAnnotatedText().equals(""));
			assertTrue(asv.getFeedbackAttachSet().isEmpty());
		}
		
		// switch to ta
		// shouldn't be able to view student2 history
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.TA_UID);
		try {
			submissionLogic.getVersionHistoryForSubmission(testData.st2a1Submission);
			fail("Did not catch a ta retrieving versionHistory for student not authorized to view");
		} catch (SecurityException se) {}
		
		// clear the history b/c some fields will be out of sync with the
		// session (in hsqldb) and will throw an error on re-retrieval
		dao.clearSession();
		
		history = submissionLogic.getVersionHistoryForSubmission(testData.st1a1Submission);
		assertEquals(history.size(), 1);
		
		// switch to instructor
		externalLogic.setCurrentUserId(AssignmentTestDataLoad.INSTRUCTOR_UID);
		history = submissionLogic.getVersionHistoryForSubmission(testData.st1a3Submission);
		assertEquals(history.size(), 2);
		// check that submission info for curr version was restricted b/c draft
		for (AssignmentSubmissionVersion asv : history) {
			if (asv.getId().equals(testData.st1a3CurrVersion.getId())) {
				assertTrue(asv.getSubmittedText().equals(""));
				assertTrue(asv.getSubmissionAttachSet().isEmpty());
			}
		}
	}
	
	public void testGetNumSubmittedVersions() throws Exception {
		// try null params
		try {
			submissionLogic.getNumSubmittedVersions(null, testData.a1Id);
			fail("did not catch null studentId passed to getNumSubmittedVersions");
		} catch (IllegalArgumentException iae) {}
		
		try {
			submissionLogic.getNumSubmittedVersions(AssignmentTestDataLoad.STUDENT1_UID, null);
			fail("did not catch null assignmentId passed to getNumSubmittedVersions");
		} catch (IllegalArgumentException iae) {}
		
		// try an assignmentId that doesn't exist
		assertEquals(0, submissionLogic.getNumSubmittedVersions(AssignmentTestDataLoad.STUDENT3_UID, 12345L));
		
		// try a student with no submissions 
		assertEquals(0, submissionLogic.getNumSubmittedVersions(AssignmentTestDataLoad.STUDENT3_UID, testData.a1Id));
		
		// try a student with 2 submissions but one is draft
		assertEquals(1, submissionLogic.getNumSubmittedVersions(AssignmentTestDataLoad.STUDENT1_UID, testData.a3Id));
		
		// add instructor feedback w/o a submission
		AssignmentSubmission st3a1Submission = new AssignmentSubmission(testData.a1, AssignmentTestDataLoad.STUDENT3_UID);
		AssignmentSubmissionVersion st3a1CurrVersion = testData.createGenericVersion(st3a1Submission);
		st3a1CurrVersion.setDraft(false);
		st3a1CurrVersion.setSubmittedDate(null);
		dao.save(st3a1Submission);
		dao.save(st3a1CurrVersion);
		
		// should show up 0 b/c not submitted
		assertEquals(0, submissionLogic.getNumSubmittedVersions(AssignmentTestDataLoad.STUDENT3_UID, testData.a1Id));
	}
}
