/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/api/logic/src/java/org/sakaiproject/assignment2/dao/AssignmentDao.java $
 * $Id: AssignmentDao.java 12544 2006-05-03 15:06:26Z wagnermr@iupui.edu $
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

package org.sakaiproject.assignment2.dao.test;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.Iterator;

import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.test.AssignmentTestDataLoad;



/**
 * @author michellewagner (this was totally written by michelle and only michelle and no one else should take credit for it - az)
 * 
 */
public class AssignmentDaoImplTest extends Assignment2DaoTestBase {

	// run this before each test starts
	protected void onSetUpBeforeTransaction() throws Exception {
		
	}

	// run this before each test starts and as part of the transaction
	protected void onSetUpInTransaction() throws Exception {
		// initialize data
		super.onSetUpInTransaction();
	}

	/**
	 * ADD unit tests below here, use testMethod as the name of the unit test,
	 * Note that if a method is overloaded you should include the arguments in
	 * the test name like so: testMethodClassInt (for method(Class, int);
	 */

	/**
	 * Test method for
	 * {@link org.sakaiproject.assignment2.dao.impl.AssignmentDaoImpl#getHighestSortIndexInSite(java.lang.String)}.
	 */
	public void testGetHighestSortIndexInSite() {
		// negative test
		// count 0 items in unknown context
		int highestIndex = assignmentDao.getHighestSortIndexInSite(AssignmentTestDataLoad.BAD_CONTEXT);
		assertEquals(0, highestIndex);
		
		// exception testing
		try {
			assignmentDao.getHighestSortIndexInSite(null);
			fail("Should have thrown exception");
		} catch (IllegalArgumentException e) {
			assertNotNull(e);
		}
		
		// positive test	
		highestIndex = assignmentDao.getHighestSortIndexInSite(AssignmentTestDataLoad.CONTEXT_ID);
		assertEquals(3, highestIndex);
	}

	public void testGetAssignmentsWithGroupsAndAttachments() {
		// try passing a null contextId
		try {
			assignmentDao.getAssignmentsWithGroupsAndAttachments(null);
			fail("did not catch null parameter passed to getAssignmentsWithGroupsAndAttachments");
		} catch (IllegalArgumentException e) {
			
		}
		
		// try passing a context that doesn't exist
		Set<Assignment2> assignments = assignmentDao.getAssignmentsWithGroupsAndAttachments(AssignmentTestDataLoad.BAD_CONTEXT);
		assertNotNull(assignments);
		assertTrue(assignments.isEmpty());

		// now try the valid context - there should be 3 assignments
		assignments = assignmentDao.getAssignmentsWithGroupsAndAttachments(AssignmentTestDataLoad.CONTEXT_ID);
		assertNotNull(assignments);
		assertTrue(assignments.size() == 4);
		
		// for each assignment returned, double check that the attachment and group sets are accurate
		for (Iterator assignIter = assignments.iterator(); assignIter.hasNext();) {
			Assignment2 assign = (Assignment2) assignIter.next();
			if (assign.getId().equals(testData.a1Id)) {
				System.out.println("A1: " + assign.getTitle() + " " + testData.a1Id);
				assertTrue(assign.getAttachmentSet().size() == 2);
				assertTrue(assign.getAssignmentGroupSet().size() == 2);
			} else if (assign.getId().equals(testData.a2Id)) {
				assertTrue(assign.getAttachmentSet().isEmpty());
				assertTrue(assign.getAssignmentGroupSet().isEmpty());
			} else if (assign.getId().equals(testData.a3Id)) {
				assertNotNull(assign.getAttachmentSet());
				assertTrue(assign.getAttachmentSet().size() == 1);
				assertTrue(assign.getAssignmentGroupSet().isEmpty());
			} else if (assign.getId().equals(testData.a4Id)) {
				assertTrue(assign.getAttachmentSet().isEmpty());
				assertTrue(assign.getAssignmentGroupSet().size() == 1);
			}
		}
		
	}
	
	public void testGetAssignmentByIdWithGroups() {
		// pass a null id
		try {
			assignmentDao.getAssignmentByIdWithGroups(null);
			fail("did not catch null parameter passed to getAssignmentByIdWithGroups");
		} catch (IllegalArgumentException e) {
			
		}
		
		// now try an id that doesn't exist - null should be returned
		Assignment2 assign = assignmentDao.getAssignmentByIdWithGroups(new Long(27));
		assertNull(assign);
		
		// now let's try some we know exist
		// double check that the assignmentGroupSet is populated correctly
		assign = assignmentDao.getAssignmentByIdWithGroups(testData.a1Id);
		assertNotNull(assign);
		assertTrue(assign.getAssignmentGroupSet().size() == 2);
		assertTrue(assign.getTitle().equals(testData.ASSIGN1_TITLE));
		
		assign = assignmentDao.getAssignmentByIdWithGroups(testData.a3Id);
		assertNotNull(assign);
		assertTrue(assign.getAssignmentGroupSet().isEmpty());
		assertTrue(assign.getTitle().equals(testData.ASSIGN3_TITLE));
	}
	
	public void testGetAssignmentByIdWithGroupsAndAttachments() {
		// try a null id
		try {
			assignmentDao.getAssignmentByIdWithGroupsAndAttachments(null);
			fail("did not catch null parameter passed to getAssignmentByIdWithGroupsAndAttachments");
		} catch (IllegalArgumentException e) {
			
		}
		
		// now try an id that doesn't exist
		Assignment2 assign = assignmentDao.getAssignmentByIdWithGroupsAndAttachments(new Long(27));
		assertNull(assign);
		
		// now let's try some we know exist - double check that the
		// assignmentGroupSet and attachmentSet are returned properly
		assign = assignmentDao.getAssignmentByIdWithGroups(testData.a1Id);
		assertNotNull(assign);
		assertTrue(assign.getAssignmentGroupSet().size() == 2);
		assertTrue(assign.getAttachmentSet().size() == 2);
		assertTrue(assign.getTitle().equals(testData.ASSIGN1_TITLE));
		
		assign = assignmentDao.getAssignmentByIdWithGroups(testData.a3Id);
		assertNotNull(assign);
		assertTrue(assign.getAssignmentGroupSet().isEmpty());
		assertTrue(assign.getAttachmentSet().size() == 1);
		assertTrue(assign.getTitle().equals(AssignmentTestDataLoad.ASSIGN3_TITLE));
	}
	
	public void testGetCurrentSubmissionVersionWithAttachments() throws Exception {
		// make sure bad data is caught
		try {
			// try a null submission
			AssignmentSubmissionVersion version = assignmentDao.getCurrentSubmissionVersionWithAttachments(null);
			fail("did not catch null submission passed to getCurrentSubmissionVersionWithAttachments");
		} catch (IllegalArgumentException iae) {
		}
		
		try {
			// what happens if submission doesn't have an id? should throw error
			AssignmentSubmission submission = new AssignmentSubmission();
			AssignmentSubmissionVersion version = assignmentDao.getCurrentSubmissionVersionWithAttachments(submission);
			fail("did not catch submission w/ no id passed to getCurrentSubmissionVersionWithAttachments");
		} catch(IllegalArgumentException iae) {}

		// try a submission that does exist
		AssignmentSubmissionVersion version = assignmentDao.getCurrentSubmissionVersionWithAttachments(testData.st1a1Submission);
		assertNotNull(version);
		// check that the correct version was returned
		assertTrue(version.getId().equals(testData.st1a1CurrVersion.getId()));
		// there shouldn't be any attachments for this one
		assertTrue(version.getSubmissionAttachSet().isEmpty());
		
		version = assignmentDao.getCurrentSubmissionVersionWithAttachments(testData.st2a1Submission);
		assertNotNull(version);
		// check that the correct version was returned
		assertTrue(version.getId().equals(testData.st2a1CurrVersion.getId()));
		assertNotNull(version.getSubmissionAttachSet());
		// this one should have 2 submission and 2 feedback attach
		assertTrue(version.getSubmissionAttachSet().size() == 2);
		assertTrue(version.getFeedbackAttachSet().size() == 2);
		
		// test out a submission w/o a version
		version = assignmentDao.getCurrentSubmissionVersionWithAttachments(testData.st2a2SubmissionNoVersions);
		assertNull(version);
	}
	
	public void testGetCurrentAssignmentSubmissionsForStudent() throws Exception {
		// pass a null studentId
		try {
			assignmentDao.getCurrentAssignmentSubmissionsForStudent(new ArrayList(), null);
			fail("method getCurrentAssignmentSubmissionsForStudent did not catch null student parameter");
		} catch (IllegalArgumentException iae) {
		}
		
		// pass a null assignments list - should return empty list
		List submissions = assignmentDao.getCurrentAssignmentSubmissionsForStudent(null, AssignmentTestDataLoad.STUDENT1_UID);
		assertTrue(submissions.isEmpty());
		// pass an empty assignments list - should return empty list
		submissions = assignmentDao.getCurrentAssignmentSubmissionsForStudent(new ArrayList(), AssignmentTestDataLoad.STUDENT1_UID);
		assertTrue(submissions.isEmpty());
		
		// add two assignments to the list
		List assignList = new ArrayList();
		assignList.add(testData.a1);
		assignList.add(testData.a2);
		
		// try a student who won't have any
		submissions = assignmentDao.getCurrentAssignmentSubmissionsForStudent(assignList, "bogusStudent");
		assertTrue(submissions.isEmpty());
		
		// this student should have 1 submission
		submissions = assignmentDao.getCurrentAssignmentSubmissionsForStudent(assignList, AssignmentTestDataLoad.STUDENT1_UID);
		assertNotNull(submissions);
		assertTrue(submissions.size() == 1);
		AssignmentSubmission sub = (AssignmentSubmission)submissions.get(0);
		// double check that the current version was populated correctly
		assertNotNull(sub.getCurrentSubmissionVersion());
		assertTrue(sub.getCurrentSubmissionVersion().getId().equals(testData.st1a1CurrVersion.getId()));
		
		// this student should have 2 submissions (1 for each assign)
		submissions = assignmentDao.getCurrentAssignmentSubmissionsForStudent(assignList, AssignmentTestDataLoad.STUDENT2_UID);
		assertNotNull(submissions);
		assertTrue(submissions.size() == 2);
		for (Iterator subIter = submissions.iterator(); subIter.hasNext();) {
			AssignmentSubmission thisSub = (AssignmentSubmission) subIter.next();
			if (thisSub.getAssignment().getId().equals(testData.a1.getId())) {
				// this one should have a currentVersion
				assertNotNull(thisSub.getCurrentSubmissionVersion());
				assertTrue(thisSub.getCurrentSubmissionVersion().getId().equals(testData.st2a1CurrVersion.getId()));
			} else if (thisSub.getAssignment().getId().equals(testData.a2.getId())) {
				// this one shouldn't have a currentVersion
				assertNull(thisSub.getCurrentSubmissionVersion());
			} else {
				fail("Unknown submission returned");
			}
		}
		
		// double check that it is restricted by the assignments we pass
		assignList = new ArrayList();
		assignList.add(testData.a2);
		// there are no submissions for this user for the passed assign
		submissions = assignmentDao.getCurrentAssignmentSubmissionsForStudent(assignList, AssignmentTestDataLoad.STUDENT1_UID);
		assertTrue(submissions.isEmpty());
		// there is one submissions for this user for the assign
		submissions = assignmentDao.getCurrentAssignmentSubmissionsForStudent(assignList, AssignmentTestDataLoad.STUDENT2_UID);
		assertTrue(submissions.size() == 1);
	}
	
	public void testGetCurrentSubmissionsForStudentsForAssignment() throws Exception {
		// pass a null assignment
		try {
			assignmentDao.getCurrentSubmissionsForStudentsForAssignment(new ArrayList(), null);
			fail("did not catch null assignment passed to getCurrentSubmissionsForStudentsForAssignment");
		} catch(IllegalArgumentException iae) {
		}
		
		// null student list - should return empty list
		Set submissions = assignmentDao.getCurrentSubmissionsForStudentsForAssignment(null, testData.a1);
		assertTrue(submissions.isEmpty());
		
		// add one real and one "fake" student
		List studentList = new ArrayList();
		studentList.add(AssignmentTestDataLoad.STUDENT1_UID);
		studentList.add("bogusStudent"); // shouldn't cause any problems
		
		// there should be 1 submission returned
		submissions = assignmentDao.getCurrentSubmissionsForStudentsForAssignment(studentList, testData.a1);
		assertTrue(submissions.size() == 1);
		for (Iterator subIter = submissions.iterator(); subIter.hasNext();) {
			AssignmentSubmission thisSub = (AssignmentSubmission) subIter.next();
			// double check that the currentVersion was populated correctly
			assertNotNull(thisSub.getCurrentSubmissionVersion());
			assertTrue(thisSub.getCurrentSubmissionVersion().getId().equals(testData.st1a1CurrVersion.getId()));
		}
		
		// add another student with a submission to the list and double check currentVersion
		// was populated correctly
		studentList.add(AssignmentTestDataLoad.STUDENT2_UID);
		submissions = assignmentDao.getCurrentSubmissionsForStudentsForAssignment(studentList, testData.a1);
		assertTrue(submissions.size() == 2);
		for (Iterator subIter = submissions.iterator(); subIter.hasNext();) {
			AssignmentSubmission thisSub = (AssignmentSubmission) subIter.next();
			if (thisSub.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID)) {
				assertNotNull(thisSub.getCurrentSubmissionVersion());
				assertTrue(thisSub.getCurrentSubmissionVersion().getId().equals(testData.st1a1CurrVersion.getId()));
			} else if (thisSub.getUserId().equals(AssignmentTestDataLoad.STUDENT2_UID)) {
				assertNotNull(thisSub.getCurrentSubmissionVersion());
				assertTrue(thisSub.getCurrentSubmissionVersion().getId().equals(testData.st2a1CurrVersion.getId()));
			} else {
				fail("Unknown submission returned by getCurrentSubmissionsForStudentsForAssignment");
			}
		}
		
		// there should only be 1 submission for this assignment
		submissions = assignmentDao.getCurrentSubmissionsForStudentsForAssignment(studentList, testData.a2);
		assertTrue(submissions.size() == 1);
		for (Iterator subIter = submissions.iterator(); subIter.hasNext();) {
			AssignmentSubmission thisSub = (AssignmentSubmission) subIter.next();
			// there shouldn't be a currentVersion for this submission
			assertNull(thisSub.getCurrentSubmissionVersion());
		}
	}
	
	public void testGetSubmissionWithVersionHistoryForStudentAndAssignment() throws Exception {
		try {
			// pass a null student
			assignmentDao.getSubmissionWithVersionHistoryForStudentAndAssignment(null, new Assignment2());
			fail("Did not catch null student passed to getSubmissionWithVersionHistoryForStudentAndAssignment");
		} catch (IllegalArgumentException iae) {
		}
		try {
			// pass a non-persisted assignment
			assignmentDao.getSubmissionWithVersionHistoryForStudentAndAssignment(AssignmentTestDataLoad.STUDENT1_UID, null);
			fail("Did not catch null assignment passed to getSubmissionWithVersionHistoryForStudentAndAssignment");
		} catch (IllegalArgumentException iae) {}
		
		// should have no submission
		AssignmentSubmission submission = assignmentDao.getSubmissionWithVersionHistoryForStudentAndAssignment(AssignmentTestDataLoad.STUDENT1_UID, testData.a2);
		assertNull(submission);
		
		// has submission
		submission = assignmentDao.getSubmissionWithVersionHistoryForStudentAndAssignment(AssignmentTestDataLoad.STUDENT1_UID, testData.a1);
		assertNotNull(submission);
		// double check that the current version is populated correctly
		assertNotNull(submission.getCurrentSubmissionVersion());
		assertTrue(submission.getCurrentSubmissionVersion().getId().equals(testData.st1a1CurrVersion.getId()));
		// check that the submission history is populated correctly
		assertNotNull(submission.getSubmissionHistorySet());
		assertTrue(submission.getSubmissionHistorySet().size() == 1);
		
		// this student should have a submission
		submission = assignmentDao.getSubmissionWithVersionHistoryForStudentAndAssignment(AssignmentTestDataLoad.STUDENT2_UID, testData.a1);
		assertNotNull(submission);
		// double check that the current version is populated correctly
		assertNotNull(submission.getCurrentSubmissionVersion());
		assertTrue(submission.getCurrentSubmissionVersion().getId().equals(testData.st2a1CurrVersion.getId()));
		// check that the submission history is populated correctly
		assertNotNull(submission.getSubmissionHistorySet());
		assertTrue(submission.getSubmissionHistorySet().size() == 3);
		// double check that the attachments are correct for the returned versions
		for (Iterator histIter = submission.getSubmissionHistorySet().iterator(); histIter.hasNext();) {
			AssignmentSubmissionVersion vers = (AssignmentSubmissionVersion) histIter.next();
			// check that the history versions have attach populated correctly
			if (vers.getId().equals(testData.st2a1Version1.getId())) {
				assertTrue(vers.getSubmissionAttachSet().size() == 1);
				assertTrue(vers.getFeedbackAttachSet().isEmpty());
			} else if (vers.getId().equals(testData.st2a1Version2.getId())) {
				assertTrue(vers.getSubmissionAttachSet().isEmpty());
				assertTrue(vers.getFeedbackAttachSet().isEmpty());
			} else if (vers.getId().equals(testData.st2a1CurrVersion.getId())) {
				assertTrue(vers.getSubmissionAttachSet().size() == 2);
				assertTrue(vers.getFeedbackAttachSet().size() == 2);
			} else {
				fail("Unknown version included in history");
			}
		}
		
		// submission w/ no versions
		submission = assignmentDao.getSubmissionWithVersionHistoryForStudentAndAssignment(AssignmentTestDataLoad.STUDENT2_UID, testData.a2);
		assertNotNull(submission);
		// there should be no current version
		assertNull(submission.getCurrentSubmissionVersion());
		// there should be no history
		assertTrue(submission.getSubmissionHistorySet().isEmpty());
	}

	public void testGetSubmissionsWithVersionHistoryForStudentListAndAssignment() throws Exception {
		// try a null assignment
		try {
			assignmentDao.getSubmissionsWithVersionHistoryForStudentListAndAssignment(new ArrayList(), null);
			fail("did not catch null assignment passed to getSubmissionsWithVersionHistoryForStudentListAndAssignment");
		} catch(IllegalArgumentException iae) {
		}
		
		// null student list - should return empty submission list
		Set submissions = assignmentDao.getSubmissionsWithVersionHistoryForStudentListAndAssignment(null, testData.a1);
		assertTrue(submissions.isEmpty());
		
		// add two students - one is not associated with this class
		List studentList = new ArrayList();
		studentList.add(AssignmentTestDataLoad.STUDENT1_UID);
		studentList.add("bogusStudent"); // shouldn't cause any problems
		
		// should return one submission for this assignment
		submissions = assignmentDao.getSubmissionsWithVersionHistoryForStudentListAndAssignment(studentList, testData.a1);
		assertTrue(submissions.size() == 1);
		for (Iterator subIter = submissions.iterator(); subIter.hasNext();) {
			AssignmentSubmission thisSub = (AssignmentSubmission) subIter.next();
			// check that current  version is populated correctly
			assertNotNull(thisSub.getCurrentSubmissionVersion());
			assertTrue(thisSub.getCurrentSubmissionVersion().getId().equals(testData.st1a1CurrVersion.getId()));
			// check that submission history is populated correctly
			assertNotNull(thisSub.getSubmissionHistorySet());
			assertTrue(thisSub.getSubmissionHistorySet().size() == 1);
		}
		
		// add another student with a submission to the list
		studentList.add(AssignmentTestDataLoad.STUDENT2_UID);
		submissions = assignmentDao.getSubmissionsWithVersionHistoryForStudentListAndAssignment(studentList, testData.a1);
		assertTrue(submissions.size() == 2);
		for (Iterator subIter = submissions.iterator(); subIter.hasNext();) {
			AssignmentSubmission thisSub = (AssignmentSubmission) subIter.next();
			if (thisSub.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID)) {
				// check that current  version is populated correctly
				assertNotNull(thisSub.getCurrentSubmissionVersion());
				assertTrue(thisSub.getCurrentSubmissionVersion().getId().equals(testData.st1a1CurrVersion.getId()));
				assertTrue(thisSub.getCurrentSubmissionVersion().getSubmissionAttachSet().isEmpty());
				assertTrue(thisSub.getCurrentSubmissionVersion().getFeedbackAttachSet().isEmpty());
				// check that submission history is populated correctly
				assertTrue(thisSub.getSubmissionHistorySet().size() == 1);
			} else if (thisSub.getUserId().equals(AssignmentTestDataLoad.STUDENT2_UID)) {
				// check that current  version is populated correctly
				assertNotNull(thisSub.getCurrentSubmissionVersion());
				assertTrue(thisSub.getCurrentSubmissionVersion().getId().equals(testData.st2a1CurrVersion.getId()));
				assertTrue(thisSub.getCurrentSubmissionVersion().getSubmissionAttachSet().size() == 2);
				assertTrue(thisSub.getCurrentSubmissionVersion().getFeedbackAttachSet().size() == 2);
				
				// check that submission history is populated correctly
				assertTrue(thisSub.getSubmissionHistorySet().size() == 3);
				for (Iterator histIter = thisSub.getSubmissionHistorySet().iterator(); histIter.hasNext();) {
					AssignmentSubmissionVersion vers = (AssignmentSubmissionVersion) histIter.next();
					// check that the history versions have attach populated correctly
					if (vers.getId().equals(testData.st2a1Version1.getId())) {
						assertTrue(vers.getSubmissionAttachSet().size() == 1);
						assertTrue(vers.getFeedbackAttachSet().isEmpty());
					} else if (vers.getId().equals(testData.st2a1Version2.getId())) {
						assertTrue(vers.getSubmissionAttachSet().isEmpty());
						assertTrue(vers.getFeedbackAttachSet().isEmpty());
					} else if (vers.getId().equals(testData.st2a1CurrVersion.getId())) {
						assertTrue(vers.getSubmissionAttachSet().size() == 2);
						assertTrue(vers.getFeedbackAttachSet().size() == 2);
					} else {
						fail("Unknown version included in history");
					}
				}
			} else {
				fail("Unknown submission returned by getSubmissionsWithVersionHistoryForStudentListAndAssignment");
			}
		}
		
		// now try an assignment with only 1 submission
		submissions = assignmentDao.getSubmissionsWithVersionHistoryForStudentListAndAssignment(studentList, testData.a2);
		assertTrue(submissions.size() == 1);
		for (Iterator subIter = submissions.iterator(); subIter.hasNext();) {
			AssignmentSubmission thisSub = (AssignmentSubmission) subIter.next();
			// there should be no current version or history for this submission
			assertNull(thisSub.getCurrentSubmissionVersion());
			assertTrue(thisSub.getSubmissionHistorySet().isEmpty());
		}
	}
	
	public void testGetAssignmentSubmissionVersionByIdWithAttachments() throws Exception {
		// try passing a null version id
		try {
			assignmentDao.getAssignmentSubmissionVersionByIdWithAttachments(null);
			fail("Did not catch null id passed to getAssignmentSubmissionVersionByIdWithAttachments");
		} catch(IllegalArgumentException iae) {
		}
		// try an id that doesn't exist - should return null
		AssignmentSubmissionVersion version = assignmentDao.getAssignmentSubmissionVersionByIdWithAttachments(new Long(1234567));
		assertNull(version);
		
		// try a real one
		version = assignmentDao.getAssignmentSubmissionVersionByIdWithAttachments(testData.st1a1CurrVersion.getId());
		assertNotNull(version);
		assertNotNull(version.getAssignmentSubmission());
		assertTrue(version.getAssignmentSubmission().getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID));
		assertTrue(version.getSubmissionAttachSet().isEmpty());
		assertTrue(version.getFeedbackAttachSet().isEmpty());
		
		// one with attachments
		version = assignmentDao.getAssignmentSubmissionVersionByIdWithAttachments(testData.st2a1CurrVersion.getId());
		assertNotNull(version);
		assertNotNull(version.getAssignmentSubmission());
		assertTrue(version.getAssignmentSubmission().getUserId().equals(AssignmentTestDataLoad.STUDENT2_UID));
		assertTrue(version.getSubmissionAttachSet().size() == 2);
		assertTrue(version.getFeedbackAttachSet().size() == 2);
	}
	
	public void testGetSubmissionWithVersionHistoryById() throws Exception {
		// try a null submissionId
		try {
			assignmentDao.getSubmissionWithVersionHistoryById(null);
			fail("Did not catch null submissionId passed to getSubmissionWithVersionHistoryById");
		} catch(IllegalArgumentException iae) {
		}
		
		// try a submission id that doesn't exist
		AssignmentSubmission submission = assignmentDao.getSubmissionWithVersionHistoryById(new Long(12345));
		assertNull(submission);
		
		// try a submission with no versions
		submission = assignmentDao.getSubmissionWithVersionHistoryById(testData.st2a2SubmissionNoVersions.getId());
		assertNotNull(submission);
		assertNull(submission.getCurrentSubmissionVersion());
		assertTrue(submission.getSubmissionHistorySet().isEmpty());
		
		// try one with versions
		submission = assignmentDao.getSubmissionWithVersionHistoryById(testData.st2a1Submission.getId());
		assertNotNull(submission);
		assertTrue(submission.getCurrentSubmissionVersion().getId().equals(testData.st2a1CurrVersion.getId()));
		// double check that the history was populated correctly
		assertTrue(submission.getSubmissionHistorySet().size() == 3);
	}
	
	public void testGetVersionHistoryForSubmission() {
		// try a null submission
		try {
			assignmentDao.getVersionHistoryForSubmission(null);
			fail("did not catch null submission passed to getVersionHistoryForSubmission");
		} catch (IllegalArgumentException iae) {}
		
		// let's try a few different submissions
		Set history = assignmentDao.getVersionHistoryForSubmission(testData.st1a1Submission);
		assertEquals(history.size(), 1);
		
		history = assignmentDao.getVersionHistoryForSubmission(testData.st2a2SubmissionNoVersions);
		assertEquals(history.size(), 0);
		
		history = assignmentDao.getVersionHistoryForSubmission(testData.st2a1Submission);
		assertEquals(history.size(), 3);
		
	}

	/*public void testGetVersionByUserIdAndSubmittedTime() throws Exception
	{
		String userId = null;
		Date submittedTime = null;
		try
		{
			assignmentDao.getVersionByUserIdAndSubmittedTime(userId, submittedTime);
			fail("Should've thrown exception with userId == submittedTime == null");
		}
		catch (IllegalArgumentException iae)
		{
			// expected response
		}

		try
		{
			userId = AssignmentTestDataLoad.STUDENT1_UID;
			submittedTime = null;
			assignmentDao.getVersionByUserIdAndSubmittedTime(userId, submittedTime);
			fail("Should've thrown exception with userId != null and submittedTime == null");
		}
		catch (IllegalArgumentException iae)
		{
			// expected response
		}

		try
		{
			userId = null;
			submittedTime = testData.st1a1CurrVersion.getSubmittedTime();
			assignmentDao.getVersionByUserIdAndSubmittedTime(userId, submittedTime);
			fail("Should've thrown exception with userId == null and submittedTime != null");
		}
		catch (IllegalArgumentException iae)
		{
			// expected response
		}

		try
		{
			userId = AssignmentTestDataLoad.STUDENT1_UID;
			submittedTime = testData.st1a1CurrVersion.getSubmittedTime();
			AssignmentSubmissionVersion asv = assignmentDao.getVersionByUserIdAndSubmittedTime(
					userId, submittedTime);
			assertEquals(asv.getCreatedBy(), userId);
			assertEquals(asv.getSubmittedTime(), submittedTime);
		}
		catch (IllegalArgumentException iae)
		{
			fail("Shouldn't have thrown exception with userId != null and submittedTime != null");
		}
	}*/
}