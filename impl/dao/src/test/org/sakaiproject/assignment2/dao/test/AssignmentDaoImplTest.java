/**
 * 
 */

package org.sakaiproject.assignment2.dao.test;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.Iterator;

import org.sakaiproject.assignment2.dao.AssignmentDao;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
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
		assertEquals(2, highestIndex);
	}

	public void testGetAssignmentsWithGroupsAndAttachments() {
		try {
			assignmentDao.getAssignmentsWithGroupsAndAttachments(null);
			fail("did not catch null parameter passed to getAssignmentsWithGroupsAndAttachments");
		} catch (Exception e) {
			
		}
		
		Set<Assignment2> assignments = assignmentDao.getAssignmentsWithGroupsAndAttachments(AssignmentTestDataLoad.BAD_CONTEXT);
		assertNotNull(assignments);
		assertTrue(assignments.isEmpty());

		assignments = assignmentDao.getAssignmentsWithGroupsAndAttachments(AssignmentTestDataLoad.CONTEXT_ID);
		assertNotNull(assignments);
		assertTrue(assignments.size() == 3);
		
		for (Iterator assignIter = assignments.iterator(); assignIter.hasNext();) {
			Assignment2 assign = (Assignment2) assignIter.next();
			if (assign.getId().equals(testData.a1Id)) {
				System.out.println("A1: " + assign.getTitle() + " " + testData.a1Id);
				assertTrue(assign.getAttachmentSet().size() == 2);
				assertTrue(assign.getAssignmentGroupSet().size() == 2);
			} else if (assign.getId().equals(testData.a2Id)) {
				assertTrue(assign.getAttachmentSet().isEmpty());
				assertTrue(assign.getAssignmentGroupSet().size() == 1);
			} else if (assign.getId().equals(testData.a3Id)) {
				assertNotNull(assign.getAttachmentSet());
				assertTrue(assign.getAttachmentSet().size() == 1);
				assertTrue(assign.getAssignmentGroupSet().isEmpty());
			}
		}
		
	}
	
	public void testGetAssignmentByIdWithGroups() {
		try {
			assignmentDao.getAssignmentByIdWithGroups(null);
			fail("did not catch null parameter passed to getAssignmentByIdWithGroups");
		} catch (Exception e) {
			
		}
		
		// now try an id that doesn't exist
		Assignment2 assign = assignmentDao.getAssignmentByIdWithGroups(new Long(27));
		assertNull(assign);
		
		// now let's try some we know exist
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
		try {
			assignmentDao.getAssignmentByIdWithGroupsAndAttachments(null);
			fail("did not catch null parameter passed to getAssignmentByIdWithGroupsAndAttachments");
		} catch (Exception e) {
			
		}
		
		// now try an id that doesn't exist
		Assignment2 assign = assignmentDao.getAssignmentByIdWithGroupsAndAttachments(new Long(27));
		assertNull(assign);
		
		// now let's try some we know exist
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
		try {
			AssignmentSubmissionVersion version = assignmentDao.getCurrentSubmissionVersionWithAttachments(null);
			fail("did not catch null submission passed to getCurrentSubmissionVersionWithAttachments");
			
			// what happens if submission doesn't exist?
			AssignmentSubmission submission = new AssignmentSubmission();
			version = assignmentDao.getCurrentSubmissionVersionWithAttachments(submission);
			fail("did not catch submission w/ no id passed to getCurrentSubmissionVersionWithAttachments");
		} catch (IllegalArgumentException iae) {
		}

		AssignmentSubmissionVersion version = assignmentDao.getCurrentSubmissionVersionWithAttachments(testData.st1a1Submission);
		assertNotNull(version);
		assertTrue(version.getId().equals(testData.st1a1CurrVersion.getId()));
		assertTrue(version.getSubmissionAttachSet().isEmpty());
		
		version = assignmentDao.getCurrentSubmissionVersionWithAttachments(testData.st2a1Submission);
		assertNotNull(version);
		assertTrue(version.getId().equals(testData.st2a1CurrVersion.getId()));
		assertNotNull(version.getSubmissionAttachSet());
		assertTrue(version.getSubmissionAttachSet().size() == 2);
		assertTrue(version.getFeedbackAttachSet().size() == 2);
		
		// test out a submission w/o a version
		version = assignmentDao.getCurrentSubmissionVersionWithAttachments(testData.st2a2SubmissionNoVersions);
		assertNull(version);
	}
	
	public void testGetCurrentAssignmentSubmissionsForStudent() throws Exception {
		try {
			assignmentDao.getCurrentAssignmentSubmissionsForStudent(new ArrayList(), null);
			fail("method getCurrentAssignmentSubmissionsForStudent did not catch null student parameter");
		} catch (IllegalArgumentException iae) {
		}
		
		// pass a null assignments list
		List submissions = assignmentDao.getCurrentAssignmentSubmissionsForStudent(null, AssignmentTestDataLoad.STUDENT1_UID);
		assertTrue(submissions.isEmpty());
		// pass an empty assignments list
		submissions = assignmentDao.getCurrentAssignmentSubmissionsForStudent(new ArrayList(), AssignmentTestDataLoad.STUDENT1_UID);
		assertTrue(submissions.isEmpty());
		
		List assignList = new ArrayList();
		assignList.add(testData.a1);
		assignList.add(testData.a2);
		
		// try a student who won't have any
		submissions = assignmentDao.getCurrentAssignmentSubmissionsForStudent(assignList, "bogusStudent");
		assertTrue(submissions.isEmpty());
		
		submissions = assignmentDao.getCurrentAssignmentSubmissionsForStudent(assignList, AssignmentTestDataLoad.STUDENT1_UID);
		assertNotNull(submissions);
		assertTrue(submissions.size() == 1);
		AssignmentSubmission sub = (AssignmentSubmission)submissions.get(0);
		assertNotNull(sub.getCurrentSubmissionVersion());
		assertTrue(sub.getCurrentSubmissionVersion().getId().equals(testData.st1a1CurrVersion.getId()));
		
		submissions = assignmentDao.getCurrentAssignmentSubmissionsForStudent(assignList, AssignmentTestDataLoad.STUDENT2_UID);
		assertNotNull(submissions);
		assertTrue(submissions.size() == 2);
		for (Iterator subIter = submissions.iterator(); subIter.hasNext();) {
			AssignmentSubmission thisSub = (AssignmentSubmission) subIter.next();
			if (thisSub.getAssignment().getId().equals(testData.a1.getId())) {
				assertNotNull(thisSub.getCurrentSubmissionVersion());
				assertTrue(thisSub.getCurrentSubmissionVersion().getId().equals(testData.st2a1CurrVersion.getId()));
			} else if (thisSub.getAssignment().getId().equals(testData.a2.getId())) {
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
		try {
			assignmentDao.getCurrentSubmissionsForStudentsForAssignment(new ArrayList(), null);
			fail("did not catch null assignment passed to getCurrentSubmissionsForStudentsForAssignment");
		} catch(IllegalArgumentException iae) {
		}
		
		// null student list
		Set submissions = assignmentDao.getCurrentSubmissionsForStudentsForAssignment(null, testData.a1);
		assertTrue(submissions.isEmpty());
		
		List studentList = new ArrayList();
		studentList.add(AssignmentTestDataLoad.STUDENT1_UID);
		studentList.add("bogusStudent"); // shouldn't cause any problems
		
		submissions = assignmentDao.getCurrentSubmissionsForStudentsForAssignment(studentList, testData.a1);
		assertTrue(submissions.size() == 1);
		for (Iterator subIter = submissions.iterator(); subIter.hasNext();) {
			AssignmentSubmission thisSub = (AssignmentSubmission) subIter.next();
			assertNotNull(thisSub.getCurrentSubmissionVersion());
			assertTrue(thisSub.getCurrentSubmissionVersion().getId().equals(testData.st1a1CurrVersion.getId()));
		}
		
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
		
		submissions = assignmentDao.getCurrentSubmissionsForStudentsForAssignment(studentList, testData.a2);
		assertTrue(submissions.size() == 1);
		for (Iterator subIter = submissions.iterator(); subIter.hasNext();) {
			AssignmentSubmission thisSub = (AssignmentSubmission) subIter.next();
			assertNull(thisSub.getCurrentSubmissionVersion());
		}
	}
	
	public void testGetSubmissionWithVersionHistoryForStudentAndAssignment() throws Exception {
		try {
			assignmentDao.getSubmissionWithVersionHistoryForStudentAndAssignment(null, new Assignment2());
			fail("Did not catch null student passed to getSubmissionWithVersionHistoryForStudentAndAssignment");
			assignmentDao.getSubmissionWithVersionHistoryForStudentAndAssignment(AssignmentTestDataLoad.STUDENT1_UID, null);
			fail("Did not catch null assignment passed to getSubmissionWithVersionHistoryForStudentAndAssignment");
		} catch (IllegalArgumentException iae) {
		}
		
		// should have no submission
		AssignmentSubmission submission = assignmentDao.getSubmissionWithVersionHistoryForStudentAndAssignment(AssignmentTestDataLoad.STUDENT1_UID, testData.a2);
		assertNull(submission);
		
		// has submission
		submission = assignmentDao.getSubmissionWithVersionHistoryForStudentAndAssignment(AssignmentTestDataLoad.STUDENT1_UID, testData.a1);
		assertNotNull(submission);
		assertNotNull(submission.getCurrentSubmissionVersion());
		assertTrue(submission.getCurrentSubmissionVersion().getId().equals(testData.st1a1CurrVersion.getId()));
		assertNotNull(submission.getSubmissionHistorySet());
		assertTrue(submission.getSubmissionHistorySet().size() == 1);
		
		submission = assignmentDao.getSubmissionWithVersionHistoryForStudentAndAssignment(AssignmentTestDataLoad.STUDENT2_UID, testData.a1);
		assertNotNull(submission);
		assertNotNull(submission.getCurrentSubmissionVersion());
		assertTrue(submission.getCurrentSubmissionVersion().getId().equals(testData.st2a1CurrVersion.getId()));
		assertNotNull(submission.getSubmissionHistorySet());
		assertTrue(submission.getSubmissionHistorySet().size() == 3);
		
		// submission w/ no versions
		submission = assignmentDao.getSubmissionWithVersionHistoryForStudentAndAssignment(AssignmentTestDataLoad.STUDENT2_UID, testData.a2);
		assertNotNull(submission);
		assertNull(submission.getCurrentSubmissionVersion());
		assertTrue(submission.getSubmissionHistorySet().isEmpty());
	}

	public void testGetSubmissionsWithVersionHistoryForStudentListAndAssignment() throws Exception {
		try {
			assignmentDao.getSubmissionsWithVersionHistoryForStudentListAndAssignment(new ArrayList(), null);
			fail("did not catch null assignment passed to getSubmissionsWithVersionHistoryForStudentListAndAssignment");
		} catch(IllegalArgumentException iae) {
		}
		
		// null student list
		Set submissions = assignmentDao.getSubmissionsWithVersionHistoryForStudentListAndAssignment(null, testData.a1);
		assertTrue(submissions.isEmpty());
		
		List studentList = new ArrayList();
		studentList.add(AssignmentTestDataLoad.STUDENT1_UID);
		studentList.add("bogusStudent"); // shouldn't cause any problems
		
		submissions = assignmentDao.getSubmissionsWithVersionHistoryForStudentListAndAssignment(studentList, testData.a1);
		assertTrue(submissions.size() == 1);
		for (Iterator subIter = submissions.iterator(); subIter.hasNext();) {
			AssignmentSubmission thisSub = (AssignmentSubmission) subIter.next();
			assertNotNull(thisSub.getCurrentSubmissionVersion());
			assertTrue(thisSub.getCurrentSubmissionVersion().getId().equals(testData.st1a1CurrVersion.getId()));
			assertNotNull(thisSub.getSubmissionHistorySet());
			assertTrue(thisSub.getSubmissionHistorySet().size() == 1);
		}
		
		studentList.add(AssignmentTestDataLoad.STUDENT2_UID);
		submissions = assignmentDao.getSubmissionsWithVersionHistoryForStudentListAndAssignment(studentList, testData.a1);
		assertTrue(submissions.size() == 2);
		for (Iterator subIter = submissions.iterator(); subIter.hasNext();) {
			AssignmentSubmission thisSub = (AssignmentSubmission) subIter.next();
			if (thisSub.getUserId().equals(AssignmentTestDataLoad.STUDENT1_UID)) {
				assertNotNull(thisSub.getCurrentSubmissionVersion());
				assertTrue(thisSub.getCurrentSubmissionVersion().getId().equals(testData.st1a1CurrVersion.getId()));
				assertTrue(thisSub.getSubmissionHistorySet().size() == 1);
			} else if (thisSub.getUserId().equals(AssignmentTestDataLoad.STUDENT2_UID)) {
				assertNotNull(thisSub.getCurrentSubmissionVersion());
				assertTrue(thisSub.getCurrentSubmissionVersion().getId().equals(testData.st2a1CurrVersion.getId()));
				assertTrue(thisSub.getSubmissionHistorySet().size() == 3);
			} else {
				fail("Unknown submission returned by getSubmissionsWithVersionHistoryForStudentListAndAssignment");
			}
		}
		
		submissions = assignmentDao.getSubmissionsWithVersionHistoryForStudentListAndAssignment(studentList, testData.a2);
		assertTrue(submissions.size() == 1);
		for (Iterator subIter = submissions.iterator(); subIter.hasNext();) {
			AssignmentSubmission thisSub = (AssignmentSubmission) subIter.next();
			assertNull(thisSub.getCurrentSubmissionVersion());
			assertTrue(thisSub.getSubmissionHistorySet().isEmpty());
		}
	}
	
	public void testGetAssignmentSubmissionVersionByIdWithAttachments() throws Exception {
		try {
			assignmentDao.getAssignmentSubmissionVersionByIdWithAttachments(null);
			fail("Did not catch null id passed to getAssignmentSubmissionVersionByIdWithAttachments");
		} catch(IllegalArgumentException iae) {
		}
		// try a bogus id
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
		try {
			assignmentDao.getSubmissionWithVersionHistoryById(null);
			fail("Did not catch null submissionId passed to getSubmissionWithVersionHistoryById");
		} catch(IllegalArgumentException iae) {
		}
		
		// try a bogus id
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
		assertTrue(submission.getSubmissionHistorySet().size() == 3);
	}
}
