/**
 * 
 */

package org.sakaiproject.assignment2.dao.test;

import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.Iterator;

import org.sakaiproject.assignment2.dao.AssignmentDao;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.springframework.test.AbstractTransactionalSpringContextTests;



/**
 * @author michellewagner (this was totally written by michelle and only michelle and no one else should take credit for it)
 * 
 */
public class AssignmentDaoImplTest extends
		AbstractTransactionalSpringContextTests {

	private static final String CONTEXT_ID = "bogusContext";
	protected AssignmentDao assignmentDao;

	protected String[] getConfigLocations() {
		// point to the needed spring config files, must be on the classpath
		// (add component/src/webapp/WEB-INF to the build path in Eclipse),
		// they also need to be referenced in the pom.xml file
		return new String[] { "hibernate-test.xml", "spring-hibernate.xml" };
	}

	// run this before each test starts
	protected void onSetUpBeforeTransaction() throws Exception {
		// load the spring created dao class bean from the Spring Application
		// Context
		assignmentDao = (AssignmentDao) applicationContext
				.getBean("org.sakaiproject.assignment2.dao.AssignmentDao");
		if (assignmentDao == null) {
			throw new NullPointerException(
					"DAO could not be retrieved from spring");
		}

		// init the test class if needed

	}

	// run this before each test starts and as part of the transaction
	protected void onSetUpInTransaction() {
		// preload additional data if desired

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
		int highestIndex = assignmentDao.getHighestSortIndexInSite(CONTEXT_ID);
		assertEquals(0, highestIndex);
		
		// exception testing
		try {
			assignmentDao.getHighestSortIndexInSite(null);
			fail("Should have thrown exception");
		} catch (IllegalArgumentException e) {
			assertNotNull(e);
		}
		
		// positive test
		// count 2 or 3 items in the known context
		Assignment2 a1 = createGenericAssignment2Object("Assignment 1");
		a1.setSortIndex(0);
		assignmentDao.save(a1);
		
		highestIndex = assignmentDao.getHighestSortIndexInSite(CONTEXT_ID);
		assertEquals(0, highestIndex);
		
		Assignment2 a2 = createGenericAssignment2Object("Assignment 2");
		a2.setSortIndex(1);
		assignmentDao.save(a2);
		
		highestIndex = assignmentDao.getHighestSortIndexInSite(CONTEXT_ID);
		assertEquals(1, highestIndex);
	}

	public void testGetAssignmentsWithGroupsAndAttachments() {
		try {
			assignmentDao.getAssignmentsWithGroupsAndAttachments(null);
			fail("did not catch null parameter passed to getAssignmentsWithGroupsAndAttachments");
		} catch (Exception e) {
			
		}
		
		Set<Assignment2> assignments = assignmentDao.getAssignmentsWithGroupsAndAttachments(CONTEXT_ID);
		assertNotNull(assignments);
		assertTrue(assignments.isEmpty());
		
		Assignment2 a1 = createGenericAssignment2Object("Assignment 1");
		Assignment2 a2 = createGenericAssignment2Object("Assignment 2");
		Assignment2 a3 = createGenericAssignment2Object("Assignment 3");
		assignmentDao.create(a1);
		assignmentDao.create(a2);
		assignmentDao.create(a3);
		
		// add attachments
		// to Assignment 1
		AssignmentAttachment attach1 = new AssignmentAttachment();
		attach1.setAssignment(a1);
		attach1.setAttachmentReference("reference1");
		assignmentDao.save(attach1);
		
		AssignmentAttachment attach2 = new AssignmentAttachment();
		attach2.setAssignment(a1);
		attach2.setAttachmentReference("reference2");
		assignmentDao.save(attach2);
		// to Assignment 3
		AssignmentAttachment attach3 = new AssignmentAttachment();
		attach3.setAssignment(a3);
		attach3.setAttachmentReference("reference1");
		assignmentDao.save(attach3);
		
		// add AssignmentGroups
		AssignmentGroup group1 = new AssignmentGroup();
		group1.setAssignment(a1);
		group1.setGroupId("Group1");
		assignmentDao.save(group1);
		
		AssignmentGroup group2 = new AssignmentGroup();
		group2.setAssignment(a1);
		group2.setGroupId("Group2");
		assignmentDao.save(group2);
		
		assignments = assignmentDao.getAssignmentsWithGroupsAndAttachments(CONTEXT_ID);
		assertNotNull(assignments);
		assertTrue(assignments.size() == 3);
		
		/*for (Iterator assignIter = assignments.iterator(); assignIter.hasNext();) {
			Assignment2 assign = (Assignment2) assignIter.next();
			if (assign.getTitle().equals("Assignment 1")) {
				assertTrue(assign.getAttachmentSet().size() == 2);
				assertTrue(assign.getAssignmentGroupSet().size() == 2);
			}
		}*/
		
	}
	
	private Assignment2 createGenericAssignment2Object(String title) {
		Assignment2 assignment = new Assignment2();
		assignment.setAllowResubmit(Boolean.FALSE);
		assignment.setContextId(CONTEXT_ID);
		assignment.setCreateTime(new Date());
		assignment.setCreator("ADMIN");
		assignment.setDraft(Boolean.FALSE);
		assignment.setInstructions("Summarize the article we discussed on 1/8");
		assignment.setNotificationType(AssignmentConstants.NOTIFY_NONE);
		assignment.setOpenTime(new Date());
		assignment.setRemoved(Boolean.FALSE);
		assignment.setRestrictedToGroups(Boolean.FALSE);
		assignment.setSubmissionType(AssignmentConstants.SUBMIT_INLINE_AND_ATTACH);
		assignment.setUngraded(Boolean.FALSE);
		assignment.setHonorPledge(Boolean.FALSE);
		assignment.setHasAnnouncement(Boolean.FALSE);
		assignment.setTitle(title);
		
		return assignment;
	}

}
