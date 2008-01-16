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
import org.sakaiproject.assignment2.test.PreloadTestData;
import org.sakaiproject.assignment2.test.AssignmentTestDataLoad;
import org.springframework.test.AbstractTransactionalSpringContextTests;



/**
 * @author michellewagner (this was totally written by michelle and only michelle and no one else should take credit for it)
 * 
 */
public class AssignmentDaoImplTest extends
		AbstractTransactionalSpringContextTests {
	
	protected AssignmentDao assignmentDao;
	protected AssignmentTestDataLoad testData;

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
		
		PreloadTestData ptd = (PreloadTestData) applicationContext.getBean("org.sakaiproject.assignment2.test.PreloadTestData");
		if (ptd == null) {
			throw new NullPointerException("PreloadTestData could not be retrieved from spring");
		}
		
		testData = ptd.getAtdl();
	}

	// run this before each test starts and as part of the transaction
	protected void onSetUpInTransaction() {
		// preload additional data if desired
		// initialize data
		
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
		int highestIndex = assignmentDao.getHighestSortIndexInSite(testData.BAD_CONTEXT);
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
		highestIndex = assignmentDao.getHighestSortIndexInSite(testData.CONTEXT_ID);
		assertEquals(2, highestIndex);
	}

	public void testGetAssignmentsWithGroupsAndAttachments() {
		try {
			assignmentDao.getAssignmentsWithGroupsAndAttachments(null);
			fail("did not catch null parameter passed to getAssignmentsWithGroupsAndAttachments");
		} catch (Exception e) {
			
		}
		
		Set<Assignment2> assignments = assignmentDao.getAssignmentsWithGroupsAndAttachments(testData.BAD_CONTEXT);
		assertNotNull(assignments);
		assertTrue(assignments.isEmpty());

		assignments = assignmentDao.getAssignmentsWithGroupsAndAttachments(testData.CONTEXT_ID);
		assertNotNull(assignments);
		assertTrue(assignments.size() == 3);
		
		for (Iterator assignIter = assignments.iterator(); assignIter.hasNext();) {
			Assignment2 assign = (Assignment2) assignIter.next();
			if (assign.getTitle().equals(testData.ASSIGN1_TITLE)) {
				assertTrue(assign.getAttachmentSet().size() == 2);
				assertTrue(assign.getAssignmentGroupSet().size() == 2);
			} else if (assign.getTitle().equals(testData.ASSIGN2_TITLE)) {
				assertTrue(assign.getAttachmentSet().isEmpty());
				assertTrue(assign.getAssignmentGroupSet().size() == 1);
			} else if (assign.getTitle().equals(testData.ASSIGN3_TITLE)) {
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
		assertTrue(assign.getTitle().equals(testData.ASSIGN3_TITLE));
	}

}
