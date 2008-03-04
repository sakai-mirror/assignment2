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
import java.util.Map;
import java.util.Set;

import junit.framework.Assert;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.test.AssignmentTestDataLoad;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;

import org.sakaiproject.section.api.coursemanagement.CourseSection;
import org.sakaiproject.section.api.coursemanagement.Course;
import org.sakaiproject.section.api.facade.Role;


public class ExternalGradebookLogicTest extends Assignment2TestBase {

    private static final Log log = LogFactory.getLog(ExternalGradebookLogicTest.class);
    
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

    public void testGetViewableAssignmentsWithGbData() {
    	// try a null contextId
    	try {
    		gradebookLogic.getViewableAssignmentsWithGbData(new ArrayList(), null);
    		fail("Did not catch null contextId passed to getViewableAssignmentsWithGbData");
    	} catch (IllegalArgumentException iae) {}
    	
    	// make sure an empty list is returned if we pass a null list
    	List viewableAssigns = gradebookLogic.getViewableAssignmentsWithGbData(null, AssignmentTestDataLoad.CONTEXT_ID);
    	assertTrue(viewableAssigns.isEmpty());
    	
    	// now try an empty list
    	viewableAssigns = gradebookLogic.getViewableAssignmentsWithGbData(new ArrayList(), AssignmentTestDataLoad.CONTEXT_ID);
    	assertTrue(viewableAssigns.isEmpty());
    	
    	// A1 & A2 - ungraded
    	// A3 & A4 - graded
    	
    	List assignList = new ArrayList();
    	assignList.add(testData.a1);
    	assignList.add(testData.a2);
    	
    	// what happens if we pass ungraded items?
    	viewableAssigns = gradebookLogic.getViewableAssignmentsWithGbData(assignList, AssignmentTestDataLoad.CONTEXT_ID);
    	assertTrue(viewableAssigns.isEmpty());
    	
    	// let's pass graded items
    	assignList = new ArrayList();
    	assignList.add(testData.a3);
    	assignList.add(testData.a4);
    	
    	// instructor should get them both back
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	viewableAssigns = gradebookLogic.getViewableAssignmentsWithGbData(assignList, AssignmentTestDataLoad.CONTEXT_ID);
    	assertEquals(2, viewableAssigns.size());
    	
    	// make sure the gb data is populated
    	for (Iterator<Assignment2> aIter = viewableAssigns.iterator(); aIter.hasNext();) {
    		Assignment2 assign = aIter.next();
    		if (assign.getId().equals(testData.a3Id)) {
    			assertTrue(assign.getPointsPossible().equals(GB_ITEM1_PTS));
    			assertNull(assign.getDueDate());
    		} else if (assign.getId().equals(testData.a4Id)) {
    			assertTrue(assign.getPointsPossible().equals(GB_ITEM2_PTS));
    		} else {
    			fail("Unknown assignment returned!");
    		}
    	}
    	
    	// switch to TA
    	// TODO grader permissions
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	viewableAssigns = gradebookLogic.getViewableAssignmentsWithGbData(assignList, AssignmentTestDataLoad.CONTEXT_ID);
    	assertEquals(2, viewableAssigns.size());
    	
    	// make sure the gb data is populated
    	for (Iterator<Assignment2> aIter = viewableAssigns.iterator(); aIter.hasNext();) {
    		Assignment2 assign = aIter.next();
    		if (assign.getId().equals(testData.a3Id)) {
    			assertTrue(assign.getPointsPossible().equals(GB_ITEM1_PTS));
    			assertNull(assign.getDueDate());
    		} else if (assign.getId().equals(testData.a4Id)) {
    			assertTrue(assign.getPointsPossible().equals(GB_ITEM2_PTS));
    		} else {
    			fail("Unknown assignment returned!");
    		}
    	}
    	
    	// switch to student - should return all items since none are draft
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	viewableAssigns = gradebookLogic.getViewableAssignmentsWithGbData(assignList, AssignmentTestDataLoad.CONTEXT_ID);
    	assertEquals(2, viewableAssigns.size());
    }
    
    public void testCreateGradebookDataIfNecessary() {
    	// we will skip this test since it relies on ComponentManager
    	
    	// let's try to create a gradebook that already exists. nothing should happen
    	/*gradebookLogic.createGradebookDataIfNecessary(AssignmentTestDataLoad.CONTEXT_ID);
    	
    	// let's try to actually create a new one
    	String NEW_CONTEXT = "newContext";
    	// double check that it doesn't exist
    	try {
    		gradebookLogic.getAllGradebookItems(NEW_CONTEXT);
    		fail("gradebook already exists!");
    	} catch (GradebookNotFoundException gfe) {}
    	
    	// now let's add it
    	gradebookLogic.createGradebookDataIfNecessary(NEW_CONTEXT);
    	// double check that it works
    	try {
    		gradebookLogic.getAllGradebookItems(NEW_CONTEXT);
    		
    	} catch (GradebookNotFoundException gfe) {
    		fail("gradebook was not added properly!");
    	}*/
    }
    
    public void testGetViewableGradableObjectIdTitleMap() {
    	// try a null contextId
    	try {
    		gradebookLogic.getViewableGradableObjectIdTitleMap(null);
    		fail("did not catch null contextId passed to getViewableGradableObjectIdTitleMap");
    	} catch (IllegalArgumentException iae) {}
    	
    	//first, let's add another gb item to the gb that is not associated with
    	// an assignment. this one is released
    	String itemName = "New name";
    	Double itemPoints = new Double(52);
    	Date itemDue = null;
    	org.sakaiproject.service.gradebook.shared.Assignment newItem = 
        	new org.sakaiproject.service.gradebook.shared.Assignment();
    	newItem.setName(itemName);
    	newItem.setPoints(itemPoints);
    	newItem.setDueDate(itemDue);
    	newItem.setReleased(true);
        gradebookService.addAssignment(AssignmentTestDataLoad.CONTEXT_ID, newItem);
        org.sakaiproject.service.gradebook.shared.Assignment item = 
        	gradebookService.getAssignment(AssignmentTestDataLoad.CONTEXT_ID, itemName);
        Long newItemId = item.getId();
        
        // start out as the instructor
        authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
        Map goIdTitleMap = gradebookLogic.getViewableGradableObjectIdTitleMap(AssignmentTestDataLoad.CONTEXT_ID);
        assertEquals(3, goIdTitleMap.size());
        assertTrue(goIdTitleMap.get(newItemId).equals(itemName));
        assertTrue(goIdTitleMap.get(gbItem1Id).equals(GB_ITEM1_NAME));
        assertTrue(goIdTitleMap.get(gbItem2Id).equals(GB_ITEM2_NAME));
        
        // try a ta
        //TODO grader perms
        authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
        goIdTitleMap = gradebookLogic.getViewableGradableObjectIdTitleMap(AssignmentTestDataLoad.CONTEXT_ID);
        assertEquals(3, goIdTitleMap.size());
        assertTrue(goIdTitleMap.get(newItemId).equals(itemName));
        assertTrue(goIdTitleMap.get(gbItem1Id).equals(GB_ITEM1_NAME));
        assertTrue(goIdTitleMap.get(gbItem2Id).equals(GB_ITEM2_NAME));
        
        // now try a student - should only get released items
        authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
        goIdTitleMap = gradebookLogic.getViewableGradableObjectIdTitleMap(AssignmentTestDataLoad.CONTEXT_ID);
        assertEquals(1, goIdTitleMap.size());
        assertTrue(goIdTitleMap.get(newItemId).equals(itemName));
  
    }
    
    public void testGetAllGradebookItems() {
    	// try passing a null contextId
    	try {
    		gradebookLogic.getAllGradebookItems(null);
    		fail("did not catch null contextId passed to getAllGradebookItems");
    	} catch (IllegalArgumentException iae) {}
    	
    	// start as instructor
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	
    	// what if we pass a bad contextId? should throw SecurityException
    	try {
    		gradebookLogic.getAllGradebookItems(AssignmentTestDataLoad.BAD_CONTEXT);
    		fail("Did not catch SecurityException for context that doesn't exist yet");
    	} catch (SecurityException se) {}
    	
    	// should return all gb items for instructor and ta
    	List allItems = gradebookLogic.getAllGradebookItems(AssignmentTestDataLoad.CONTEXT_ID);
    	assertEquals(2, allItems.size());
    	
    	// switch to TA
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	allItems = gradebookLogic.getAllGradebookItems(AssignmentTestDataLoad.CONTEXT_ID);
    	assertEquals(2, allItems.size());
    	
    	// student should throw security exception
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	try {
    		gradebookLogic.getAllGradebookItems(AssignmentTestDataLoad.CONTEXT_ID);
    		fail("Did not catch SecurityException for student accessing all gb items");
    	} catch (SecurityException se) {}
    }
    
    public void testGetViewableGroupIdToTitleMap() {
    	// try null
    	try {
    		gradebookLogic.getViewableGroupIdToTitleMap(null);
    		fail("did not catch null contextId passed to getViewableGroupIdToTitleMap");
    	} catch (IllegalArgumentException iae) {}
    	
    	// start as instructor
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	
    	// try an invalid context
    	Map groupIdTitleMap = gradebookLogic.getViewableGroupIdToTitleMap(AssignmentTestDataLoad.BAD_CONTEXT);
    	assertTrue(groupIdTitleMap.isEmpty());
    	
    	groupIdTitleMap = gradebookLogic.getViewableGroupIdToTitleMap(AssignmentTestDataLoad.CONTEXT_ID);
    	assertEquals(groupIdTitleMap.size(), 3);
    	assertEquals((String)groupIdTitleMap.get(section1Uid), AssignmentTestDataLoad.GROUP1_NAME);
    	assertEquals((String)groupIdTitleMap.get(section2Uid), AssignmentTestDataLoad.GROUP2_NAME);
    	assertEquals((String)groupIdTitleMap.get(section3Uid), AssignmentTestDataLoad.GROUP3_NAME);
    	
    	// try the ta
    	// TODO grader perms
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	groupIdTitleMap = gradebookLogic.getViewableGroupIdToTitleMap(AssignmentTestDataLoad.CONTEXT_ID);
    	assertEquals(1, groupIdTitleMap.size());
    	assertEquals((String)groupIdTitleMap.get(section1Uid), AssignmentTestDataLoad.GROUP1_NAME);
    	
    	// try the student - should not get any sections returned b/c can't grade
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	groupIdTitleMap = gradebookLogic.getViewableGroupIdToTitleMap(AssignmentTestDataLoad.CONTEXT_ID);
    	assertEquals(0, groupIdTitleMap.size());
    	
    }
    
    public void testGetViewableStudentsForGradedItemMap() {
    	// try null parameters
    	try {
    		gradebookLogic.getViewableStudentsForGradedItemMap(null, gbItem1Id);
    		fail("Did not catch null contextId passed to getViewableStudentsForGradedItemMap");
    	} catch (IllegalArgumentException iae) {}
    	
    	try {
    		gradebookLogic.getViewableStudentsForGradedItemMap(AssignmentTestDataLoad.CONTEXT_ID, null);
    		fail("Did not catch null gbItemId passed to getViewableStudentsForGradedItemMap");
    	} catch (IllegalArgumentException iae) {}
    	
    	// what if we pass a bad contextId? should throw SecurityException
    	try {
    		gradebookLogic.getViewableStudentsForGradedItemMap(AssignmentTestDataLoad.BAD_CONTEXT, gbItem1Id);
    		fail("Did not catch SecurityException thrown b/c user does not have permissions in bad context");
    	} catch (SecurityException se) {}
    	
    	// become instructor
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	
    	// what if gb item doesn't exist?
    	Map studentFunctionMap = gradebookLogic.getViewableStudentsForGradedItemMap(AssignmentTestDataLoad.CONTEXT_ID, new Long(12345));
    	assertTrue(studentFunctionMap.isEmpty());
    	
    	// instructor should be able to view all students
    	studentFunctionMap = gradebookLogic.getViewableStudentsForGradedItemMap(AssignmentTestDataLoad.CONTEXT_ID, gbItem1Id);
    	assertEquals(3, studentFunctionMap.size());
    	assertEquals(AssignmentConstants.GRADE, studentFunctionMap.get(AssignmentTestDataLoad.STUDENT1_UID));
    	assertEquals(AssignmentConstants.GRADE, studentFunctionMap.get(AssignmentTestDataLoad.STUDENT2_UID));
    	assertEquals(AssignmentConstants.GRADE, studentFunctionMap.get(AssignmentTestDataLoad.STUDENT3_UID));
    	
    	// switch to TA
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	// should only get students in his/her section
    	// TODO grader perms
    	studentFunctionMap = gradebookLogic.getViewableStudentsForGradedItemMap(AssignmentTestDataLoad.CONTEXT_ID, gbItem1Id);
    	assertEquals(1, studentFunctionMap.size());
    	assertEquals(AssignmentConstants.GRADE, studentFunctionMap.get(AssignmentTestDataLoad.STUDENT1_UID));
    	
    	// try a student - should get a SecurityException
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	try {
    		studentFunctionMap = gradebookLogic.getViewableStudentsForGradedItemMap(AssignmentTestDataLoad.CONTEXT_ID, gbItem1Id);
    		fail("did not catch student trying to access student information");
    	} catch (SecurityException se) {}
    }
    
    public void testIsCurrentUserAbleToEdit() {
    	// this should only be true for the instructor
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	assertTrue(gradebookLogic.isCurrentUserAbleToEdit(AssignmentTestDataLoad.CONTEXT_ID));
    
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	assertFalse(gradebookLogic.isCurrentUserAbleToEdit(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	assertFalse(gradebookLogic.isCurrentUserAbleToEdit(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT2_UID);
    	assertFalse(gradebookLogic.isCurrentUserAbleToEdit(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT3_UID);
    	assertFalse(gradebookLogic.isCurrentUserAbleToEdit(AssignmentTestDataLoad.CONTEXT_ID));
    }
    
    public void testIsCurrentUserAbleToGradeAll() {
    	// this should only be true for the instructor
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	assertTrue(gradebookLogic.isCurrentUserAbleToGradeAll(AssignmentTestDataLoad.CONTEXT_ID));
    
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	assertFalse(gradebookLogic.isCurrentUserAbleToGradeAll(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	assertFalse(gradebookLogic.isCurrentUserAbleToGradeAll(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT2_UID);
    	assertFalse(gradebookLogic.isCurrentUserAbleToGradeAll(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT3_UID);
    	assertFalse(gradebookLogic.isCurrentUserAbleToGradeAll(AssignmentTestDataLoad.CONTEXT_ID));
    }
    
    public void testIsCurrentUserAbleToGrade() {
    	// this should be true for the instructor and ta
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	assertTrue(gradebookLogic.isCurrentUserAbleToGrade(AssignmentTestDataLoad.CONTEXT_ID));
    
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	assertTrue(gradebookLogic.isCurrentUserAbleToGrade(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	assertFalse(gradebookLogic.isCurrentUserAbleToGrade(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT2_UID);
    	assertFalse(gradebookLogic.isCurrentUserAbleToGrade(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT3_UID);
    	assertFalse(gradebookLogic.isCurrentUserAbleToGrade(AssignmentTestDataLoad.CONTEXT_ID));
    }
    
    public void testIsCurrentUserAbleToViewOwnGrades() {
    	// this is true for students
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	assertFalse(gradebookLogic.isCurrentUserAbleToViewOwnGrades(AssignmentTestDataLoad.CONTEXT_ID));
    
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	assertFalse(gradebookLogic.isCurrentUserAbleToViewOwnGrades(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	assertTrue(gradebookLogic.isCurrentUserAbleToViewOwnGrades(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT2_UID);
    	assertTrue(gradebookLogic.isCurrentUserAbleToViewOwnGrades(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT3_UID);
    	assertTrue(gradebookLogic.isCurrentUserAbleToViewOwnGrades(AssignmentTestDataLoad.CONTEXT_ID));
    }
    
    public void testIsCurrentUserAStudentInGb() {
    	// this is true for students
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	assertFalse(gradebookLogic.isCurrentUserAStudentInGb(AssignmentTestDataLoad.CONTEXT_ID));
    
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	assertFalse(gradebookLogic.isCurrentUserAStudentInGb(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	assertTrue(gradebookLogic.isCurrentUserAStudentInGb(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT2_UID);
    	assertTrue(gradebookLogic.isCurrentUserAStudentInGb(AssignmentTestDataLoad.CONTEXT_ID));
    	
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT3_UID);
    	assertTrue(gradebookLogic.isCurrentUserAStudentInGb(AssignmentTestDataLoad.CONTEXT_ID));
    }
    
    public void testIsCurrentUserAbleToGradeStudentForItem() {
    	// try some null data
    	try {
    		gradebookLogic.isCurrentUserAbleToGradeStudentForItem(null, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    		fail("did not catch null contextId passed to isCurrentUserAbleToGradeStudentForItem");
    	} catch (IllegalArgumentException iae) {}
    	
    	try {
    		gradebookLogic.isCurrentUserAbleToGradeStudentForItem(AssignmentTestDataLoad.CONTEXT_ID, null, gbItem1Id);
    		fail("did not catch null studentId passed to isCurrentUserAbleToGradeStudentForItem");
    	} catch (IllegalArgumentException iae) {}
    	
    	try {
    		gradebookLogic.isCurrentUserAbleToGradeStudentForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, null);
    		fail("did not catch null gbItemId passed to isCurrentUserAbleToGradeStudentForItem");
    	} catch (IllegalArgumentException iae) {}
    	
    	// start as instructor
    	// should be true for all students
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	assertTrue(gradebookLogic.isCurrentUserAbleToGradeStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id));
    	assertTrue(gradebookLogic.isCurrentUserAbleToGradeStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT2_UID, gbItem1Id));
    	assertTrue(gradebookLogic.isCurrentUserAbleToGradeStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT3_UID, gbItem1Id));
    	
    	// ta should only be able to grade student 1
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	assertTrue(gradebookLogic.isCurrentUserAbleToGradeStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id));
    	assertFalse(gradebookLogic.isCurrentUserAbleToGradeStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT2_UID, gbItem1Id));
    	assertFalse(gradebookLogic.isCurrentUserAbleToGradeStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT3_UID, gbItem1Id));
    	
    	// student should all be false
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	assertFalse(gradebookLogic.isCurrentUserAbleToGradeStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id));
    	assertFalse(gradebookLogic.isCurrentUserAbleToGradeStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT2_UID, gbItem1Id));
    	assertFalse(gradebookLogic.isCurrentUserAbleToGradeStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT3_UID, gbItem1Id));
    }
    
    public void testGetGradeViewPermissionForCurrentUserForStudentForItem() {
    	// try some null parameters
    	try {
    		gradebookLogic.getGradeViewPermissionForCurrentUserForStudentForItem(null, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    		fail("did not catch null contextId passed to getGradeViewPermissionForCurrentUserForStudentForItem");
    	} catch (IllegalArgumentException iae) {}
    	
    	try {
    		gradebookLogic.getGradeViewPermissionForCurrentUserForStudentForItem(AssignmentTestDataLoad.CONTEXT_ID, null, gbItem1Id);
    		fail("did not catch null studentId passed to getGradeViewPermissionForCurrentUserForStudentForItem");
    	} catch (IllegalArgumentException iae) {}
    	
    	try {
    		gradebookLogic.getGradeViewPermissionForCurrentUserForStudentForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, null);
    		fail("did not catch null itemId passed to getGradeViewPermissionForCurrentUserForStudentForItem");
    	} catch (IllegalArgumentException iae) {}
    	
    	// instructor should be able to grade all
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	assertEquals(AssignmentConstants.GRADE, gradebookLogic.getGradeViewPermissionForCurrentUserForStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id));
    	assertEquals(AssignmentConstants.GRADE, gradebookLogic.getGradeViewPermissionForCurrentUserForStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT2_UID, gbItem1Id));
    	assertEquals(AssignmentConstants.GRADE, gradebookLogic.getGradeViewPermissionForCurrentUserForStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT3_UID, gbItem1Id));
    	
    	// ta should only be able to grade student 1
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	assertEquals(AssignmentConstants.GRADE, gradebookLogic.getGradeViewPermissionForCurrentUserForStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id));
    	assertNull(gradebookLogic.getGradeViewPermissionForCurrentUserForStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT2_UID, gbItem1Id));
    	assertNull(gradebookLogic.getGradeViewPermissionForCurrentUserForStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT3_UID, gbItem1Id));
    	
    	// student should all be null
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	assertNull(gradebookLogic.getGradeViewPermissionForCurrentUserForStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id));
    	assertNull(gradebookLogic.getGradeViewPermissionForCurrentUserForStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT2_UID, gbItem1Id));
    	assertNull(gradebookLogic.getGradeViewPermissionForCurrentUserForStudentForItem(
    			AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT3_UID, gbItem1Id));
    }
}
