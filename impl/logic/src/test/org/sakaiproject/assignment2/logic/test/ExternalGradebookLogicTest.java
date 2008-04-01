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
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.test.AssignmentTestDataLoad;
import org.sakaiproject.assignment2.exception.GradebookItemNotFoundException;
import org.sakaiproject.assignment2.exception.InvalidGradeForAssignmentException;
import org.sakaiproject.assignment2.exception.NoGradebookDataExistsException;
import org.sakaiproject.assignment2.logic.GradebookItem;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
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
        List<String> sectionCategories = sectionAwareness.getSectionCategories(AssignmentTestDataLoad.CONTEXT_ID);
        CourseSection section1 = integrationSupport.createSection(site.getUuid(), AssignmentTestDataLoad.GROUP1_NAME,
				(String)sectionCategories.get(0),
				Integer.valueOf(40), null, null, null, true, false, true,  false, false, false, false);
		section1Uid = section1.getUuid();

		CourseSection section2 = integrationSupport.createSection(site.getUuid(), AssignmentTestDataLoad.GROUP2_NAME,
				(String)sectionCategories.get(0),
				40, null, null, null, true, false, true,  false, false, false, false);
		section2Uid = section2.getUuid();
		CourseSection section3 = integrationSupport.createSection(site.getUuid(), AssignmentTestDataLoad.GROUP3_NAME,
				(String)sectionCategories.get(0),
				40, null, null, null, true, false, true,  false, false, false, false);
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
    		gradebookLogic.getViewableAssignmentsWithGbData(new ArrayList<Assignment2>(), null);
    		fail("Did not catch null contextId passed to getViewableAssignmentsWithGbData");
    	} catch (IllegalArgumentException iae) {}
    	
    	// make sure an empty list is returned if we pass a null list
    	List<Assignment2> viewableAssigns = gradebookLogic.getViewableAssignmentsWithGbData(null, AssignmentTestDataLoad.CONTEXT_ID);
    	assertTrue(viewableAssigns.isEmpty());
    	
    	// now try an empty list
    	viewableAssigns = gradebookLogic.getViewableAssignmentsWithGbData(new ArrayList<Assignment2>(), AssignmentTestDataLoad.CONTEXT_ID);
    	assertTrue(viewableAssigns.isEmpty());
    	
    	// A1 & A2 - ungraded
    	// A3 & A4 - graded
    	
    	List<Assignment2> assignList = new ArrayList<Assignment2>();
    	assignList.add(testData.a1);
    	assignList.add(testData.a2);
    	
    	// what happens if we pass ungraded items?
    	viewableAssigns = gradebookLogic.getViewableAssignmentsWithGbData(assignList, AssignmentTestDataLoad.CONTEXT_ID);
    	assertTrue(viewableAssigns.isEmpty());
    	
    	// let's pass graded items
    	assignList = new ArrayList<Assignment2>();
    	assignList.add(testData.a3);
    	assignList.add(testData.a4);
    	
    	// instructor should get them both back
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	viewableAssigns = gradebookLogic.getViewableAssignmentsWithGbData(assignList, AssignmentTestDataLoad.CONTEXT_ID);
    	assertEquals(2, viewableAssigns.size());
    	
    	// make sure the gb data is populated
    	for (Assignment2 assign : viewableAssigns) {
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
    	for (Assignment2 assign : viewableAssigns) {
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
        Map<Long, String> goIdTitleMap = gradebookLogic.getViewableGradableObjectIdTitleMap(AssignmentTestDataLoad.CONTEXT_ID);
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
    	List<GradebookItem> allItems = gradebookLogic.getAllGradebookItems(AssignmentTestDataLoad.CONTEXT_ID);
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
    	Map<String, String> groupIdTitleMap = gradebookLogic.getViewableGroupIdToTitleMap(AssignmentTestDataLoad.BAD_CONTEXT);
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
    	Map<String, String> studentFunctionMap = gradebookLogic.getViewableStudentsForGradedItemMap(AssignmentTestDataLoad.CONTEXT_ID, 12345L);
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
    
    public void testGetStudentGradeForItem() {
    	// try some null parameters
    	try {
    		gradebookLogic.getStudentGradeForItem(null, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    		fail("did not catch null contextId passed to getStudentGradeForItem");
    	} catch (IllegalArgumentException iae) {}
    	
    	try {
    		gradebookLogic.getStudentGradeForItem(AssignmentTestDataLoad.CONTEXT_ID, null, gbItem1Id);
    		fail("did not catch null studentId passed to getStudentGradeForItem");
    	} catch (IllegalArgumentException iae) {}
    	
    	try {
    		gradebookLogic.getStudentGradeForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, null);
    		fail("did not catch null itemId passed to getStudentGradeForItem");
    	} catch (IllegalArgumentException iae) {}
    	
    	// start out as instructor
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	// try a bad context first
    	String grade = gradebookLogic.getStudentGradeForItem(AssignmentTestDataLoad.BAD_CONTEXT, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    	assertNull(grade);
    	
    	// try a gb item that doesn't exist
    	grade = gradebookLogic.getStudentGradeForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, 12345L);
    	assertNull(grade);
    	
    	// try a real one
    	grade = gradebookLogic.getStudentGradeForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    	assertEquals(st1a3Grade.toString(), grade);
    	
    	// switch to the ta
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	// should only be able to see student1's grade
    	grade = gradebookLogic.getStudentGradeForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    	assertEquals(st1a3Grade.toString(), grade);
    	// shouldn't see st2a4grade
    	try {
    		grade = gradebookLogic.getStudentGradeForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT2_UID, gbItem2Id);
    		fail("did not catch ta trying to get grade for student without auth");
    	} catch (SecurityException se) {}
    	
    	// switch to student
    	// should only be able to retrieve their own
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	grade = gradebookLogic.getStudentGradeForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    	// this gb item isn't released yet, so student should get null grade back
    	assertNull(grade);
    	
    	try {
    		grade = gradebookLogic.getStudentGradeForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT2_UID, gbItem2Id);
    		fail("did not catch student trying to get grade for another student without auth");
    	} catch (SecurityException se) {}
    }
    
    public void testGetStudentGradeCommentForItem() {
    	// try some null parameters
    	try {
    		gradebookLogic.getStudentGradeCommentForItem(null, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    		fail("did not catch null contextId passed to getStudentGradeCommentForItem");
    	} catch (IllegalArgumentException iae) {}
    	
    	try {
    		gradebookLogic.getStudentGradeCommentForItem(AssignmentTestDataLoad.CONTEXT_ID, null, gbItem1Id);
    		fail("did not catch null studentId passed to getStudentGradeCommentForItem");
    	} catch (IllegalArgumentException iae) {}
    	
    	try {
    		gradebookLogic.getStudentGradeCommentForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, null);
    		fail("did not catch null itemId passed to getStudentGradeCommentForItem");
    	} catch (IllegalArgumentException iae) {}
    	
    	// start out as instructor
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	// try a bad context first
    	String comment = gradebookLogic.getStudentGradeCommentForItem(AssignmentTestDataLoad.BAD_CONTEXT, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    	assertNull(comment);
    	
    	// try a gb item that doesn't exist
    	comment = gradebookLogic.getStudentGradeCommentForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, 12345L);
    	assertNull(comment);
    	
    	// try a real one
    	comment = gradebookLogic.getStudentGradeCommentForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    	assertEquals(st1a3Comment, comment);
    	
    	// switch to the ta
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	// the gb does not have its own authz checks for retrieving comments, so should be able to retrieve any student
    	comment = gradebookLogic.getStudentGradeCommentForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    	assertEquals(st1a3Comment, comment);

    	comment = gradebookLogic.getStudentGradeCommentForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT2_UID, gbItem2Id);
    	assertEquals(st2a4Comment, comment);
    	
    	// switch to student
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	// the gb does not have its own authz checks for retrieving comments, so should be able to retrieve any student
    	comment = gradebookLogic.getStudentGradeCommentForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    	assertEquals(st1a3Comment, comment);

    	comment = gradebookLogic.getStudentGradeCommentForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT2_UID, gbItem2Id);
    	assertEquals(st2a4Comment, comment);
    }
    
    public void testPopulateGradesForSubmissions() {
    	// try some null parameters
    	try {
    		gradebookLogic.populateGradesForSubmissions(null, new ArrayList<AssignmentSubmission>(), testData.a1);
    		fail("did not catch null contextId passed to populateGradesForSubmissions");
    	} catch (IllegalArgumentException iae) {}
    	
    	try {
    		gradebookLogic.populateGradesForSubmissions(AssignmentTestDataLoad.CONTEXT_ID, new ArrayList<AssignmentSubmission>(), null);
    		fail("did not catch null assignment passed to populateGradesForSubmissions");
    	} catch (IllegalArgumentException iae) {}
    	
    	// try passing a null list
    	// should do nothing
    	gradebookLogic.populateGradesForSubmissions(AssignmentTestDataLoad.CONTEXT_ID, null, testData.a3);
    	
    	List<AssignmentSubmission> subList = new ArrayList<AssignmentSubmission>();
    	subList.add(testData.st1a3Submission);
    	subList.add(testData.st2a3Submission);
    	
    	// switch to instructor
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	gradebookLogic.populateGradesForSubmissions(AssignmentTestDataLoad.CONTEXT_ID, subList, testData.a3);
    	// verify grades were populated
    	AssignmentSubmission sub1 = (AssignmentSubmission)subList.get(0);
    	assertFalse(sub1.isGradebookGradeReleased());
    	assertEquals(st1a3Grade.toString(), sub1.getGradebookGrade());
    	assertEquals(st1a3Comment, sub1.getGradebookComment());
    	
    	AssignmentSubmission sub2 = (AssignmentSubmission)subList.get(1);
    	assertFalse(sub2.isGradebookGradeReleased());
    	assertNull(sub2.getGradebookGrade());
    	assertNull(sub2.getGradebookComment());
    	
    	// switch to ta
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	// should get SecurityException b/c st2 is in list
    	try {
    		gradebookLogic.populateGradesForSubmissions(AssignmentTestDataLoad.CONTEXT_ID, subList, testData.a3);
    		fail("did not catch ta trying to access student grade info w/o authorization");
    	} catch (SecurityException se) {}
    	
    	// let's only include auth students in list
    	subList = new ArrayList<AssignmentSubmission>();
    	subList.add(testData.st1a3Submission);
    	gradebookLogic.populateGradesForSubmissions(AssignmentTestDataLoad.CONTEXT_ID, subList, testData.a3);
    	// verify grades were populated
    	sub1 = (AssignmentSubmission)subList.get(0);
    	assertFalse(sub1.isGradebookGradeReleased());
    	assertEquals(st1a3Grade.toString(), sub1.getGradebookGrade());
    	assertEquals(st1a3Comment, sub1.getGradebookComment());
    	
    	// now try student - shouldn't have auth
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	try {
    		gradebookLogic.populateGradesForSubmissions(AssignmentTestDataLoad.CONTEXT_ID, subList, testData.a3);
    		fail("did not catch student trying to retrieve grade data via populateGradesForSubmissions");
    	} catch (SecurityException se) {}
    }
    
    public void testSaveGradeAndCommentForStudent() {
    	// try some nulls
    	try {
    		gradebookLogic.saveGradeAndCommentForStudent(null, gbItem1Id, AssignmentTestDataLoad.STUDENT1_UID, null, null);
    		fail("did not catch null contextId passed to saveGradeAndCommentForStudent");
    	} catch (IllegalArgumentException iae) {}
    	
    	try {
    		gradebookLogic.saveGradeAndCommentForStudent(AssignmentTestDataLoad.CONTEXT_ID, null, AssignmentTestDataLoad.STUDENT1_UID, null, null);
    		fail("did not catch null gradableObjectId passed to saveGradeAndCommentForStudent");
    	} catch (IllegalArgumentException iae) {}
    	
    	try {
    		gradebookLogic.saveGradeAndCommentForStudent(AssignmentTestDataLoad.CONTEXT_ID, gbItem1Id, null, null, null);
    		fail("did not catch null studentId passed to saveGradeAndCommentForStudent");
    	} catch (IllegalArgumentException iae) {}
    	
    	// try a bad contextId
    	try {
    		gradebookLogic.saveGradeAndCommentForStudent(AssignmentTestDataLoad.BAD_CONTEXT, gbItem1Id, AssignmentTestDataLoad.STUDENT1_UID, null, null);
    		fail("did not catch invalid contextId passed to saveGradeAndCommentForStudent");
    	} catch (NoGradebookDataExistsException ngdee) {}
    	
    	// try a bad gradableObjectId
    	try {
    		gradebookLogic.saveGradeAndCommentForStudent(AssignmentTestDataLoad.CONTEXT_ID, 12345L, AssignmentTestDataLoad.STUDENT1_UID, null, null);
    		fail("did not catch invalid gradableObjectId passed to saveGradeAndCommentForStudent");
    	} catch (GradebookItemNotFoundException gine) {}
    	
    	// start as instructor
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	String grade = "15.0";
    	String comment = "Good work";
    	
    	gradebookLogic.saveGradeAndCommentForStudent(AssignmentTestDataLoad.CONTEXT_ID, gbItem1Id, AssignmentTestDataLoad.STUDENT1_UID, grade, comment);
    	// now retrieve it to make sure it stuck
    	String dbGrade = gradebookLogic.getStudentGradeForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    	assertEquals(grade, dbGrade);
    	String dbComment = gradebookLogic.getStudentGradeCommentForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    	assertEquals(comment, dbComment);
    	
    	// try an invalid grade
    	try {
    		gradebookLogic.saveGradeAndCommentForStudent(AssignmentTestDataLoad.CONTEXT_ID, 
    			gbItem1Id, AssignmentTestDataLoad.STUDENT1_UID, "A+", "BAD COMMENT");
    		fail("Did not catch invalid grade passed to saveGradeAndCommentForStudent");
    	} catch (InvalidGradeForAssignmentException igfae) {}
    	// now retrieve it to make sure it didn't do an update
    	dbGrade = gradebookLogic.getStudentGradeForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    	assertEquals(grade, dbGrade);
    	dbComment = gradebookLogic.getStudentGradeCommentForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    	assertEquals(comment, dbComment);
    	
    	// switch to TA
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	// should be able to update st1
    	grade = "25.87";
    	comment = "Jolly good show!";
    	gradebookLogic.saveGradeAndCommentForStudent(AssignmentTestDataLoad.CONTEXT_ID, gbItem1Id, AssignmentTestDataLoad.STUDENT1_UID, grade, comment);
    	// now retrieve it to make sure it stuck
    	dbGrade = gradebookLogic.getStudentGradeForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    	assertEquals(grade, dbGrade);
    	dbComment = gradebookLogic.getStudentGradeCommentForItem(AssignmentTestDataLoad.CONTEXT_ID, AssignmentTestDataLoad.STUDENT1_UID, gbItem1Id);
    	assertEquals(comment, dbComment);
    	
    	// but should not be able to update st2
    	try {
    		gradebookLogic.saveGradeAndCommentForStudent(AssignmentTestDataLoad.CONTEXT_ID, 
        			gbItem1Id, AssignmentTestDataLoad.STUDENT2_UID, grade, comment);
        		fail("Did not catch ta trying to update grades w/o auth in saveGradeAndCommentForStudent");
    	} catch (SecurityException se) {}
    	
    	// student's should not have auth
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	try {
    		gradebookLogic.saveGradeAndCommentForStudent(AssignmentTestDataLoad.CONTEXT_ID, 
        			gbItem1Id, AssignmentTestDataLoad.STUDENT2_UID, grade, comment);
        		fail("Did not catch student trying to update grades w/o auth in saveGradeAndCommentForStudent");
    	} catch (SecurityException se) {}
    }
    
    public void testIsGradeValid() {
    	//try a null context
    	try {
    		gradebookLogic.isGradeValid(null, "A");
    		fail("did not catch null contextId passed to isGradeValid");
    	} catch (IllegalArgumentException iae) {}
    	
    	// try passing a null grade - should be valid
    	assertTrue(gradebookLogic.isGradeValid(AssignmentTestDataLoad.CONTEXT_ID, null));
    	
    	//if these tests fail, double check the gradebook. we are using assumptions
    	// of valid grades as of 3/08
    	// we are using a default gb, which is points-based
    	assertTrue(gradebookLogic.isGradeValid(AssignmentTestDataLoad.CONTEXT_ID, "98"));
    	assertFalse(gradebookLogic.isGradeValid(AssignmentTestDataLoad.CONTEXT_ID, "A"));
    }
    
    public void testIdentifyStudentsWithInvalidGrades() {
    	// try a null context
    	try {
    		gradebookLogic.identifyStudentsWithInvalidGrades(null, new HashMap<String, String>());
    		fail("did not catch null contextId passed to identifyStudentsWithInvalidGrades");
    	} catch (IllegalArgumentException iae) {}
    	
    	// a null map should return an empty list
    	List<String> studentList = gradebookLogic.identifyStudentsWithInvalidGrades(AssignmentTestDataLoad.CONTEXT_ID, null);
    	assertTrue(studentList.isEmpty());
    	
    	// we are using a points-based gb
    	Map<String, String> studentGradeMap = new HashMap<String, String>();
    	studentGradeMap.put(AssignmentTestDataLoad.STUDENT1_UID, "10");
    	studentGradeMap.put(AssignmentTestDataLoad.STUDENT2_UID, "124.5");
    	
    	studentList = gradebookLogic.identifyStudentsWithInvalidGrades(AssignmentTestDataLoad.CONTEXT_ID, studentGradeMap);
    	assertTrue(studentList.isEmpty()); // should all be valid
    	
    	// try some invalid grades
    	studentGradeMap = new HashMap<String, String>();
    	studentGradeMap.put(AssignmentTestDataLoad.STUDENT1_UID, "-10"); // invalid
    	studentGradeMap.put(AssignmentTestDataLoad.STUDENT2_UID, "124.5"); //valid
    	studentGradeMap.put(AssignmentTestDataLoad.STUDENT3_UID, "A"); // invalid
    	
    	studentList = gradebookLogic.identifyStudentsWithInvalidGrades(AssignmentTestDataLoad.CONTEXT_ID, studentGradeMap);
    	assertEquals(2, studentList.size()); // should have 2 invalid students
    	assertTrue(studentList.contains(AssignmentTestDataLoad.STUDENT1_UID));
    	assertTrue(studentList.contains(AssignmentTestDataLoad.STUDENT3_UID));
    	
    }
    
    public void testSaveGradesAndCommentsForSubmissions() {
    	// try null info
    	try {
    		gradebookLogic.saveGradesAndCommentsForSubmissions(null, gbItem1Id, new ArrayList<AssignmentSubmission>());
    		fail("did not catch null contextId passed to saveGradesAndCommentsForSubmissions");
    	} catch (IllegalArgumentException iae) {}
    	
    	try {
    		gradebookLogic.saveGradesAndCommentsForSubmissions(AssignmentTestDataLoad.CONTEXT_ID, null, new ArrayList<AssignmentSubmission>());
    		fail("did not catch null gradableObjectId passed to saveGradesAndCommentsForSubmissions");
    	} catch (IllegalArgumentException iae) {}
    	
    	// as instructor
    	authn.setAuthnContext(AssignmentTestDataLoad.INSTRUCTOR_UID);
    	
    	// try passing a null list - should do nothing
    	gradebookLogic.saveGradesAndCommentsForSubmissions(AssignmentTestDataLoad.CONTEXT_ID, gbItem1Id, null);
    	
    	// let's refresh the submission objects to make sure the gb associations are there
    	testData.st1a3Submission = submissionLogic.getAssignmentSubmissionById(testData.st1a3Submission.getId());
    	testData.st2a3Submission = submissionLogic.getAssignmentSubmissionById(testData.st2a3Submission.getId());
    	
    	List<AssignmentSubmission> subList = new ArrayList<AssignmentSubmission>();
    	// try adding submissions for different assignments
    	subList.add(testData.st1a1Submission);
    	subList.add(testData.st1a3Submission);
    	
    	try {
    		gradebookLogic.saveGradesAndCommentsForSubmissions(AssignmentTestDataLoad.CONTEXT_ID, gbItem1Id, subList);
    		fail("did not catch a passed submission list from diff assignments");
    	} catch (IllegalArgumentException iae) {}
    	
    	subList = new ArrayList<AssignmentSubmission>();
    	String st1Grade = "15.5";
    	String st1Comment = "Good work";
    	String st2Grade = "0.0";
    	String st2Comment = "You need to turn this in ASAP";
    	testData.st1a3Submission.setGradebookGrade(st1Grade);
    	testData.st1a3Submission.setGradebookComment(st1Comment);
    	testData.st2a3Submission.setGradebookGrade(st2Grade);
    	testData.st2a3Submission.setGradebookComment(st2Comment);
    	
    	subList.add(testData.st1a3Submission);
    	subList.add(testData.st2a3Submission);
    	
    	// should work this time
    	gradebookLogic.saveGradesAndCommentsForSubmissions(AssignmentTestDataLoad.CONTEXT_ID, gbItem1Id, subList);
    	
    	// now let's retrieve it to see if the save was successful
    	AssignmentSubmission sub1 = submissionLogic.getAssignmentSubmissionById(testData.st1a3Submission.getId());
    	assertEquals(st1Grade, sub1.getGradebookGrade());
    	assertEquals(st1Comment, sub1.getGradebookComment());
    	AssignmentSubmission sub2 = submissionLogic.getAssignmentSubmissionById(testData.st2a3Submission.getId());
    	assertEquals(st2Grade, sub2.getGradebookGrade());
    	assertEquals(st2Comment, sub2.getGradebookComment());
    	
    	// try a bad grade - make sure nothing was updated
    	testData.st1a3Submission.setGradebookGrade("10.5");
    	testData.st1a3Submission.setGradebookComment("lovely");
    	testData.st2a3Submission.setGradebookGrade("A");  // INVALID
    	testData.st2a3Submission.setGradebookComment("jolly good show");
    	subList = new ArrayList<AssignmentSubmission>();
    	subList.add(testData.st1a3Submission);
    	subList.add(testData.st2a3Submission);
    	
    	try {
    		gradebookLogic.saveGradesAndCommentsForSubmissions(AssignmentTestDataLoad.CONTEXT_ID, gbItem1Id, subList);
        	fail("did not catch invalid grade passed to saveGradesAndCommentsForSubmissions");
    	} catch (InvalidGradeForAssignmentException igfae) {}
    	
    	// double check that the grades and comments weren't actually changed
    	sub1 = submissionLogic.getAssignmentSubmissionById(testData.st1a3Submission.getId());
    	assertEquals(st1Grade, sub1.getGradebookGrade());
    	assertEquals(st1Comment, sub1.getGradebookComment());
    	sub2 = submissionLogic.getAssignmentSubmissionById(testData.st2a3Submission.getId());
    	assertEquals(st2Grade, sub2.getGradebookGrade());
    	assertEquals(st2Comment, sub2.getGradebookComment());
    	
    	// check security
    	authn.setAuthnContext(AssignmentTestDataLoad.TA_UID);
    	// the ta should only be able to update st1
    	subList = new ArrayList<AssignmentSubmission>();
    	st1Grade = "25.52";
    	st1Comment = "graded by ta";
    	testData.st1a3Submission.setGradebookGrade(st1Grade);
    	testData.st1a3Submission.setGradebookComment(st1Comment);
    	subList.add(testData.st1a3Submission);
    	
    	gradebookLogic.saveGradesAndCommentsForSubmissions(AssignmentTestDataLoad.CONTEXT_ID, gbItem1Id, subList);
    	
    	// now let's retrieve it to see if the save was successful
    	sub1 = submissionLogic.getAssignmentSubmissionById(testData.st1a3Submission.getId());
    	assertEquals(st1Grade, sub1.getGradebookGrade());
    	assertEquals(st1Comment, sub1.getGradebookComment());
    	
    	// now try to include st2 in the list - nothing should be updated
    	testData.st1a3Submission.setGradebookGrade("10.5");
    	testData.st1a3Submission.setGradebookComment("lovely");
    	testData.st2a3Submission.setGradebookGrade("11.26"); 
    	testData.st2a3Submission.setGradebookComment("jolly good show");
    	subList = new ArrayList<AssignmentSubmission>();
    	subList.add(testData.st1a3Submission);
    	subList.add(testData.st2a3Submission);
    	
    	try {
    		gradebookLogic.saveGradesAndCommentsForSubmissions(AssignmentTestDataLoad.CONTEXT_ID, gbItem1Id, subList);
    		fail("did not catch ta trying to grade st2 without auth!");
    	} catch (SecurityException se) {}
    	
    	// make sure students hit security, as well
    	authn.setAuthnContext(AssignmentTestDataLoad.STUDENT1_UID);
    	try {
    		gradebookLogic.saveGradesAndCommentsForSubmissions(AssignmentTestDataLoad.CONTEXT_ID, gbItem1Id, subList);
    		fail("did not catch student trying to save grade info without auth!");
    	} catch (SecurityException se) {}
    }
}
