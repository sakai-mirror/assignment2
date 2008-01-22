/******************************************************************************
 * EvalTestDataLoad.java - created by aaronz@vt.edu
 * 
 * Copyright (c) 2007 Virginia Polytechnic Institute and State University
 * Licensed under the Educational Community License version 1.0
 * 
 * A copy of the Educational Community License has been included in this 
 * distribution and is available at: http://www.opensource.org/licenses/ecl1.php
 * 
 * Contributors:
 * Aaron Zeckoski (aaronz@vt.edu) - primary
 * 
 *****************************************************************************/

package org.sakaiproject.assignment2.test;

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sakaiproject.assignment2.dao.AssignmentDao;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;


/**
 * This class holds a bunch of items used to prepopulate the database and then
 * do testing, it also handles initilization of the objects and saving
 * (Note for developers - do not modify this without permission from the author)
 * 
 * @author Aaron Zeckoski (aaronz@vt.edu)
 */
public class AssignmentTestDataLoad {

	// constants
	public static final String CONTEXT_ID = "validContext";
	public static final String BAD_CONTEXT = "badContext";
	
	public static final String ASSIGN1_TITLE = "Assignment 1";
	public static final String ASSIGN2_TITLE = "Assignment 2";
	public static final String ASSIGN3_TITLE = "Assignemnt 3";

	public Assignment2 a1;
	public Assignment2 a2;
	public Assignment2 a3;
	public Long a1Id;
	public Long a2Id;
	public Long a3Id;
	
	public AssignmentAttachment attach1Fora1;
	public AssignmentAttachment attach2Fora1;
	public AssignmentAttachment attach1Fora3;
	
	public AssignmentGroup group1Fora1;
	public AssignmentGroup group2Fora1;
	public AssignmentGroup group1Fora2;

	public AssignmentTestDataLoad() {
		initialize();

	}

	/**
	 * initialize all the objects in this data load pea
	 * (this will make sure all the public properties are not null)
	 */
	public void initialize() {
		
	}

	/**
	 * Store all of the persistent objects in this pea
	 * @param dao A DAO with a save method which takes a persistent object as an argument<br/>
	 * Example: dao.save(templateUser);
	 */
	public void createTestData(AssignmentDao dao) {
		a1 = createGenericAssignment2Object(ASSIGN1_TITLE, 0);
		a2 = createGenericAssignment2Object(ASSIGN2_TITLE, 1);
		a3 = createGenericAssignment2Object(ASSIGN3_TITLE, 2);

		dao.create(a1);
		dao.create(a2);
		dao.create(a3);
		
		a1Id = a1.getId();
		a2Id = a2.getId();
		a3Id = a3.getId();
		
		// add attachments
		// to Assignment 1
		attach1Fora1 = new AssignmentAttachment();
		attach1Fora1.setAssignment(a1);
		attach1Fora1.setAttachmentReference("reference1");
		dao.save(attach1Fora1);

		attach2Fora1 = new AssignmentAttachment();
		attach2Fora1.setAssignment(a1);
		attach2Fora1.setAttachmentReference("reference2");
		dao.save(attach2Fora1);
		// to Assignment 3
		attach1Fora3 = new AssignmentAttachment();
		attach1Fora3.setAssignment(a3);
		attach1Fora3.setAttachmentReference("reference1");
		dao.save(attach1Fora3);

		// add AssignmentGroups
		group1Fora1 = new AssignmentGroup();
		group1Fora1.setAssignment(a1);
		group1Fora1.setGroupId("Group1");
		dao.save(group1Fora1);

		group2Fora1 = new AssignmentGroup();
		group2Fora1.setAssignment(a1);
		group2Fora1.setGroupId("Group2");
		dao.save(group2Fora1);
		
		group1Fora2 = new AssignmentGroup();
		group1Fora2.setAssignment(a2);
		group1Fora2.setGroupId("Group3");
		dao.save(group1Fora2);

	}


	private Assignment2 createGenericAssignment2Object(String title, int sortIndex) {
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
		assignment.setSortIndex(sortIndex);
		assignment.setTitle(title);

		return assignment;
	}

}
