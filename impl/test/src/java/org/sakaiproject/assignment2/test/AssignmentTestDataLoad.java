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
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
import org.sakaiproject.assignment2.model.FeedbackAttachment;
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
	public static final String ASSIGN3_TITLE = "Assignment 3";
	public static final String ASSIGN4_TITLE = "Assignment 4";
	
	public static final String INSTRUCTOR_UID = "instructorUid";
	public static final String TA_UID = "taUid";
	public static final String STUDENT1_UID = "student1";
	public static final String STUDENT2_UID = "student2";
	public static final String STUDENT3_UID = "student3";
	
	public static final String GROUP1_NAME = "Group 1";
	public static final String GROUP2_NAME = "Group 2";
	public static final String GROUP3_NAME = "Group 3";

	public Assignment2 a1;
	public Assignment2 a2;
	public Assignment2 a3;
	public Assignment2 a4;

	public Long a1Id;
	public Long a2Id;
	public Long a3Id;
	public Long a4Id;
	
	// Assignment 1 has 2 attachments and 2 groups
	// Assignment 2 has 0 attachments and 0 group
	// Assignment 3 has 1 attachment and 0 groups
	// Assignment 4 has 0 attachments and 1 group
	
	public AssignmentSubmission st1a1Submission;
	public AssignmentSubmission st2a1Submission;
	
	public AssignmentSubmissionVersion st1a1CurrVersion; // no attach
	public AssignmentSubmissionVersion st2a1Version1; // 1 Sub Attach
	public AssignmentSubmissionVersion st2a1Version2; // no attach
	public AssignmentSubmissionVersion st2a1CurrVersion; // 2 FA, 2 SA
	
	public AssignmentSubmission st2a2SubmissionNoVersions;

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
		a4 = createGenericAssignment2Object(ASSIGN4_TITLE, 3);
		
		a1Id = a1.getId();
		a2Id = a2.getId();
		a3Id = a3.getId();
		a4Id = a4.getId();
		
		Set<AssignmentAttachment> attachFora1 = new HashSet();
		Set<AssignmentAttachment> attachFora2 = new HashSet();
		Set<AssignmentAttachment> attachFora3 = new HashSet();
		Set<AssignmentAttachment> attachFora4 = new HashSet();
		
		Set<AssignmentGroup> groupsFora1 = new HashSet();
		Set<AssignmentGroup> groupsFora2 = new HashSet();
		Set<AssignmentGroup> groupsFora3 = new HashSet();
		Set<AssignmentGroup> groupsFora4 = new HashSet();
		
		// add attachments
		// to Assignment 1
		attachFora1.add(new AssignmentAttachment(a1, "ref1"));
		attachFora1.add(new AssignmentAttachment(a1, "ref2"));
	
		// to Assignment 3
		attachFora3.add(new AssignmentAttachment(a3, "ref1"));

		// add AssignmentGroups
		groupsFora1.add(new AssignmentGroup(a1, GROUP1_NAME));
		groupsFora1.add(new AssignmentGroup(a1, GROUP2_NAME));

		groupsFora4.add(new AssignmentGroup(a4, GROUP3_NAME));
		
		Set assignSet = new HashSet();
		assignSet.add(a1);
		assignSet.add(a2);
		assignSet.add(a3);
		assignSet.add(a4);

		dao.saveMixedSet(new Set[] {assignSet, attachFora1, groupsFora1, attachFora2, groupsFora2, attachFora3, groupsFora3,
				attachFora4, groupsFora4});
		
		a1Id = a1.getId();
		a2Id = a2.getId();
		a3Id = a3.getId();
		a4Id = a4.getId();

		// now create submissions
		// start with a1
		st1a1Submission = new AssignmentSubmission(a1, STUDENT1_UID, Boolean.TRUE);
		st1a1CurrVersion = createGenericVersion(st1a1Submission);
		dao.save(st1a1Submission);
		dao.save(st1a1CurrVersion);
		
		st2a1Submission = new AssignmentSubmission(a1, STUDENT2_UID, Boolean.TRUE);
		st2a1Version1 = createGenericVersion(st2a1Submission);
		dao.save(st2a1Submission);
		dao.save(st2a1Version1);
		SubmissionAttachment st2a1v2Attach =
			new SubmissionAttachment(st2a1Version1, "attachmentRef");
		dao.save(st2a1v2Attach);
		st2a1Version2 = createGenericVersion(st2a1Submission);
		dao.save(st2a1Version2);
		st2a1CurrVersion = createGenericVersion(st2a1Submission);
		dao.save(st2a1CurrVersion);
		SubmissionAttachment st2a1v3attach = 
			new SubmissionAttachment(st2a1CurrVersion, "attachmentRef");
		dao.save(st2a1v3attach);
		SubmissionAttachment st2a1v3attach2 = 
			new SubmissionAttachment(st2a1CurrVersion, "attachmentRef");
		dao.save(st2a1v3attach2);
		FeedbackAttachment st2a1v3Fattach = 
			new FeedbackAttachment(st2a1CurrVersion, "attachmentRef");
		dao.save(st2a1v3Fattach);
		FeedbackAttachment st2a1v3Fattach2 = 
			new FeedbackAttachment(st2a1CurrVersion, "attachmentRef");
		dao.save(st2a1v3Fattach2);
		
		// create a submission w/o any versions
		st2a2SubmissionNoVersions = new AssignmentSubmission(a2, STUDENT2_UID, Boolean.FALSE);
		dao.save(st2a2SubmissionNoVersions);
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
		assignment.setUngraded(Boolean.TRUE);
		assignment.setHonorPledge(Boolean.FALSE);
		assignment.setHasAnnouncement(Boolean.FALSE);
		assignment.setSortIndex(sortIndex);
		assignment.setTitle(title);

		return assignment;
	}
	
	private AssignmentSubmissionVersion createGenericVersion(AssignmentSubmission submission) {
		AssignmentSubmissionVersion version = new AssignmentSubmissionVersion();
		version.setAssignmentSubmission(submission);
		version.setCreatedBy(submission.getUserId());
		version.setCreatedTime(new Date());
		version.setDraft(Boolean.FALSE);
		version.setSubmittedText("submitted text by " + submission.getUserId());
		version.setSubmittedTime(new Date());
		return version;
	}

}
