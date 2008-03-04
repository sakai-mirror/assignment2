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

package org.sakaiproject.assignment2.logic.impl;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment.api.Assignment;
import org.sakaiproject.assignment.api.AssignmentSubmissionEdit;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
import org.sakaiproject.assignment2.model.SubmissionAttachmentBase;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.UploadAllOptions;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
//import org.sakaiproject.assignment2.tool.beans.NotificationBean;
import org.sakaiproject.assignment2.exception.StaleObjectModificationException;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.utils.ComparatorsUtils;
import org.sakaiproject.assignment2.dao.AssignmentDao;
import org.sakaiproject.component.cover.ServerConfigurationService;
import org.sakaiproject.entity.api.Reference;
import org.sakaiproject.event.api.SessionState;
import org.sakaiproject.time.cover.TimeService;
import org.sakaiproject.tool.cover.ToolManager;
import org.sakaiproject.exception.IdUnusedException;
import org.sakaiproject.exception.PermissionException;
import org.sakaiproject.exception.TypeException;
import org.sakaiproject.user.api.User;
import org.sakaiproject.user.api.UserDirectoryService;
import org.sakaiproject.user.api.UserNotDefinedException;
import org.sakaiproject.util.StringUtil;
import org.springframework.orm.hibernate3.HibernateOptimisticLockingFailureException;
import org.springframework.web.multipart.MultipartFile;
import org.hibernate.StaleObjectStateException;

/**
 * This is the interface for interaction with the AssignmentSubmission object
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class AssignmentSubmissionLogicImpl implements AssignmentSubmissionLogic{
	
	private static Log log = LogFactory.getLog(AssignmentSubmissionLogicImpl.class);
	
	private ExternalLogic externalLogic;
	public void setExternalLogic(ExternalLogic externalLogic) {
		this.externalLogic = externalLogic;
	}

	private AssignmentDao dao;
	public void setDao(AssignmentDao dao) {
		this.dao = dao;
	}

	private ExternalGradebookLogic gradebookLogic;
	public void setExternalGradebookLogic(ExternalGradebookLogic gradebookLogic) {
		this.gradebookLogic = gradebookLogic;
	}

	private AssignmentPermissionLogic permissionLogic;
	public void setPermissionLogic(AssignmentPermissionLogic permissionLogic) {
		this.permissionLogic = permissionLogic;
	}

	private UserDirectoryService userDirectoryService;
	public void setUserDirectoryService(UserDirectoryService userDirectoryService) {
		this.userDirectoryService = userDirectoryService;
	}

	private AssignmentLogic assignmentLogic;
	public void setAssignmentLogic(AssignmentLogic assignmentLogic)
	{
		this.assignmentLogic = assignmentLogic;
	}

//    private NotificationBean notificationBean;
//    public void setNotificationBean(NotificationBean notificationBean) {
//    	this.notificationBean = notificationBean;
//    }
    
	public void init(){
		if (log.isDebugEnabled()) log.debug("init");
	}
	
	public AssignmentSubmission getAssignmentSubmissionById(Long submissionId){
		if (submissionId == null) {
			throw new IllegalArgumentException("Null submissionId passed to getAssignmentSubmissionById");
		}

		AssignmentSubmission submission =  (AssignmentSubmission) dao.findById(AssignmentSubmission.class, submissionId);
		String currentUserId = externalLogic.getCurrentUserId();
		String currentContextId = externalLogic.getCurrentContextId();

		// if the submission rec exists, we need to grab the most current version
		if (submission != null) {
			
			Assignment2 assignment = submission.getAssignment();
			
			if (!permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(submission.getUserId(), assignment)) {
				throw new SecurityException("user" + externalLogic.getCurrentUserId() + " attempted to view submission with id " + submissionId + " but is not authorized");
			}
			
			// since we may make modifications to this object for 
			// diff users that we will not save, don't use the
			// persistent object
			AssignmentSubmissionVersion persistedCurrentVersion = dao.getCurrentSubmissionVersionWithAttachments(submission);
			AssignmentSubmissionVersion currentVersion = null;
			
			if (persistedCurrentVersion != null) {
				currentVersion = AssignmentSubmissionVersion.deepCopy(persistedCurrentVersion, true, true);
			}
			
			if (currentVersion != null) {
				filterOutRestrictedVersionInfo(currentVersion, currentUserId);
			}
			
			submission.setCurrentSubmissionVersion(currentVersion);
			
			if (!assignment.isUngraded() && assignment.getGradableObjectId() != null) {
				// retrieve the grade information for this submission
				gradebookLogic.populateAllGradeInfoForSubmission(currentContextId, 
						currentUserId, submission);
				
				gradebookLogic.populateGradebookItemDetailsForAssignment(currentContextId, assignment);
			}
	
			Date dueDate = assignment.isUngraded() ? assignment.getDueDateForUngraded() : assignment.getDueDate();
					
			// populate this submission's status
			Integer status = getSubmissionStatusConstantForCurrentVersion(submission.getCurrentSubmissionVersion(), dueDate);
			submission.setSubmissionStatusConstant(status);
		}
		return submission;

	}
	
	public AssignmentSubmissionVersion getSubmissionVersionById(Long submissionVersionId) {
		if (submissionVersionId == null) {
			throw new IllegalArgumentException("null submissionVersionId passed to getSubmissionVersionById");
		}
		
		AssignmentSubmissionVersion persistedVersion = dao.getAssignmentSubmissionVersionByIdWithAttachments(submissionVersionId);
		
		// since we may make modifications to this object for 
		// diff users that we will not save, don't use the
		// persistent object
		AssignmentSubmissionVersion version = null;
		if (persistedVersion != null) {
			version = AssignmentSubmissionVersion.deepCopy(persistedVersion, true, true);
		}
		
		String currentUserId = externalLogic.getCurrentUserId();
		
		if (version != null) {		
			AssignmentSubmission submission = version.getAssignmentSubmission();
			Assignment2 assignment = submission.getAssignment();
			
			// ensure that the current user is authorized to view this user for this assignment
			if (!permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(submission.getUserId(), assignment)) {
				throw new SecurityException("User " + currentUserId + " attempted to access the version " + 
						submissionVersionId + " for student " + submission.getUserId() + " without authorization");
			}
			
			filterOutRestrictedVersionInfo(version, currentUserId);
			
			// populate gradebook information, if appropriate
			if (!assignment.isUngraded() && assignment.getGradableObjectId() != null) {
				gradebookLogic.populateAllGradeInfoForSubmission(externalLogic.getCurrentContextId(), 
						currentUserId, submission);
			}
		}
		
		return version;
	}
	
	public AssignmentSubmission getCurrentSubmissionByAssignmentIdAndStudentId(Long assignmentId, String studentId) {
		if (assignmentId == null || studentId == null) {
			throw new IllegalArgumentException("Null assignmentId or userId passed to getCurrentSubmissionByAssignmentAndUser");
		}
		
		String contextId = externalLogic.getCurrentContextId();
		String currentUserId = externalLogic.getCurrentUserId();
		
		AssignmentSubmission submission = null;
		
		Assignment2 assignment = dao.getAssignmentByIdWithGroups(assignmentId);
		if (assignment != null) {
			if (!permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(studentId, assignment)) {
				throw new SecurityException("Current user " + currentUserId + " is not allowed to view submission for " + studentId + " for assignment " + assignment.getId());
			}
			
			// since we may make modifications to this object depending on 
			// the user that we will not save, don't use the
			// persistent object - make a copy
			AssignmentSubmission persistedSubmission = dao.getSubmissionWithVersionHistoryForStudentAndAssignment(studentId, assignment);
			if (persistedSubmission != null) {
				submission = AssignmentSubmission.deepCopy(persistedSubmission, true, true, true);
			}

			if (submission == null) {
				// return an "empty" submission
				submission = new AssignmentSubmission(assignment, studentId);
			} else {
				filterOutRestrictedInfo(submission, currentUserId, true);
			} 
			
			// retrieve the grade information for this submission
			if (!assignment.isUngraded() && assignment.getGradableObjectId() != null) {
				gradebookLogic.populateAllGradeInfoForSubmission(contextId, 
						currentUserId, submission);
				
				gradebookLogic.populateGradebookItemDetailsForAssignment(contextId, assignment);
			}
			
			Date dueDate = assignment.isUngraded() ? assignment.getDueDateForUngraded() : assignment.getDueDate();
			
			// populate this submission's status
			Integer status = getSubmissionStatusConstantForCurrentVersion(
					submission.getCurrentSubmissionVersion(), dueDate);
			submission.setSubmissionStatusConstant(status);

		}
		
		return submission;
	}
	
	public void saveStudentSubmission(String userId, Assignment2 assignment, Boolean draft, 
			String submittedText, Set<SubmissionAttachment> subAttachSet) {
		if (userId == null || assignment == null || draft == null) {
			throw new IllegalArgumentException("null userId, assignment, or draft passed to saveAssignmentSubmission");
		}
		
		if (assignment.getId() == null) {
			throw new IllegalArgumentException("assignment without an id passed to saveStudentSubmission");
		}
		
		Date currentTime = new Date();
		String currentUserId = externalLogic.getCurrentUserId();
		String contextId = externalLogic.getCurrentContextId();
		
		if (!currentUserId.equals(userId)) {
			throw new SecurityException("User " + currentUserId + " attempted to save a submission for " +
					userId + ". You may only make a submission for yourself!");
		}
		
		if (!permissionLogic.isUserAbleToMakeSubmissionForAssignment(contextId, assignment)) {
			log.warn("User " + currentUserId + " attempted to make a submission " +
					"without authorization for assignment " + assignment.getId());
			throw new SecurityException("User " + currentUserId + " attempted to make a submission " +
					"without authorization for assignment " + assignment.getId());
		}
		
		if (!submissionIsOpenForStudentForAssignment(currentUserId, assignment.getId())) {
			log.warn("User " + currentUserId + " attempted to make a submission " +
					"but submission for this user for assignment " + assignment.getId() + " is closed.");
			throw new SecurityException("User " + currentUserId + " attempted to make a submission " +
					"for closed assignment " + assignment.getId());
		}
		
		// if there is no current version or the most recent version was submitted, we will need
		// to create a new version. If the current version is draft, we will continue to update
		// this version until it is submitted
		
		// retrieve the latest submission for this user and assignment
		AssignmentSubmission submission = getAssignmentSubmissionForStudentAndAssignment(assignment, userId);
		AssignmentSubmissionVersion version = null;

		boolean isAnUpdate = false;
		
		if (submission == null) {
			// there is no submission for this user yet
			submission = new AssignmentSubmission(assignment, userId);
			
			// we need to create a new version for this submission
			version = new AssignmentSubmissionVersion();
		} else {
			// the submission exists, so we need to check the current version
			version = dao.getCurrentSubmissionVersionWithAttachments(submission);
			
			// if the current version hasn't been submitted yet, we will
			// update that one. otherwise, create a new version
			if (version != null && version.getSubmittedTime() == null) {
				
				isAnUpdate = true;
				
			} else {
				// we need to create a new version for this submission
				version = new AssignmentSubmissionVersion();
			}
		}

		// now let's set the new data
		version.setAssignmentSubmission(submission);
		version.setDraft(draft);
		version.setSubmittedText(submittedText);

		if (isAnUpdate) {
			version.setModifiedBy(currentUserId);
			version.setModifiedTime(currentTime);
		} else {
			version.setCreatedBy(currentUserId);
			version.setCreatedTime(currentTime);
		}

		if (!version.isDraft()) {
			version.setSubmittedTime(currentTime);
			
			// if this isn't a draft, set the annotated text to be the submitted text
			// to allow instructor to provide inline comments for submitted text
			version.setAnnotatedText(submittedText);
		}
		
		// identify any attachments that were deleted or need to be created
		// - we don't update attachments
		Set attachToDelete = identifyAttachmentsToDelete(version.getSubmissionAttachSet(), subAttachSet);
		Set attachToCreate = identifyAttachmentsToCreate(subAttachSet);
		
		// make sure the version was populated on the FeedbackAttachments
		populateVersionForAttachmentSet(attachToCreate, version);
		populateVersionForAttachmentSet(attachToDelete, version);

		try {

			Set<AssignmentSubmissionVersion> versionSet = new HashSet<AssignmentSubmissionVersion>();
			versionSet.add(version);

			Set<AssignmentSubmission> submissionSet = new HashSet<AssignmentSubmission>();
			submissionSet.add(submission);
			
			if (attachToCreate == null) {
				attachToCreate = new HashSet();
			}

			dao.saveMixedSet(new Set[] {submissionSet, versionSet, attachToCreate});
			if (log.isDebugEnabled()) log.debug("Updated/Added student submission version " + 
					version.getId() + " for user " + submission.getUserId() + " for assignment " + 
					submission.getAssignment().getTitle()+ " ID: " + submission.getAssignment().getId());
			if (log.isDebugEnabled()) log.debug("New submission attachments created: " + attachToCreate.size());

			if (attachToDelete != null && !attachToDelete.isEmpty()) {
				dao.deleteSet(attachToDelete);
				if (log.isDebugEnabled()) log.debug("Removed " + attachToDelete.size() + 
						"sub attachments deleted for updated version " + 
						version.getId() + " by user " + currentUserId);
			}
			/*
			notificationBean.notificationToStudent(submission);
			if (assignment.getNotificationType() ==  AssignmentConstants.NOTIFY_FOR_EACH)
			{
				notificationBean.notificationToInstructors(submission, assignment);
			}*/
			
		} catch (HibernateOptimisticLockingFailureException holfe) {
			if(log.isInfoEnabled()) log.info("An optimistic locking failure occurred while attempting to update submission version" + version.getId());
			throw new StaleObjectModificationException(holfe);
		} catch (StaleObjectStateException sose) {
			if(log.isInfoEnabled()) log.info("An optimistic locking failure occurred while attempting to update submission version" + version.getId());
			throw new StaleObjectModificationException(sose);
		}
		/*catch (IdUnusedException e)
		{
			log.error("assignment2", e);
		}
		catch (UserNotDefinedException e)
		{
			log.error("assignment2", e);
		}
		catch (PermissionException e)
		{
			log.error("assignment2", e);
		}
		catch (TypeException e)
		{
			log.error("assignment2", e);
		}*/

	}

	public void saveInstructorFeedback(Long versionId, String studentId, Assignment2 assignment, 
			Integer numSubmissionsAllowed, Date resubmitCloseTime, String annotatedText, 
			String feedbackNotes, Date releasedTime, Set feedbackAttachSet) {
		
		if (studentId == null || assignment == null) {
			throw new IllegalArgumentException("Null studentId or assignment passed" +
					"to saveInstructorFeedback");
		}
		
		Date currentTime = new Date();
		String currentUserId = externalLogic.getCurrentUserId();
		String contextId = externalLogic.getCurrentContextId();
		
		if (!permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(studentId, assignment)) {
			throw new SecurityException("User " + currentUserId + 
					" attempted to submit feedback for student " + studentId + " without authorization");
		}
		
		// let's retrieve the student's submission
		AssignmentSubmission submission = getAssignmentSubmissionForStudentAndAssignment(assignment, studentId);
		
		if (submission != null && versionId == null) {
			throw new IllegalArgumentException("Null versionId passed to saveInstructorFeedback " +
					"even though a submission exists for this student");
		}
		
		if (submission == null) {
			submission = new AssignmentSubmission(assignment, studentId);
		}
		
		AssignmentSubmissionVersion version = null;
		boolean newVersion = false;
		
		// let's try to retrieve the version to update
		if (versionId != null) {
			version = dao.getAssignmentSubmissionVersionByIdWithAttachments(versionId);
			if (version != null) {
				// double check that this version matches the student and assignment
				if (!(version.getAssignmentSubmission().getUserId().equals(studentId) &&
						version.getAssignmentSubmission().getAssignment().getId().equals(assignment.getId()))) {
					throw new IllegalArgumentException("The versionId passed is not" +
							" associated with the given student and assignment");
				}
				
			} else {
				throw new IllegalArgumentException("No version exists with the given versionId: " + versionId);
			}
		} else {
			// if null, we should be creating a new version b/c the instructor
			// wants to provide feedback but the student has not made a submission
			version = new AssignmentSubmissionVersion();
			newVersion = true;
		}
		
		submission.setResubmitCloseTime(resubmitCloseTime);
		submission.setNumSubmissionsAllowed(numSubmissionsAllowed);
		
		version.setAssignmentSubmission(submission);
		version.setAnnotatedText(annotatedText);
		version.setFeedbackNotes(feedbackNotes);
		version.setReleasedTime(releasedTime);
		version.setLastFeedbackSubmittedBy(currentUserId);
		version.setLastFeedbackTime(currentTime);
		
		if (newVersion) {
			version.setCreatedBy(currentUserId);
			version.setCreatedTime(currentTime);
			version.setDraft(false);
		}
		
		// identify any attachments that were deleted
		Set attachToDelete = identifyAttachmentsToDelete(version.getFeedbackAttachSet(), feedbackAttachSet);
		Set attachToCreate = identifyAttachmentsToCreate(feedbackAttachSet);
		
		// make sure the version was populated on the FeedbackAttachments
		populateVersionForAttachmentSet(attachToCreate, version);
		populateVersionForAttachmentSet(attachToDelete, version);

		try {

			Set<AssignmentSubmissionVersion> versionSet = new HashSet<AssignmentSubmissionVersion>();
			versionSet.add(version);

			Set<AssignmentSubmission> submissionSet = new HashSet<AssignmentSubmission>();
			submissionSet.add(submission);
			
			if (attachToCreate == null) {
				attachToCreate = new HashSet();
			}

			dao.saveMixedSet(new Set[] {submissionSet, versionSet, attachToCreate});
			if (log.isDebugEnabled()) log.debug("Updated/Added feedback for version " + 
					version.getId() + " for user " + submission.getUserId() + " for assignment " + 
					submission.getAssignment().getTitle()+ " ID: " + submission.getAssignment().getId());
			if (log.isDebugEnabled()) log.debug("Created feedbackAttachments: " + attachToCreate.size());

			if (attachToDelete != null && !attachToDelete.isEmpty()) {
				dao.deleteSet(attachToDelete);
				if (log.isDebugEnabled()) log.debug("Removed feedback attachments deleted for updated version " + version.getId() + " by user " + currentUserId);
			}

		} catch (HibernateOptimisticLockingFailureException holfe) {
			if(log.isInfoEnabled()) log.info("An optimistic locking failure occurred while attempting to update submission version" + version.getId());
			throw new StaleObjectModificationException(holfe);
		} catch (StaleObjectStateException sose) {
			if(log.isInfoEnabled()) log.info("An optimistic locking failure occurred while attempting to update submission version" + version.getId());
			throw new StaleObjectModificationException(sose);
		}
	}
	
	public List<AssignmentSubmission> getViewableSubmissionsForAssignmentId(Long assignmentId) {
		if (assignmentId == null) {
			throw new IllegalArgumentException("null assignmentId passed to getViewableSubmissionsForAssignmentId");
		}
		
		String contextId = externalLogic.getCurrentContextId();
		
		List<AssignmentSubmission> viewableSubmissions = new ArrayList<AssignmentSubmission>();
		
		Assignment2 assignment = (Assignment2)dao.findById(Assignment2.class, assignmentId);

		if (assignment != null) {
			// get a list of all the students that the current user may view for the given assignment
			List<String> viewableStudents = permissionLogic.getViewableStudentsForUserForItem(assignment);

			if (viewableStudents != null && !viewableStudents.isEmpty()) {
				// populate the gb info for this assignment, if appropriate. this is used for due date to 
				// determine if submission is late
				if (!assignment.isUngraded() && assignment.getGradableObjectId() != null) {
					gradebookLogic.populateGradebookItemDetailsForAssignment(contextId, assignment);
				}
				
				Date dueDate = assignment.isUngraded() ? assignment.getDueDateForUngraded() : assignment.getDueDate();
				
				// get the submissions for these students
				Set<AssignmentSubmission> existingSubmissions = dao.getCurrentSubmissionsForStudentsForAssignment(viewableStudents, assignment);

				Map studentIdSubmissionMap = new HashMap();
				if (existingSubmissions != null) {
					for (Iterator subIter = existingSubmissions.iterator(); subIter.hasNext();) {
						AssignmentSubmission submission = (AssignmentSubmission) subIter.next();
						if (submission != null) {
							studentIdSubmissionMap.put(submission.getUserId(), submission);
						}
					}
				}

				// now, iterate through the students and create empty AssignmentSubmission recs
				// if no submission exists yet
				for (Iterator studentIter = viewableStudents.iterator(); studentIter.hasNext();) {
					String studentId = (String) studentIter.next();
					if (studentId != null) {
						AssignmentSubmission thisSubmission = 
							(AssignmentSubmission)studentIdSubmissionMap.get(studentId);
						
						if (thisSubmission == null) {
							// no submission exists for this student yet, so just
							// add an empty rec to the returned list
							thisSubmission = new AssignmentSubmission(assignment, studentId);
						} else {
							// we need to filter restricted info from instructor
							// if this is draft, so let's create a copy of this
							// object and modify it
							thisSubmission = AssignmentSubmission.deepCopy(thisSubmission, false, false, false);
							filterOutRestrictedInfo(thisSubmission, externalLogic.getCurrentUserId(), false);
						}
						
						// populate this submission's status		
						Integer status = getSubmissionStatusConstantForCurrentVersion(
								thisSubmission.getCurrentSubmissionVersion(), dueDate);
						thisSubmission.setSubmissionStatusConstant(status);
						
						viewableSubmissions.add(thisSubmission);
					}
				}
			}
			
			// if this assignment is graded, populate the grade information
			if (!assignment.isUngraded() && assignment.getGradableObjectId() != null) {
				gradebookLogic.populateGradesForSubmissions(contextId, viewableSubmissions, assignment);
			}
		}

		return viewableSubmissions;
	}
	
	public void setSubmissionStatusConstantForAssignments(List<Assignment2> assignments, String studentId) {
		if (studentId == null) {
			throw new IllegalArgumentException("Null studentId passed to setSubmissionStatusForAssignments");
		}
		
		if (assignments != null) {
			String contextId = externalLogic.getCurrentContextId();
			
			// retrieve the associated submission recs with current version data populated
			List<AssignmentSubmission> submissions = dao.getCurrentAssignmentSubmissionsForStudent(assignments, studentId);
			Map<Long, AssignmentSubmission> assignmentIdToSubmissionMap = new HashMap<Long, AssignmentSubmission>();
			if (submissions != null) {
				for (Iterator subIter = submissions.iterator(); subIter.hasNext();) {
					AssignmentSubmission submission = (AssignmentSubmission) subIter.next();
					if (submission != null) {
						Assignment2 assign = submission.getAssignment();
						if (assign != null) {
							assignmentIdToSubmissionMap.put(assign.getId(), submission);
						}
					}
				}
			}
			
			for (Iterator assignIter = assignments.iterator(); assignIter.hasNext();) {
				Assignment2 assign = (Assignment2)assignIter.next();

				if (assign != null) {
					AssignmentSubmission currSubmission = (AssignmentSubmission)assignmentIdToSubmissionMap.get(assign.getId());
					AssignmentSubmissionVersion currVersion = currSubmission != null ? 
							currSubmission.getCurrentSubmissionVersion() : null;
							
					// to check for late, we need the due date
					if (!assign.isUngraded() && assign.getGradableObjectId() != null) {
						gradebookLogic.populateGradebookItemDetailsForAssignment(contextId, assign);
					}
					
					Date dueDate = assign.isUngraded() ? assign.getDueDateForUngraded() : assign.getDueDate();
					
					Integer status = getSubmissionStatusConstantForCurrentVersion(currVersion, dueDate);
					assign.setSubmissionStatusConstant(new Integer(status));
				}
			}
		}
	}
	
	public Integer getSubmissionStatusConstantForCurrentVersion(AssignmentSubmissionVersion currentVersion,
			Date dueDate) {
		int status = AssignmentConstants.SUBMISSION_NOT_STARTED;
		
		if (currentVersion == null) {
			status = AssignmentConstants.SUBMISSION_NOT_STARTED;
		} else if (currentVersion.getId() == null) {
			status = AssignmentConstants.SUBMISSION_NOT_STARTED;
		} else if (currentVersion.isDraft()) {
			status = AssignmentConstants.SUBMISSION_IN_PROGRESS;
		} else if (currentVersion.getSubmittedTime() != null) {
			if (dueDate != null && dueDate.before(currentVersion.getSubmittedTime())) {
				status = AssignmentConstants.SUBMISSION_LATE;
			} else {
				status = AssignmentConstants.SUBMISSION_SUBMITTED;
			}
		}
		
		return status;
	}
	
	public void sortSubmissions(List<AssignmentSubmission> submissionList, String sortBy, boolean ascending) {
		if (submissionList != null && !submissionList.isEmpty()) {
			Comparator<AssignmentSubmission> comp;
			if(AssignmentSubmissionLogic.SORT_BY_RELEASED.equals(sortBy)) {
				comp = new ComparatorsUtils.SubmissionFeedbackReleasedComparator();
			} else if(AssignmentSubmissionLogic.SORT_BY_SUBMIT_DATE.equals(sortBy)) {
				comp = new ComparatorsUtils.SubmissionDateComparator();
			} else if(AssignmentSubmissionLogic.SORT_BY_GRADE.equals(sortBy)) {
				comp = new ComparatorsUtils.SubmissionGradeComparator();
			}else if(AssignmentSubmissionLogic.SORT_BY_STATUS.equals(sortBy)){
				comp = new ComparatorsUtils.SubmissionStatusComparator();
			} else {
				comp = new ComparatorsUtils.SubmissionNameComparator();
			}

			Collections.sort(submissionList, comp);
			if(!ascending) {
				Collections.reverse(submissionList);
			}
		}
	}
	
	private void populateVersionForAttachmentSet(Set attachSet, AssignmentSubmissionVersion version) {
		if (attachSet != null && !attachSet.isEmpty()) {
			for (Iterator attachIter = attachSet.iterator(); attachIter.hasNext();) {
				SubmissionAttachmentBase attach = (SubmissionAttachmentBase) attachIter.next();
				if (attach != null) {
					attach.setSubmissionVersion(version);
				}
			}
		}
	}
	
	private Set identifyAttachmentsToDelete(Set existingAttachSet, Set updatedAttachSet) {
		Set attachToRemove = new HashSet();
		
		if (existingAttachSet != null) {
			for (Iterator existingIter = existingAttachSet.iterator(); existingIter.hasNext();) {
				SubmissionAttachmentBase attach = (SubmissionAttachmentBase) existingIter.next();
				if (attach != null) {
					if (updatedAttachSet == null ||
							!updatedAttachSet.contains(attach)) {
						// we need to delete this attachment
						attachToRemove.add(attach);
					} 
				}
			}
		} 
		
		return attachToRemove;
	}
	
	private Set identifyAttachmentsToCreate(Set updatedAttachSet) {
		Set attachToCreate = new HashSet();
		
		if (updatedAttachSet != null) {
			for (Iterator attachIter = updatedAttachSet.iterator(); attachIter.hasNext();) {
				SubmissionAttachmentBase attach = (SubmissionAttachmentBase) attachIter.next();
				if (attach != null) {
					if (attach.getId() == null) {
						attachToCreate.add(attach);
					} 
				}
			}
		} 
		
		return attachToCreate;
	}
	
	public boolean submissionIsOpenForStudentForAssignment(String studentId, Long assignmentId) {
		if (studentId == null || assignmentId == null) {
			throw new IllegalArgumentException("null parameter passed to studentAbleToSubmit");
		} 

		Assignment2 assignment = (Assignment2)dao.findById(Assignment2.class, assignmentId);
		if (assignment == null) {
			throw new IllegalArgumentException("No assignment exists with id " + assignmentId);
		}
		
		// retrieve the submission history for this student for this assignment
		AssignmentSubmission submission = dao.getSubmissionWithVersionHistoryForStudentAndAssignment(studentId, assignment);
		Set<AssignmentSubmissionVersion> versionHistory = null;
		if (submission != null) {
			versionHistory = dao.getVersionHistoryForSubmission(submission);
		}
		
		// we need to determine if this is the first submission for the student
		int currNumSubmissions = 0;
		if (submission == null) {
			currNumSubmissions = 0;
		} else if (versionHistory == null) {
			currNumSubmissions = 0;
		} else {
			// we need to look at the submission history to determine if there
			// are any submission by the student (not drafts and not versions
			// created by instructor feedback when no submission)
			for (Iterator versionIter = versionHistory.iterator(); versionIter.hasNext();) {
				AssignmentSubmissionVersion version = (AssignmentSubmissionVersion) versionIter.next();
				if (version != null) {
					if (version.getSubmittedTime() != null) {
						currNumSubmissions++;
					}
				}
			}
		}
		
		// there are several factors into whether a student may make a submission or not
		// 1) if student has not already made a submission:
		//   a) assignment is open AND if applicable, accept until date has not passed
		// 2) student does have a submission
		//	 a) assignment is open AND if applicable, accept until date has not passed
		//		AND instructor has allowed resubmission until accept until date and
		// 		max num submissions has not been reached
		//   OR
		//   b) instructor has allowed resubmission for this student and the
		//		resubmit until date has not passed and max num submissions has
		//		not been reached
		
		boolean studentAbleToSubmit = false;
		boolean assignmentIsOpen = assignment.getOpenTime().before(new Date()) && 
		(assignment.getAcceptUntilTime() == null ||
					(assignment.getAcceptUntilTime() != null && 
							assignment.getAcceptUntilTime().after(new Date())));
		
		if (currNumSubmissions == 0) {
			if (assignmentIsOpen) {
				studentAbleToSubmit = true;
			} else if(submission != null){
				// in this scenario, the instructor took some action on this student without there
				// being a submission, such as adding feedback or allowing this student to resubmit.
				// thus, there is a submission record, but the student hasn't submitted anything yet
				// in this case, we need to check if the student is allowed to resubmit

				if (submission.getResubmitCloseTime() == null ||
						submission.getResubmitCloseTime().after(new Date())) {
					if (submission.getNumSubmissionsAllowed() != null && 
							(submission.getNumSubmissionsAllowed() == null || 
							submission.getNumSubmissionsAllowed().intValue() > currNumSubmissions)) {
						studentAbleToSubmit = true;
					}
				}
			}
		} else {
			// this is a resubmission, so we need to check for resubmission privileges
			// first, check for resubmission on the assignment level
			if (assignmentIsOpen) { 
				if (assignment.getNumSubmissionsAllowed() != null &&
						(assignment.getNumSubmissionsAllowed().equals(new Integer(-1)) ||
						assignment.getNumSubmissionsAllowed().intValue() > currNumSubmissions)) {
					studentAbleToSubmit = true;
				}
			} 
			
			// if they still can't submit, check the student level
			if (!studentAbleToSubmit) {
				if (submission.getResubmitCloseTime() == null || 
						submission.getResubmitCloseTime().after(new Date()))
				{
					if (submission.getNumSubmissionsAllowed() != null && 
							(submission.getNumSubmissionsAllowed().equals(new Integer(-1)) || 
							submission.getNumSubmissionsAllowed().intValue() > currNumSubmissions)) {
						studentAbleToSubmit = true;
					}
				}
			}
		}
		
		return studentAbleToSubmit;
	}

	public boolean isMostRecentVersionDraft(AssignmentSubmission submission) {
		if (submission == null) {
			throw new IllegalArgumentException("null submission passed to currentVersionIsDraft");
		}

		boolean currVersionIsDraft = false;
		
		// if the id is null, there is no current version
		if (submission.getId() != null) {
			AssignmentSubmissionVersion version = dao.getCurrentSubmissionVersionWithAttachments(submission);
	
			if (version != null && version.isDraft()) {
				currVersionIsDraft = true;
			}
		}

		return currVersionIsDraft;
	}
	
	public void releaseAllFeedbackForAssignment(Long assignmentId) {
		if (assignmentId == null) {
			throw new IllegalArgumentException("null assignmentId passed to releaseAllFeedbackForAssignment");
		}

		String contextId = externalLogic.getCurrentContextId();
		if (!gradebookLogic.isCurrentUserAbleToGrade(contextId)) {
			throw new SecurityException("User attempted to release feedback for assignment " + assignmentId + " without authorization");
		}
		
		Assignment2 assignment = (Assignment2) dao.findById(Assignment2.class, assignmentId);
		if (assignment == null) {
			throw new IllegalArgumentException("Assignment with id " + assignmentId + " does not exist");
		}
		
		List<String> gradableStudents = permissionLogic.getGradableStudentsForUserForItem(assignment);
		if (gradableStudents != null && !gradableStudents.isEmpty()) {
			Set<AssignmentSubmission> submissionList = dao.getSubmissionsWithVersionHistoryForStudentListAndAssignment(
					gradableStudents, assignment);
			
			Date releasedTime = new Date();
			
			if (submissionList != null && !submissionList.isEmpty()) {
				
				Set<AssignmentSubmissionVersion> versionsToUpdate = new HashSet<AssignmentSubmissionVersion>();
				
				for (Iterator subIter = submissionList.iterator(); subIter.hasNext();) {
					AssignmentSubmission submission = (AssignmentSubmission) subIter.next();
					if (submission != null) {
						if (submission.getSubmissionHistorySet() != null &&
								!submission.getSubmissionHistorySet().isEmpty()) {
							// we need to iterate through all of the versions and
							// release them
							for (Iterator versionIter = submission.getSubmissionHistorySet().iterator(); versionIter.hasNext();) {
								AssignmentSubmissionVersion version = (AssignmentSubmissionVersion) versionIter.next();
								if (version != null) {
									version.setReleasedTime(releasedTime);
									versionsToUpdate.add(version);
								}
							}
						}
					}
				}
				
				try {
					dao.saveMixedSet(new Set[] { versionsToUpdate });
					if (log.isDebugEnabled()) log.debug("All versions for assignment " + assignmentId + " released by " + externalLogic.getCurrentUserId());
				} catch (HibernateOptimisticLockingFailureException holfe) {
					if(log.isInfoEnabled()) log.info("An optimistic locking failure occurred while attempting to update submission versions for assignment " + assignmentId);
		            throw new StaleObjectModificationException(holfe);
				} catch (StaleObjectStateException sose) {
					if(log.isInfoEnabled()) log.info("An optimistic locking failure occurred while attempting to update submission versions for assignment " + assignmentId);
					throw new StaleObjectModificationException(sose);
				}
			}
		}
	}
	
	public void releaseAllFeedbackForSubmission(Long submissionId) {
		if (submissionId == null) {
			throw new IllegalArgumentException("null submissionId passed to releaseAllFeedbackForSubmission");
		}

		AssignmentSubmission subWithHistory = dao.getSubmissionWithVersionHistoryById(submissionId);

		if (subWithHistory == null) {
			throw new IllegalArgumentException("No submission exists with id " + submissionId);
		}

		if (!permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(subWithHistory.getUserId(), subWithHistory.getAssignment())) {
			throw new SecurityException("User " + externalLogic.getCurrentUserId() + " attempted to release feedback" +
					" for student " + subWithHistory.getUserId() + " and assignment " + 
					subWithHistory.getAssignment().getId() + "without authorization");
		}

		if (subWithHistory.getSubmissionHistorySet() != null &&
				!subWithHistory.getSubmissionHistorySet().isEmpty()) {
			// we need to iterate through all of the versions and
			// release them
			Date releasedTime = new Date();
			Set<AssignmentSubmissionVersion> updatedVersions = new HashSet<AssignmentSubmissionVersion>();
			for (Iterator versionIter = subWithHistory.getSubmissionHistorySet().iterator(); versionIter.hasNext();) {
				AssignmentSubmissionVersion version = (AssignmentSubmissionVersion) versionIter.next();
				if (version != null) {
					version.setReleasedTime(releasedTime);
					updatedVersions.add(version);
				}
			}
			
			try {
				dao.saveMixedSet(new Set[] { updatedVersions });
				if (log.isDebugEnabled()) log.debug("All submission versions for submission " + submissionId + " released by " + externalLogic.getCurrentUserId());
			} catch (HibernateOptimisticLockingFailureException holfe) {
				if(log.isInfoEnabled()) log.info("An optimistic locking failure occurred while attempting to update release all version for submission " + submissionId);
	            
				throw new StaleObjectModificationException(holfe);
			}
		}
	}
	
	public void releaseFeedbackForVersion(Long submissionVersionId) {
		if (submissionVersionId == null) {
			throw new IllegalArgumentException("Null submissionVersionId passed to releaseFeedbackForVersion");
		}
		
		AssignmentSubmissionVersion version = (AssignmentSubmissionVersion)dao.findById(
				AssignmentSubmissionVersion.class, submissionVersionId);
		if (version == null) {
			throw new IllegalArgumentException("No version " + submissionVersionId + " exists");
		}
		
		AssignmentSubmission submission = version.getAssignmentSubmission();
		
		if (!permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(submission.getUserId(), submission.getAssignment())) {
			throw new SecurityException("User " + externalLogic.getCurrentUserId() + " attempted to release feedback" +
					" for student " + submission.getUserId() + " and assignment " + 
					submission.getAssignment().getId() + "without authorization");
		}
		
		version.setReleasedTime(new Date());
		
		try {
			dao.update(version);
			if (log.isDebugEnabled()) log.debug("Version " + version.getId() + " released by " + externalLogic.getCurrentUserId());
		} catch (HibernateOptimisticLockingFailureException holfe) {
			if(log.isInfoEnabled()) log.info("An optimistic locking failure occurred while attempting to update submission version" + version.getId());
            throw new StaleObjectModificationException(holfe);
		}
	}
	
	/**
	 * when retrieving a submission and/or version, some fields may be restricted
	 * for the curr user. If curr user is the submitter and feedback has not been
	 * released, we do not want to return feedback. If curr user is not the submitter
	 * and the version is draft, we do not want to return the submission text and attach
	 * @param submission - do not pass the persistent object since we do not
	 * want to save the changes we are making
	 * @param currentUserId
	 * @param includeHistory - true if the submissionHistorySet was populated and
	 * needs to be filtered, as well
	 */
	private void filterOutRestrictedInfo(AssignmentSubmission submission, String currentUserId, boolean includeHistory) {
		// if the current user is the submitter and feedback has not been 
		// released, do not return the feedback info
		// if the current user is not the submitter and a version is draft, 
		// do not return any of the submission info

		// check the version history
		if (includeHistory) {
			if (submission.getSubmissionHistorySet() != null && !submission.getSubmissionHistorySet().isEmpty()) {
				for (Iterator versionIter = submission.getSubmissionHistorySet().iterator(); versionIter.hasNext();) {
					AssignmentSubmissionVersion version = (AssignmentSubmissionVersion) versionIter.next();
					filterOutRestrictedVersionInfo(version, currentUserId);
				}
			}
		}
		
		// also check the currentVersion
		if (submission.getCurrentSubmissionVersion() != null) {
			filterOutRestrictedVersionInfo(submission.getCurrentSubmissionVersion(), currentUserId);
		}
	}
	
	/**
	 * when retrieving a submission and/or version, some fields may be restricted
	 * for the curr user. If curr user is the submitter and feedback has not been
	 * released, we do not want to return feedback. If curr user is not the submitter
	 * and the version is draft, we do not want to return the submission text and attach
	 * @param version - do not pass the persistent object since we do not
	 * want to save the changes we are making
	 * @param currentUserId
	 */
	private void filterOutRestrictedVersionInfo(AssignmentSubmissionVersion version, String currentUserId) {
		if (version != null) {
			// if the current user is the submitter
			if (version.getAssignmentSubmission().getUserId().equals(currentUserId)) {
				if (version.getReleasedTime() == null || version.getReleasedTime().after(new Date())) {
					// do not populate the feedback since not released 
					version.setFeedbackAttachSet(new HashSet());
					version.setFeedbackNotes("");
					version.setAnnotatedText("");
					if (log.isDebugEnabled()) log.debug("Not populating feedback-specific info b/c curr user is submitter and feedback not released");
				}
			} else {
				// do not populate submission info if still draft
				if (version.isDraft()) {
					version.setSubmittedText("");
					version.setSubmissionAttachSet(new HashSet());
					if (log.isDebugEnabled()) log.debug("Not populating submission-specific info b/c draft status and current user is not submitter");
				}
			}
		}
	}
	
	public List<AssignmentSubmissionVersion> getVersionHistoryForSubmission(AssignmentSubmission submission) {
		if (submission == null) {
			throw new IllegalArgumentException("Null submission passed to getVersionHistoryForSubmission");
		}
		
		List<AssignmentSubmissionVersion> filteredVersionHistory = new ArrayList<AssignmentSubmissionVersion>();
		
		if (!permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(submission.getUserId(), submission.getAssignment())) {
			throw new SecurityException("User " + externalLogic.getCurrentUserId() +
					" attempted to access version history for student " + submission.getUserId() +
					" without authorization!");
		}

		// if id is null, this submission does not exist yet - will return empty
		// version history
		if (submission.getId() != null) {
			String currentUserId = externalLogic.getCurrentUserId();

			Set historySet = dao.getVersionHistoryForSubmission(submission);
			if (historySet != null && !historySet.isEmpty()) {
				for (Iterator vIter = historySet.iterator(); vIter.hasNext();) {
					AssignmentSubmissionVersion version = (AssignmentSubmissionVersion) vIter.next();
					if (version != null) {
						AssignmentSubmissionVersion versionCopy = 
							AssignmentSubmissionVersion.deepCopy(version, true, true);
						filterOutRestrictedVersionInfo(versionCopy, currentUserId);
						filteredVersionHistory.add(versionCopy);
					}

				}}
		} else {
			if (log.isDebugEnabled()) log.debug("Submission does not exist so no version history retrieved");
		}
		
		Collections.sort(filteredVersionHistory, new ComparatorsUtils.VersionCreatedDateComparatorDesc());
		
		return filteredVersionHistory;
	}
	
	private AssignmentSubmission getAssignmentSubmissionForStudentAndAssignment(Assignment2 assignment, String studentId) {
		AssignmentSubmission submission = null;
		
		List<AssignmentSubmission> subList = dao.findByProperties(AssignmentSubmission.class, 
				new String[] {"assignment", "userId"}, new Object[] {assignment, studentId});
		
		if (subList != null && subList.size() > 0) {
			submission = (AssignmentSubmission) subList.get(0);
		}
		
		return submission;
	}

	/**
	 * Process uploaded zip file.
	 * 
	 * @param fileFromUpload
	 * @param assignmentId
	 * @param options
	 */
//	public void processUploadAll(Long assignmentId, UploadAllOptions options, MultipartFile fileFromUpload)
//	{
////		SessionState state = ((JetspeedRunData) data)
////				.getPortletSessionState(((JetspeedRunData) data).getJs_peid());
////		ParameterParser params = data.getParameters();
//
////		String contextString = toolManager.getCurrentPlacement().getContext();
////		String toolTitle = toolManager.getTool(ASSIGNMENT_TOOL_ID).getTitle();
////		String aReference = (String) state.getAttribute(EXPORT_ASSIGNMENT_REF);
//		String associateGradebookAssignment = null;
//
//		// TODO move this to validation
//		if (!options.isSubmissionText() && !options.isSubmissionAttachments()
//				&& !options.isFeedbackText() && !options.isGradeFile()
//				&& !options.isComments() && !options.isFeedbackAttachments())
//		{
//			// has to choose one upload feature
//			addAlert(state, rb.getString("uploadall.alert.choose.element"));
//		}
//		else
//		{
//			// constructor the hashtable for all submission objects
//			Hashtable submisTable = new Hashtable();
//			Assignment2 assn = null;
//			Set<AssignmentSubmission> subs = null;
//			try
//			{
//				assn = assignmentLogic.getAssignmentById(assignmentId);
//				associateGradebookAssignment = StringUtil.trimToNull(assn.getProperties()
//						.getProperty(AssignmentService.PROP_ASSIGNMENT_ASSOCIATE_GRADEBOOK_ASSIGNMENT));
//				subs = assn.getSubmissionsSet();
//				if (subs != null)
//				{
//					for (AssignmentSubmission sub : subs)
//					{
//						AssignmentSubmissionVersion ver = sub.getCurrentSubmissionVersion();
//						String userId = StringUtil.trimToNull(ver.getCreatedBy());
//						if (userId != null)
//						{
//							User user = userDirectoryService.getUser(userId);
//							String feedback = (sub.getSubmissionStatus() == AssignmentConstants.SUBMISSION_SUBMITTED && ver.getSubmittedTime() != null) ? ver
//									.getSubmittedTime().toString() : "", s.getFeedbackText();
//							UploadGradeWrapper ugw = new UploadGradeWrapper(sub.getGradebookGrade()
//									, ver.getSubmittedText(), ver.getFeedbackNotes(), ver
//									.getSubmissionAttachSet(), ver.getFeedbackAttachSet(), feedback);
//							submisTable.put(user.getDisplayId(), ugw);
//						}
//					}
//				}
//			}
//			catch (Exception e)
//			{
//				Log.warn("chef", e.toString());
//			}
//
//			// see if the user uploaded a file
////			String fileName = null;
////			fileFromUpload = params.getFileItem("file");
//
//			String max_file_size_mb = ServerConfigurationService.getString("content.upload.max",
//					"1");
//			int max_bytes = 1024 * 1024;
//			try
//			{
//				max_bytes = Integer.parseInt(max_file_size_mb) * 1024 * 1024;
//			}
//			catch (NumberFormatException e)
//			{
//				// if unable to parse an integer from the value
//				// in the properties file, use 1 MB as a default
//				max_file_size_mb = "1";
//				max_bytes = 1024 * 1024;
//			}
//
//			// TODO move this to validation
//			if (fileFromUpload == null)
//			{
//				// "The user submitted a file to upload but it was too big!"
//				addAlert(state, rb.getString("uploadall.size") + " " + max_file_size_mb + "MB "
//						+ rb.getString("uploadall.exceeded"));
//			}
//			// TODO move this to validation
//			else if (fileFromUpload.getFileName() == null
//					|| fileFromUpload.getFileName().length() == 0)
//			{
//				// no file
//				addAlert(state, rb.getString("uploadall.alert.zipFile"));
//			}
//			else
//			{
//				byte[] fileData = fileFromUpload.getBytes();
//
//				if (fileData.length >= max_bytes)
//				{
//					addAlert(state, rb.getString("uploadall.size") + " " + max_file_size_mb + "MB "
//							+ rb.getString("uploadall.exceeded"));
//				}
//				else if (fileData.length > 0)
//				{
//					ZipInputStream zin = new ZipInputStream(new ByteArrayInputStream(fileData));
//					ZipEntry entry;
//
//					try
//					{
//						while ((entry = zin.getNextEntry()) != null)
//						{
//							String entryName = entry.getName();
//							if (!entry.isDirectory() && entryName.indexOf("/.") == -1)
//							{
//								if (entryName.endsWith("grades.csv"))
//								{
//									if (options.isGradeFile())
//									{
//										// read grades.cvs from zip
//										String result = StringUtil.trimToZero(readIntoString(zin));
//										String[] lines = null;
//										if (result.indexOf("\r\n") != -1)
//											lines = result.split("\r\n");
//										else if (result.indexOf("\r") != -1)
//											lines = result.split("\r");
//										else if (result.indexOf("\n") != -1)
//											lines = result.split("\n");
//										// skip the first 3 lines because they are headers
//										// - line 1: assignment title, type of grading
//										// - line 2: blank line
//										// - line 3: column header
//										for (int i = 3; i < lines.length; i++)
//										{
//											// escape the first three header lines
//											String[] items = lines[i].split(",");
//											if (items.length > 4)
//											{
//												// has grade information
//												try
//												{
//													// user eid
//													User u = userDirectoryService.getUserByEid(items[1]);
//													if (u != null)
//													{
//														UploadGradeWrapper w = (UploadGradeWrapper) submisTable
//																.get(u.getDisplayId());
//														if (w != null)
//														{
//															String itemString = items[4];
//															int gradeType = assn.getContent()
//																	.getTypeOfGrade();
//															if (gradeType == Assignment.SCORE_GRADE_TYPE)
//															{
//																validPointGrade(state, itemString);
//															}
//															else
//															{
//																validLetterGrade(state, itemString);
//															}
//															// check that no error messages were generated
//															if (state.getAttribute(STATE_MESSAGE) == null)
//															{
//																w
//																		.setGrade(gradeType == Assignment.SCORE_GRADE_TYPE ? scalePointGrade(
//																				state, itemString)
//																				: itemString);
//																submisTable.put(u
//																		.getDisplayId(), w);
//															}
//														}
//													}
//												}
//												catch (Exception e)
//												{
//													log.warn(e.getMessage(), e);
//												}
//											}
//										}
//									}
//								}
//								else
//								{
//									// get user eid part
//									String userEid = "";
//									if (entryName.indexOf("/") != -1)
//									{
//										// remove the part of zip name
//										userEid = entryName.substring(entryName.indexOf("/") + 1);
//										// get out the user name part
//										if (userEid.indexOf("/") != -1)
//										{
//											userEid = userEid.substring(0, userEid.indexOf("/"));
//										}
//										// get the eid part
//										if (userEid.indexOf("(") != -1)
//										{
//											userEid = userEid.substring(userEid.indexOf("(") + 1,
//													userEid.indexOf(")"));
//										}
//										userEid = StringUtil.trimToNull(userEid);
//									}
//									if (submisTable.containsKey(userEid))
//									{
//										if (options.isComments() && entryName.indexOf("comments") != -1)
//										{
//											// read the comments file
//											String comment = getBodyTextFromZipHtml(zin);
//											if (comment != null)
//											{
//												UploadGradeWrapper r = (UploadGradeWrapper) submisTable
//														.get(userEid);
//												r.setComment(comment);
//												submisTable.put(userEid, r);
//											}
//										}
//										if (options.isFeedbackText()
//												&& entryName.indexOf("feedbackText") != -1)
//										{
//											// upload the feedback text
//											String text = getBodyTextFromZipHtml(zin);
//											if (text != null)
//											{
//												UploadGradeWrapper r = (UploadGradeWrapper) submisTable
//														.get(userEid);
//												r.setFeedbackText(text);
//												submisTable.put(userEid, r);
//											}
//										}
//										if (options.isSubmissionText()
//												&& entryName.indexOf("_submissionText") != -1)
//										{
//											// upload the student submission text
//											String text = getBodyTextFromZipHtml(zin);
//											if (text != null)
//											{
//												UploadGradeWrapper r = (UploadGradeWrapper) submisTable
//														.get(userEid);
//												r.setText(text);
//												submisTable.put(userEid, r);
//											}
//										}
//										if (options.isSubmissionAttachments())
//										{
//											// upload the submission attachment
//											String submissionFolder = "/"
//													+ rb
//															.getString("download.submission.attachment")
//													+ "/";
//											if (entryName.indexOf(submissionFolder) != -1)
//											{
//												// clear the submission attachment first
//												UploadGradeWrapper r = (UploadGradeWrapper) submisTable
//														.get(userEid);
//												r.setSubmissionAttachments(new Vector());
//												submisTable.put(userEid, r);
//												uploadZipAttachments(state, submisTable, zin,
//														entry, entryName, userEid, "submission");
//											}
//										}
//										if (options.isFeedbackAttachments())
//										{
//											// upload the feedback attachment
//											String submissionFolder = "/"
//													+ rb.getString("download.feedback.attachment")
//													+ "/";
//											if (entryName.indexOf(submissionFolder) != -1)
//											{
//												// clear the submission attachment first
//												UploadGradeWrapper r = (UploadGradeWrapper) submisTable
//														.get(userEid);
//												r.setFeedbackAttachments(new Vector());
//												submisTable.put(userEid, r);
//												uploadZipAttachments(state, submisTable, zin,
//														entry, entryName, userEid, "feedback");
//											}
//										}
//
//										// if this is a timestamp file
//										if (entryName.indexOf("timestamp") != -1)
//										{
//											byte[] timeStamp = readIntoBytes(zin, entryName, entry
//													.getSize());
//											UploadGradeWrapper r = (UploadGradeWrapper) submisTable
//													.get(userEid);
//											r.setSubmissionTimestamp(new String(timeStamp));
//											submisTable.put(userEid, r);
//										}
//									}
//								}
//							}
//						}
//					}
//					catch (IOException e)
//					{
//						// uploaded file is not a valid archive
//						addAlert(state, rb.getString("uploadall.alert.zipFile"));
//
//					}
//				}
//			}
//
//			// check that no error messages were generated
//			if (state.getAttribute(STATE_MESSAGE) == null)
//			{
//				// update related submissions
//				if (assn != null && subs != null)
//				{
//					for (AssignmentSubmission sub : sub)
//					{
//						AssignmentSubmissionVersion ver = sub.getCurrentSubmissionVersion();
//						String userId = StringUtil.trimToNull(ver.getCreatedBy());
//						if (userId != null)
//						{
//							User user = userDirectoryService.getUser(userId);
//							if (submisTable.containsKey(user.getDisplayId()))
//							{
//								// update the AssignmetnSubmission record
//								try
//								{
//									AssignmentSubmissionEdit sEdit = AssignmentService
//											.editSubmission(s.getReference());
//
//									UploadGradeWrapper w = (UploadGradeWrapper) submisTable
//											.get(uName);
//
//									// the submission text
//									if (options.isSubmissionText())
//									{
//										sEdit.setSubmittedText(w.getText());
//									}
//
//									// the feedback text
//									if (options.isFeedbackText())
//									{
//										sEdit.setFeedbackText(w.getFeedbackText());
//									}
//
//									// the submission attachment
//									if (options.isSubmissionAttachments())
//									{
//										sEdit.clearSubmittedAttachments();
//										for (Iterator attachments = w.getSubmissionAttachments()
//												.iterator(); attachments.hasNext();)
//										{
//											sEdit.addSubmittedAttachment((Reference) attachments
//													.next());
//										}
//									}
//
//									// the feedback attachment
//									if (options.isFeedbackAttachments())
//									{
//										sEdit.clearFeedbackAttachments();
//										for (Iterator attachments = w.getFeedbackAttachments()
//												.iterator(); attachments.hasNext();)
//										{
//											sEdit.addFeedbackAttachment((Reference) attachments
//													.next());
//										}
//									}
//
//									// the feedback comment
//									if (options.isComments())
//									{
//										sEdit.setFeedbackComment(w.getComment());
//									}
//
//									// the grade file
//									if (options.isGradeFile())
//									{
//										// set grade
//										String grade = StringUtil.trimToNull(w.getGrade());
//										sEdit.setGrade(grade);
//										if (grade != null
//												&& !grade.equals(rb.getString("gen.nograd"))
//												&& !grade.equals("ungraded"))
//											sEdit.setGraded(true);
//									}
//
//									// release or not
//									if (sEdit.getGraded())
//									{
//										sEdit.setGradeReleased(options.isReleaseGrades());
//										sEdit.setReturned(options.isReleaseGrades());
//									}
//									else
//									{
//										sEdit.setGradeReleased(false);
//										sEdit.setReturned(false);
//									}
//
//									if (options.isReleaseGrades() && sEdit.getGraded())
//									{
//										sEdit.setTimeReturned(TimeService.newTime());
//									}
//
//									// if the current submission lacks timestamp while the timestamp
//									// exists inside the zip file
//									if (StringUtil.trimToNull(w.getSubmissionTimeStamp()) != null
//											&& sEdit.getTimeSubmitted() == null)
//									{
//										sEdit.setTimeSubmitted(TimeService.newTimeGmt(w
//												.getSubmissionTimeStamp()));
//										sEdit.setSubmitted(true);
//									}
//
//									// for further information
//									boolean graded = sEdit.getGraded();
//									String sReference = sEdit.getReference();
//
//									// commit
//									assignmentService.commitEdit(sEdit);
//
//									if (options.isReleaseGrades() && graded)
//									{
//										// update grade in gradebook
//										if (associateGradebookAssignment != null)
//										{
//											integrateGradebook(state, assignmentId,
//													associateGradebookAssignment, null, null, null,
//													-1, null, sReference, "update");
//										}
//									}
//
//								}
//								catch (Exception ee)
//								{
//									Log.debug("chef", ee.toString());
//								}
//							}
//						}
//					}
//				}
//			}
//		}
//
//		// check that no error messages were generated
//		if (state.getAttribute(STATE_MESSAGE) == null)
//		{
//			// go back to the list of submissions view
//			cleanUploadAllContext(state);
//			state.setAttribute(STATE_MODE, MODE_INSTRUCTOR_GRADE_ASSIGNMENT);
//		}
//	}

	private String readIntoString(ZipInputStream zin) throws IOException
	{
		StringBuilder buffer = new StringBuilder();
		int size = 2048;
		byte[] data = new byte[size];
		try
		{
			while ((size = zin.read(data, 0, data.length)) > 0)
			{
				buffer.append(new String(data, 0, size));
			}
		}
		catch (IOException e)
		{
			log.debug("assignment2 readIntoString", e);
		}
		return buffer.toString();
	}

	private String getBodyTextFromZipHtml(ZipInputStream zin)
	{
		String rv = "";
		try
		{
			rv = StringUtil.trimToNull(readIntoString(zin));
		}
		catch (IOException e)
		{
			log.debug("assignment2", e);
		}
		if (rv != null)
		{
			int start = rv.indexOf("<body>");
			int end = rv.indexOf("</body>");
			if (start != -1 && end != -1)
			{
				// get the text in between
				rv = rv.substring(start+6, end);
			}
		}
		return rv;
	}
}
