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

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AttachmentBase;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
import org.sakaiproject.assignment2.model.SubmissionAttachmentBase;
import org.sakaiproject.assignment2.model.FeedbackAttachment;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.exception.StaleObjectModificationException;
import org.sakaiproject.assignment2.exception.SubmissionExistsException;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.utils.ComparatorsUtils;
import org.sakaiproject.assignment2.dao.AssignmentDao;
import org.sakaiproject.content.api.ContentHostingService;
import org.sakaiproject.content.api.ContentResource;
import org.sakaiproject.entity.api.Entity;
import org.sakaiproject.entity.api.ResourceProperties;
import org.sakaiproject.exception.IdUnusedException;
import org.sakaiproject.exception.PermissionException;
import org.sakaiproject.exception.ServerOverloadException;
import org.sakaiproject.exception.TypeException;
import org.sakaiproject.util.FormattedText;
import org.sakaiproject.util.StringUtil;
import org.sakaiproject.util.Validator;
import org.springframework.orm.hibernate3.HibernateOptimisticLockingFailureException;
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
    
    private ContentHostingService contentHostingService;
    public void setContentHostingService(ContentHostingService contentHostingService) {
    	this.contentHostingService = contentHostingService;
    }
    
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
				currentVersion = AssignmentSubmissionVersion.deepCopy(persistedCurrentVersion);
			}
			
			if (currentVersion != null) {
				filterOutRestrictedVersionInfo(currentVersion, currentUserId);
			}
			
			submission.setCurrentSubmissionVersion(currentVersion);
			
			if (!assignment.isUngraded() && assignment.getGradableObjectId() != null) {
				// retrieve the grade information for this submission
				gradebookLogic.populateAllGradeInfoForSubmission(currentContextId, 
						currentUserId, submission);
			}
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
			version = AssignmentSubmissionVersion.deepCopy(persistedVersion);
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
				submission = AssignmentSubmission.deepCopy(persistedSubmission);
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
			}
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
		AssignmentSubmission submission = dao.getSubmissionWithVersionHistoryForStudentAndAssignment(userId, assignment);
		AssignmentSubmissionVersion version = null;
		Set<SubmissionAttachment> attachToDelete = null;
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
				
				// let's make sure the subAttachSet isn't pointing to the
				// persistent objects
				if (subAttachSet != null && !subAttachSet.isEmpty()) {
					for (Iterator attachIter = subAttachSet.iterator(); attachIter.hasNext();) {
						SubmissionAttachment attach = (SubmissionAttachment) attachIter.next();
						if (attach != null) {
							attach = SubmissionAttachment.deepCopy(attach);
						}
					}
				}
				
				// identify any attachments that were removed from this version
				attachToDelete = identifyAttachmentsToDelete(version.getSubmissionAttachSet(), subAttachSet);
				
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

		// make sure the version was populated on the SubmissionAttachments
		populateVersionForAttachmentSet(subAttachSet, version);
		//version.setSubmissionAttachSet(subAttachSet);

		try {

			Set<AssignmentSubmissionVersion> versionSet = new HashSet();
			versionSet.add(version);

			Set<AssignmentSubmission> submissionSet = new HashSet();
			submissionSet.add(submission);
			
			if (subAttachSet == null) {
				subAttachSet = new HashSet();
			}

			dao.saveMixedSet(new Set[] {submissionSet, versionSet, subAttachSet});
			if (log.isDebugEnabled()) log.debug("Updated/Added student submission version " + version.getId() + " for user " + submission.getUserId() + " for assignment " + submission.getAssignment().getTitle()+ " ID: " + submission.getAssignment().getId());

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
	
	public void saveInstructorFeedback(AssignmentSubmissionVersion version) {
		if (version == null) {
			throw new IllegalArgumentException("null version passed to saveInstructorFeedback");
		}
		
		AssignmentSubmission submission = version.getAssignmentSubmission();
		if (submission == null) {
			throw new IllegalArgumentException("no submission associated with the given version");
		}
		
		if (submission.getAssignment() == null) {
			throw new IllegalArgumentException("no assignment was associated with this version's submission");
		}
		
		if (!permissionLogic.isUserAbleToProvideFeedbackForStudentForAssignment(submission.getUserId(), submission.getAssignment())) {
			throw new SecurityException("User " + externalLogic.getCurrentUserId() + " attempted to submit feedback for student " + submission.getUserId() + " without authorization");
		}
		
		Date currentTime = new Date();
		String currentUserId = externalLogic.getCurrentUserId();
		
		// the instructor is submitting feedback even though the student has
		// not made a submission
		if (submission.getId() == null) {
			// we need to check that this submission doesn't already exist
			List submissions = dao.findByProperties(AssignmentSubmission.class, 
					new String[] {"userId", "assignment"}, new Object[] {submission.getUserId(), submission.getAssignment()});
			if (submissions != null && submissions.size() > 0) {
				throw new SubmissionExistsException("User " + currentUserId + " attempted to save a duplicate " +
						"submission rec for userId " + submission.getUserId() + " and assignment " + submission.getAssignment().getId());
			}
		} 

		if (version.getId() != null) {
			// instructor is providing feedback on the student's current version
			try {
				// make sure you use the dao method b/c the logic method does not populate
				// submission info if draft
				AssignmentSubmissionVersion existingVersion = dao.getAssignmentSubmissionVersionByIdWithAttachments(version.getId());
				if (existingVersion == null) {
					throw new IllegalArgumentException("No version exists with id " + version.getId());
				}
				
				// identify the removed attachments before we start modifying the 
				// existing version
				Set<FeedbackAttachment> attachToDelete = 
					identifyAttachmentsToDelete(existingVersion.getFeedbackAttachSet(), version.getFeedbackAttachSet());
				
				// we will update the existing version to prevent overwriting 
				// non-feedback fields accidentally
				existingVersion.setReleasedTime(version.getReleasedTime());
				existingVersion.setAnnotatedText(version.getAnnotatedText());
				existingVersion.setFeedbackNotes(version.getFeedbackNotes());
				existingVersion.setLastFeedbackSubmittedBy(currentUserId);
				existingVersion.setLastFeedbackTime(currentTime);
				
				Set<FeedbackAttachment> attachSet = new HashSet();
				if (version.getFeedbackAttachSet() != null) {
					attachSet = version.getFeedbackAttachSet();
					populateVersionForAttachmentSet(attachSet, existingVersion);
				}
				
				Set<AssignmentSubmissionVersion> versionSet = new HashSet();
				versionSet.add(existingVersion);
				
				Set<AssignmentSubmission> submissionSet = new HashSet();
				submissionSet.add(submission);
				
				dao.saveMixedSet(new Set[] {submissionSet, versionSet, attachSet});
				if (log.isDebugEnabled()) log.debug("Updated student submission version " + existingVersion.getId() + " for user " + submission.getUserId() + " for assignment " + submission.getAssignment().getTitle()+ " ID: " + submission.getAssignment().getId());
				
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
			
		} else {
			// instructor is providing feedback but the student did not
			// have a submission yet
			AssignmentSubmissionVersion newVersion = new AssignmentSubmissionVersion();
			newVersion.setAssignmentSubmission(submission);
			newVersion.setDraft(Boolean.FALSE);
			newVersion.setAnnotatedText(version.getAnnotatedText());
			newVersion.setFeedbackNotes(version.getFeedbackNotes());
			newVersion.setReleasedTime(version.getReleasedTime());
			
			newVersion.setLastFeedbackSubmittedBy(currentUserId);
			newVersion.setLastFeedbackTime(currentTime);
			
			newVersion.setCreatedBy(externalLogic.getCurrentUserId());
			newVersion.setCreatedTime(currentTime);
			
			// we need to handle attachments specially. The version must be persisted
			// before attachment can be saved
			Set<FeedbackAttachment> attachSet = new HashSet();
			if (version.getFeedbackAttachSet() != null) {
				attachSet = version.getFeedbackAttachSet();
				populateVersionForAttachmentSet(attachSet, newVersion);
			}
			
			Set<AssignmentSubmissionVersion> versionSet = new HashSet();
			versionSet.add(newVersion);
			
			Set<AssignmentSubmission> submissionSet = new HashSet();
			submissionSet.add(submission);
			
			dao.saveMixedSet(new Set[] {submissionSet, versionSet, attachSet});
			if (log.isDebugEnabled()) log.debug("New submission version " + newVersion.getId() + " created by " + currentUserId + " via saveInstructorFeedback");
		}
	
	}
	
	public List<AssignmentSubmission> getViewableSubmissionsForAssignmentId(Long assignmentId) {
		if (assignmentId == null) {
			throw new IllegalArgumentException("null assignmentId passed to getViewableSubmissionsForAssignmentId");
		}
		
		String contextId = externalLogic.getCurrentContextId();
		
		List<AssignmentSubmission> viewableSubmissions = new ArrayList();
		
		Assignment2 assignment = (Assignment2)dao.findById(Assignment2.class, assignmentId);

		if (assignment != null) {
			// get a list of all the students that the current user may view for the given assignment
			List<String> viewableStudents = permissionLogic.getViewableStudentsForUserForItem(assignment);

			if (viewableStudents != null && !viewableStudents.isEmpty()) {

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
							if (thisSubmission.getCurrentSubmissionVersion() != null &&
									thisSubmission.getCurrentSubmissionVersion().isDraft()) {
								// we should any submission info b/c the
								// instructor may not see it when draft
								thisSubmission.getCurrentSubmissionVersion().setSubmissionAttachSet(new HashSet());
								thisSubmission.getCurrentSubmissionVersion().setSubmittedText("");
							} 
						}
						
						viewableSubmissions.add(thisSubmission);
					}
				}
			}
			
			// if this assignment is graded, populate the grade information
			if (!assignment.isUngraded() && assignment.getGradableObjectId() != null) {
				gradebookLogic.populateGradesForSubmissions(viewableSubmissions, assignment);
			}
		}

		return viewableSubmissions;
	}
	
	public void setSubmissionStatusForAssignments(List<Assignment2> assignments, String studentId) {
		if (studentId == null) {
			throw new IllegalArgumentException("Null studentId passed to setSubmissionStatusForAssignments");
		}
		
		if (assignments != null) {
			// retrieve the associated submission recs with current version data populated
			List<AssignmentSubmission> submissions = dao.getCurrentAssignmentSubmissionsForStudent(assignments, studentId);
			Map<Long, AssignmentSubmission> assignmentIdToSubmissionMap = new HashMap();
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
					int status = getSubmissionStatus(currSubmission);
					assign.setSubmissionStatusConstant(new Integer(status));
				}
			}
		}
	}
	
	public int getSubmissionStatus(AssignmentSubmission submission) {
		int status = AssignmentConstants.SUBMISSION_NOT_STARTED;
		
		if (submission == null) {
			status = AssignmentConstants.SUBMISSION_NOT_STARTED;
		} else if (submission.getCurrentSubmissionVersion() == null ||
				submission.getCurrentSubmissionVersion().getId() == null) {
			status = AssignmentConstants.SUBMISSION_NOT_STARTED;
		} else if (submission.getCurrentSubmissionVersion().isDraft()) {
			status = AssignmentConstants.SUBMISSION_IN_PROGRESS;
		} else if (submission.getCurrentSubmissionVersion().getSubmittedTime() != null) {
			status = AssignmentConstants.SUBMISSION_SUBMITTED;
		}
		
		return status;
	}
	
	public void sortSubmissions(List<AssignmentSubmission> submissionList, String sortBy, boolean ascending) {
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
				
				Set<AssignmentSubmissionVersion> versionsToUpdate = new HashSet();
				
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
			Set<AssignmentSubmissionVersion> updatedVersions = new HashSet();
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
		
		AssignmentSubmissionVersion version = getSubmissionVersionById(submissionVersionId);
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

	public void getSubmissionsZip(OutputStream outputStream, Assignment2 assignment, String feedbackDir, String submDir, Map<String, String> headers) throws PermissionException
	  {
	    String assignmentId = assignment.getId().toString();
	    if (log.isDebugEnabled()) log.debug(this + ": getSubmissionsZip reference=" + assignmentId);

	    List<AssignmentSubmission> submissions = getViewableSubmissionsForAssignmentId(assignment.getId());

	    StringBuilder exceptionMessage = new StringBuilder();
	    if (gradebookLogic.isCurrentUserAbleToGrade(assignment.getContextId()))
	    {
	      zipSubmissions(assignment, submissions.iterator(), outputStream, exceptionMessage, feedbackDir, submDir, headers);

	      if (exceptionMessage.length() > 0)
	      {
	        // log any error messages
	        if (log.isDebugEnabled())
	          log.debug(this + assignmentId + exceptionMessage.toString());
	      }
	    }
	  } // getSubmissionsZip
	
	protected void zipSubmissions(Assignment2 assignment, Iterator<AssignmentSubmission> submissions, OutputStream outputStream, StringBuilder exceptionMessage, String feedbackDir, String submDir, Map<String, String> headers)
	{
	  String assignmentTitle = assignment.getTitle();
	  //String assignmentId = assignment.getAssignmentId().toString();
	  String gradeTypeString = "fixme";
	  try
	  {
	    ZipOutputStream out = new ZipOutputStream(outputStream);

	    // create the folder structure - named after the assignment's title
	    String root = Validator.escapeZipEntry(assignmentTitle) + Entity.SEPARATOR;

	    String submittedText = "";
	    if (!submissions.hasNext())
	    {
	      exceptionMessage.append("There is no submission yet. ");
	    }

	    // the buffer used to store grade information
	    StringBuilder gradesBuilder = new StringBuilder(assignmentTitle + "," + gradeTypeString + "\n\n");
	    gradesBuilder.append(headers.get("id") + "," +
	        headers.get("eid") + "," +
	        headers.get("lastname") + "," +
	        headers.get("firstname") + "," +
	        headers.get("grade") + "\n");

	    // Create the ZIP file
	    String submittersName = "";
	    int count = 1;
	    while (submissions.hasNext())
	    {
	      AssignmentSubmission s = (AssignmentSubmission) submissions.next();
	      String userId = (String) s.getUserId();
	      AssignmentSubmissionVersion sv = getCurrentSubmissionByAssignmentIdAndStudentId(assignment.getId(), userId).getCurrentSubmissionVersion();

	      if (sv.getSubmittedTime() != null)
	      {
	        String name = externalLogic.getUserDisplayName(userId);
	        String fullName = externalLogic.getUserFullName(userId);
	        String submittersString = name + "(" + userId + ")";
	        gradesBuilder.append(name + "," + userId + "," + fullName + "," + s.getGradebookGrade() + "\n");

	        if (StringUtil.trimToNull(submittersString) != null)
	        {
	          submittersName = submittersName.concat(StringUtil.trimToNull(submittersString));
	          submittedText = sv.getSubmittedText();

	          boolean added = false;
	          while (!added)
	          {
	            try
	            {
	              submittersName = submittersName.concat("/");
	              // create the folder structure - named after the submitter's name
	              if (submittedText != null && submittedText != "")
	              {
	                // create the text file only when a text submission is allowed
	                ZipEntry textEntry = new ZipEntry(submittersName + submittersString + "_submissionText.txt");
	                out.putNextEntry(textEntry);
	                byte[] text = submittedText.getBytes();
	                out.write(text);
	                textEntry.setSize(text.length);
	                out.closeEntry();
	              }

	              // Write the timestamp for the submission
	              ZipEntry textEntry = new ZipEntry(submittersName + "timestamp.txt");
	              out.putNextEntry(textEntry);
	              byte[] b = (sv.getSubmittedTime().toString()).getBytes();
	              out.write(b);
	              textEntry.setSize(b.length);
	              out.closeEntry();

	              // the comments.txt file to show instructor's comments
	              ZipEntry ctextEntry = new ZipEntry(submittersName + "comments.txt");
	              out.putNextEntry(ctextEntry);
	              byte[] cb = FormattedText.encodeUnicode(sv.getFeedbackNotes()).getBytes();
	              out.write(cb);
	              ctextEntry.setSize(cb.length);
	              out.closeEntry();

	              // create an attachment folder for the feedback attachments
	              String feedbackSubAttachmentFolder = submittersName + feedbackDir + "/";
	              ZipEntry feedbackSubAttachmentFolderEntry = new ZipEntry(feedbackSubAttachmentFolder);
	              out.putNextEntry(feedbackSubAttachmentFolderEntry);
	              out.closeEntry();

	              // create a attachment folder for the submission attachments
	              String sSubAttachmentFolder = submittersName + submDir + "/";
	              ZipEntry sSubAttachmentFolderEntry = new ZipEntry(sSubAttachmentFolder);
	              out.putNextEntry(sSubAttachmentFolderEntry);
	              out.closeEntry();
	              // add all submission attachment into the submission attachment folder
	              zipAttachments(out, submittersName, sSubAttachmentFolder, sv.getSubmissionAttachSet());
	              // add all feedback attachment folder
	              zipAttachments(out, submittersName, feedbackSubAttachmentFolder, sv.getFeedbackAttachSet());

	              added = true;
	            }
	            catch (IOException e)
	            {
	              exceptionMessage.append("Can not establish the IO to create zip file for user "
	                  + submittersName);
	              log.debug(this + ": getSubmissionsZip--IOException unable to create the zip file for user"
	                  + submittersName);
	              submittersName = submittersName.substring(0, submittersName.length() - 1) + "_" + count++;
	            }
	          } //while
	        } // if
	      } // if
	    } // while -- there is submission

	    // create a grades.csv file into zip
	    ZipEntry gradesCSVEntry = new ZipEntry(root + "grades.csv");
	    out.putNextEntry(gradesCSVEntry);
	    byte[] grades = gradesBuilder.toString().getBytes();
	    out.write(grades);
	    gradesCSVEntry.setSize(grades.length);
	    out.closeEntry();

	    // Complete the ZIP file
	    out.finish();
	    out.flush();
	    out.close();
	  }
	  catch (IOException e)
	  {
	    exceptionMessage.append("Can not establish the IO to create zip file. ");
	    log.debug(this + ": getSubmissionsZip--IOException unable to create the zip file for assignment "
	        + assignmentTitle);
	  }
	}
	
	private void zipAttachments(ZipOutputStream out, String submittersName, String sSubAttachmentFolder, Set<? extends AttachmentBase> attachments) {
		  int attachedUrlCount = 0;
		  for (AttachmentBase r : attachments)
		  {
		    
		    try
		    {
		      ContentResource resource = contentHostingService.getResource(r.getAttachmentReference());

		      String contentType = resource.getContentType();

		      ResourceProperties props = resource.getProperties();
		      String displayName = props.getPropertyFormatted(props.getNamePropDisplayName());

		      // for URL content type, encode a redirect to the body URL
		      if (contentType.equalsIgnoreCase(ResourceProperties.TYPE_URL))
		      {
		        displayName = "attached_URL_" + attachedUrlCount;
		        attachedUrlCount++;
		      }

		      // buffered stream input
		      InputStream content = resource.streamContent();
		      byte data[] = new byte[1024 * 10];
		      BufferedInputStream bContent = new BufferedInputStream(content, data.length);

		      ZipEntry attachmentEntry = new ZipEntry(sSubAttachmentFolder + displayName);
		      out.putNextEntry(attachmentEntry);
		      int bCount = -1;
		      while ((bCount = bContent.read(data, 0, data.length)) != -1)
		      {
		        out.write(data, 0, bCount);
		      }
		      out.closeEntry();
		      content.close();
		    }
		    catch (PermissionException e)
		    {
		      log.debug(this + ": getSubmissionsZip--PermissionException submittersName="
		          + submittersName + " attachment reference=" + r);
		    }
		    catch (IdUnusedException e)
		    {
		      log.debug(this + ": getSubmissionsZip--IdUnusedException submittersName="
		          + submittersName + " attachment reference=" + r);
		    }
		    catch (TypeException e)
		    {
		      log.debug(this + ": getSubmissionsZip--TypeException: submittersName="
		          + submittersName + " attachment reference=" + r);
		    }
		    catch (IOException e)
		    {
		      log.debug(this + ": getSubmissionsZip--IOException: Problem in creating the attachment file: submittersName="
		              + submittersName + " attachment reference=" + r);
		    }
		    catch (ServerOverloadException e)
		    {
		      log.debug(this + ": getSubmissionsZip--ServerOverloadException: submittersName="
		          + submittersName + " attachment reference=" + r);
		    }
		  } // for
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
		
		List<AssignmentSubmissionVersion> filteredVersionHistory = new ArrayList();
		String currentUserId = externalLogic.getCurrentUserId();
		
		Set historySet = dao.getVersionHistoryForSubmission(submission);
		if (historySet != null && !historySet.isEmpty()) {
			for (Iterator vIter = historySet.iterator(); vIter.hasNext();) {
				AssignmentSubmissionVersion version = (AssignmentSubmissionVersion) vIter.next();
				if (version != null) {
					AssignmentSubmissionVersion versionCopy = AssignmentSubmissionVersion.deepCopy(version);
					filterOutRestrictedVersionInfo(versionCopy, currentUserId);
					filteredVersionHistory.add(versionCopy);
				}
			}
		}
		
		return filteredVersionHistory;
	}

}
