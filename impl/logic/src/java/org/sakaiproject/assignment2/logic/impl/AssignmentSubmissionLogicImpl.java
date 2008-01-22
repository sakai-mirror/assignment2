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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionAttachment;
import org.sakaiproject.assignment2.model.AssignmentFeedbackAttachment;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.exception.SubmissionExistsException;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.utils.ComparatorsUtils;
import org.sakaiproject.assignment2.dao.AssignmentDao;


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
    
	public void init(){
		log.debug("init");
	}
	
	public AssignmentSubmission getAssignmentSubmissionById(Long submissionId){
		if (submissionId == null) {
			throw new IllegalArgumentException("Null submissionId passed to getAssignmentSubmissionById");
		}

		AssignmentSubmission submission =  (AssignmentSubmission) dao.findById(AssignmentSubmission.class, submissionId);
		Assignment2 assignment = submission.getAssignment();

		// if the submission rec exists, we need to grab the most current version
		if (submission != null) {
			if (!permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(submission.getUserId(), assignment)) {
				throw new SecurityException("user" + externalLogic.getCurrentUserId() + " attempted to view submission with id " + submissionId + " but is not authorized");
			}
			AssignmentSubmissionVersion currentVersion = dao.getCurrentSubmissionVersionWithAttachments(submission, Boolean.FALSE);
			if (currentVersion != null) {
				if (!submission.getUserId().equals(externalLogic.getCurrentUserId())) {
					// if the current user is not the submitter, we don't want to display a draft version
					// but want it flagged as in progress
					if (!currentVersion.isDraft()) {
						submission.setCurrentVersionIsDraft(Boolean.FALSE);
					} else {
						submission.setCurrentVersionIsDraft(Boolean.TRUE);
						// we need to now retrieve the most recent non-draft version
						currentVersion = dao.getCurrentSubmissionVersionWithAttachments(submission, Boolean.TRUE);
					}
					
				}
			}
			
			submission.setCurrentSubmissionVersion(currentVersion);
			
			if (!assignment.isUngraded() && assignment.getGradableObjectId() != null) {
				// retrieve the grade information for this submission
				gradebookLogic.populateAllGradeInfoForSubmission(externalLogic.getCurrentContextId(), submission);
			}
		}
		return submission;

	}
	
	public AssignmentSubmissionVersion getSubmissionVersionById(Long submissionVersionId) {
		if (submissionVersionId == null) {
			throw new IllegalArgumentException("null submissionVersionId passed to getSubmissionVersionById");
		}
		
		AssignmentSubmissionVersion version = dao.getAssignmentSubmissionVersionByIdWithAttachments(submissionVersionId);
		AssignmentSubmission submission = version.getAssignmentSubmission();
		Assignment2 assignment = submission.getAssignment();
		
		if (version != null) {
			// only the student may view a draft version
			if (version.isDraft() && !submission.getUserId().equals(externalLogic.getCurrentUserId())) {
				throw new SecurityException("User " + externalLogic.getCurrentUserId() + " attempted to access a draft version " + 
						submissionVersionId + " for student " + submission.getUserId());
			}
			
			// ensure that the current user is authorized to view this user for this assignment
			if (!permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(submission.getUserId(), assignment)) {
				throw new SecurityException("User " + externalLogic.getCurrentUserId() + " attempted to access the version " + 
						submissionVersionId + " for student " + submission.getUserId() + " without authorization");
			}
			
			// populate gradebook information, if appropriate
			if (!assignment.isUngraded() && assignment.getGradableObjectId() != null) {
				gradebookLogic.populateAllGradeInfoForSubmission(externalLogic.getCurrentContextId(), submission);
			}
		}
		
		return version;
	}
	
	public AssignmentSubmission getCurrentSubmissionByAssignmentIdAndStudentIdForInstructorView(Long assignmentId, String studentId) {
		return getCurrentSubmissionByAssignmentIdAndStudentId(assignmentId, studentId, Boolean.TRUE);
	}
	
	public AssignmentSubmission getCurrentSubmissionByAssignmentIdAndStudentIdForStudentView(Long assignmentId, String studentId) {
		return getCurrentSubmissionByAssignmentIdAndStudentId(assignmentId, studentId, Boolean.FALSE);
	}
	
	private AssignmentSubmission getCurrentSubmissionByAssignmentIdAndStudentId(Long assignmentId, String studentId, boolean instructorView) {
		if (assignmentId == null || studentId == null) {
			throw new IllegalArgumentException("Null assignmentId or userId passed to getCurrentSubmissionByAssignmentAndUser");
		}
		
		String contextId = externalLogic.getCurrentContextId();
		
		AssignmentSubmission submission = null;
		
		Assignment2 assignment = dao.getAssignmentByIdWithGroups(assignmentId);
		if (assignment != null) {
			if (!permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(studentId, assignment)) {
				throw new SecurityException("Current user is not allowed to view submission for " + studentId + " for assignment " + assignment.getId());
			}
			
			submission = dao.getSubmissionWithVersionHistoryForStudentAndAssignment(studentId, assignment, !instructorView);
			
			// if the submission rec exists, we need to grab the most current version
			if (submission != null) {
				AssignmentSubmissionVersion currentVersion = dao.getCurrentSubmissionVersionWithAttachments(submission, Boolean.FALSE);
				if (currentVersion != null) {
					if (instructorView) {
						// if it is an instructor view, we don't want to display a draft version
						// but want it flagged as in progress. we will return the most recent
						// non-draft submission
						if (!currentVersion.isDraft()) {
							submission.setCurrentVersionIsDraft(Boolean.FALSE);
						} else {
							submission.setCurrentVersionIsDraft(Boolean.TRUE);
							// we need to now retrieve the most recent non-draft version
							currentVersion = dao.getCurrentSubmissionVersionWithAttachments(submission, Boolean.TRUE);
						}
						
					} else {
						submission.setCurrentVersionIsDraft(currentVersion.isDraft());
					}
				}
				
				submission.setCurrentSubmissionVersion(currentVersion);
				
			} else {
				submission = new AssignmentSubmission(assignment, studentId);
			}
			
			if (!assignment.isUngraded() && assignment.getGradableObjectId() != null) {
				// retrieve the grade information for this submission
				gradebookLogic.populateAllGradeInfoForSubmission(contextId, submission);
			}
		}
		
		return submission;
	}

	public void saveStudentSubmission(AssignmentSubmission assignmentSubmission) {
		if (assignmentSubmission == null) {
			throw new IllegalArgumentException("null assignmentSubmission passed to saveAssignmentSubmission");
		}
		
		AssignmentSubmissionVersion currVersion = assignmentSubmission.getCurrentSubmissionVersion();

		if (currVersion == null) {
			throw new IllegalArgumentException("null currentSubmissionVersion associated with the assignmentSubmission in saveStudentSubmission");
		}
		currVersion.setAssignmentSubmission(assignmentSubmission);
		saveStudentSubmission(currVersion);
	}
	
	public void saveStudentSubmission(AssignmentSubmissionVersion version) {
		if (version == null) {
			throw new IllegalArgumentException("null version passed to saveAssignmentSubmission");
		}
		
		AssignmentSubmission submission = version.getAssignmentSubmission();
		if (submission == null) {
			throw new IllegalArgumentException("no AssignmentSubmission record was associated with the given version in saveStudentSubmission");
		}
		
		if (!permissionLogic.isUserAbleToMakeSubmissionForAssignment(externalLogic.getCurrentContextId(), submission.getAssignment())) {
			log.warn("User " + externalLogic.getCurrentUserId() + " attempted to make a submission " +
					"without authorization for assignment " + submission.getAssignment().getId());
			throw new SecurityException("User " + externalLogic.getCurrentUserId() + " attempted to make a submission " +
					"without authorization for assignment " + submission.getAssignment().getId());
		}
		
		if (!submissionIsOpenForStudentForAssignment(externalLogic.getCurrentUserId(), submission.getAssignment())) {
			log.warn("User " + externalLogic.getCurrentUserId() + " attempted to make a submission " +
					"but submission for this user for assignment " + submission.getAssignment().getId() + " is closed.");
			throw new SecurityException("User " + externalLogic.getCurrentUserId() + " attempted to make a submission " +
					"for closed assignment " + submission.getAssignment().getId());
		}
		
		Date currentTime = new Date();
		
		// if there is no current version or the most recent version was submitted, we will need
		// to create a new version. If the current version is draft, we will continue to update
		// this version until it is submitted
		AssignmentSubmissionVersion existingVersion = null;
		if (submission.getId() == null) {
			// this is a new submission, so create it
			// we need to check that this submission doesn't already exist
			List submissions = dao.findByProperties(AssignmentSubmission.class, 
					new String[] {"userId", "assignment"}, new Object[] {submission.getUserId(), submission.getAssignment()});
			if (submissions != null && submissions.size() > 0) {
				throw new SubmissionExistsException("User " + externalLogic.getCurrentUserId() + " attempted to save a duplicate " +
						"submission rec for userId " + submission.getUserId() + " and assignment " + submission.getAssignment().getId());
			}
			
			dao.create(submission);
			log.debug("New student submission rec added for user " + submission.getUserId() + " for assignment " + submission.getAssignment().getTitle() + " ID: " + submission.getAssignment().getId());
		} else {
			existingVersion = dao.getCurrentSubmissionVersionWithAttachments(submission, Boolean.FALSE);
		}
		
		// we will handle attachments separately. we will set the attach set to null before
		// saving the version b/c the attachment objects may not exist yet
		Set<AssignmentSubmissionAttachment> submissionAttachments = version.getSubmissionAttachSet();
		
		if (existingVersion != null && existingVersion.isDraft()) {
			// we need to update this rec
			if (version.getId() == null) {
				version.setId(existingVersion.getId());
			}
			
			if (version.isDraft()) {
				version.setSubmittedTime(null);
			} else {
				version.setAnnotatedText(version.getSubmittedText());
				version.setSubmittedTime(currentTime);
			}
			
			// handle attachments after the save to account for attachment objects
			// that need to be created first or else we will get a pretty transient
			// object exception
			version.setSubmissionAttachSet(null);
			
			version.setModifiedBy(externalLogic.getCurrentUserId());
			version.setModifiedTime(currentTime);
			
			dao.update(version);
			log.debug("Updated student submission version " + existingVersion.getId() + " for user " + submission.getUserId() + " for assignment " + submission.getAssignment().getTitle()+ " ID: " + submission.getAssignment().getId());

			existingVersion.setSubmissionAttachSet(submissionAttachments);
			updateStudentAttachments(existingVersion, version);
		} else {
			// this is a new version
			AssignmentSubmissionVersion newVersion = version.clone();
			
			// if this isn't a draft, set the annotated text to be the submitted text
			// to allow instructor to edit in the future
			if (newVersion.isDraft()) {
				newVersion.setAnnotatedText(null);
				newVersion.setSubmittedTime(null);
			} else {
				newVersion.setAnnotatedText(newVersion.getSubmittedText());
				newVersion.setSubmittedTime(currentTime);
			}
			
			// clean up any old info that isn't pertinent to a new version
			// submitted by the student
			newVersion.setFeedbackNotes(null);
			newVersion.setFeedbackAttachSet(null);
			newVersion.setLastFeedbackSubmittedBy(null);
			newVersion.setLastFeedbackTime(null);
			newVersion.setModifiedBy(null);
			newVersion.setModifiedTime(null);
			
			// handle attachments separately
			newVersion.setSubmissionAttachSet(null);
			
			newVersion.setCreatedBy(externalLogic.getCurrentUserId());
			newVersion.setCreatedTime(currentTime);
			
			dao.create(newVersion);
			log.debug("New student submission version added for user " + submission.getUserId() + " for assignment " + submission.getAssignment().getTitle()+ " ID: " + submission.getAssignment().getId());

			newVersion.setSubmissionAttachSet(submissionAttachments);
			updateStudentAttachments(null, newVersion);
		}

	}
	
	public void saveInstructorFeedback(AssignmentSubmission submission) {
		if (submission == null) {
			throw new IllegalArgumentException("null submission passed to saveInstructorFeedback");
		}
		
		saveInstructorFeedback(submission.getCurrentSubmissionVersion());
	}
	
	public void saveInstructorFeedback(AssignmentSubmissionVersion version) {
		if (version == null) {
			throw new IllegalArgumentException("null version passed to saveInstructorFeedback");
		}
		
		AssignmentSubmission submission = version.getAssignmentSubmission();
		if (submission == null) {
			throw new IllegalArgumentException("no submission associated with the given version");
		}
		
		if (version.isDraft() && !externalLogic.getCurrentUserId().equals(submission.getUserId())) {
			throw new SecurityException("User " + externalLogic.getCurrentUserId() + " attempted to edit a draft version rec for student " + submission.getUserId());
		}
		
		if (!permissionLogic.isUserAbleToProvideFeedbackForSubmission(version.getAssignmentSubmission())) {
			throw new SecurityException("User " + externalLogic.getCurrentUserId() + " attempted to submit feedback for student " + submission.getUserId() + " without authorization");
		}
		
		AssignmentSubmissionVersion existingVersion = null;
		Date currentTime = new Date();
		
		// the instructor is submitting feedback even though the student has
		// not made a submission
		if (submission.getId() == null) {
			// we need to check that this submission doesn't already exist
			List submissions = dao.findByProperties(AssignmentSubmission.class, 
					new String[] {"userId", "assignment"}, new Object[] {submission.getUserId(), submission.getAssignment()});
			if (submissions != null && submissions.size() > 0) {
				throw new SubmissionExistsException("User " + externalLogic.getCurrentUserId() + " attempted to save a duplicate " +
						"submission rec for userId " + submission.getUserId() + " and assignment " + submission.getAssignment().getId());
			}
			
			dao.create(submission);
			log.debug("New student submission rec added for user " + submission.getUserId() + " for assignment " + submission.getAssignment().getTitle() + " ID: " + submission.getAssignment().getId()
						+ " added by " + externalLogic.getCurrentUserId() + " via saveInstructorFeedback");
		} else {
			// retrieve the most current non-draft version
			existingVersion = dao.getCurrentSubmissionVersionWithAttachments(submission, Boolean.TRUE);

			dao.update(submission);
			log.debug("Submission updated for user " + submission.getUserId() + " for assignment " + submission.getAssignment().getTitle() + " ID: " + submission.getAssignment().getId()
					+ " by " + externalLogic.getCurrentUserId() + " via saveInstructorFeedback");
		}


		Set<AssignmentFeedbackAttachment> feedbackAttachments =	version.getFeedbackAttachSet();

		// we need to handle attachments separately
		version.setFeedbackAttachSet(null);
		
		version.setLastFeedbackSubmittedBy(externalLogic.getCurrentUserId());
		version.setLastFeedbackTime(currentTime);

		if (version.getId() != null) {
			// instructor is providing feedback on the student's current version
			dao.update(version);
			log.debug("Submission version " + version.getId() + " updated by " + externalLogic.getCurrentUserId() + " via saveInstructorFeedback");
		} else {
			// instructor is providing feedback but the student did not
			// have a submission yet
			
			version.setCreatedBy(externalLogic.getCurrentUserId());
			version.setCreatedTime(currentTime);
			
			dao.create(version);
			log.debug("New submission version " + version.getId() + " created by " + externalLogic.getCurrentUserId() + " via saveInstructorFeedback");
		}

		version.setFeedbackAttachSet(feedbackAttachments);
		updateFeedbackAttachments(existingVersion, version);	
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
								thisSubmission.setCurrentVersionIsDraft(Boolean.TRUE);
							} else {
								thisSubmission.setCurrentVersionIsDraft(Boolean.FALSE);
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
			comp = new ComparatorsUtils.SubmissionGradeReleasedComparator();
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
	
	private void updateStudentAttachments(AssignmentSubmissionVersion existingVersion, AssignmentSubmissionVersion newVersion) {
		if (newVersion == null) {
			throw new IllegalArgumentException("null version passed to updateStudentAttachments");
		}
		
		if (newVersion.getId() == null) {
			throw new IllegalArgumentException("the version passed to updateStudentAttachments must exist in db. id was null");
		}
		
		if (existingVersion == null) {
			// All of these attachments need to be created
			if (newVersion.getSubmissionAttachSet() != null) {
				for (Iterator attachIter = newVersion.getSubmissionAttachSet().iterator(); attachIter.hasNext();) {
					AssignmentSubmissionAttachment attach = (AssignmentSubmissionAttachment) attachIter.next();
					if (attach != null) {
						attach.setId(null);
						attach.setSubmissionVersion(newVersion);
						dao.create(attach);
						log.debug("SubmissionAttachment created with id " + attach.getId());
					}
				}
			}
		} else {
			// we need to compare the old to the new because this is an update
			Set<AssignmentSubmissionAttachment> revisedAttachSet = new HashSet();
			
			if (newVersion.getSubmissionAttachSet() != null && !newVersion.getSubmissionAttachSet().isEmpty()) {
	        	for (Iterator attachIter = newVersion.getSubmissionAttachSet().iterator(); attachIter.hasNext();) {
	        		AssignmentSubmissionAttachment attach = (AssignmentSubmissionAttachment) attachIter.next();
	        		if (attach != null && attach.getId() == null) {
	        			// this is a new attachment and needs to be created
	        			attach.setSubmissionVersion(newVersion);
	        			dao.save(attach);
	        			log.debug("New submission attachment created: " + attach.getAttachmentReference() + "with attach id " + attach.getId());
	        			revisedAttachSet.add(attach);
	        		}
	        	}
	        }
			
			// now we need to handle the case in which existing attachments were removed
			if (existingVersion != null) {
				for (Iterator existingIter = existingVersion.getSubmissionAttachSet().iterator(); existingIter.hasNext();) {
					AssignmentSubmissionAttachment attach = (AssignmentSubmissionAttachment) existingIter.next();
					if (attach != null) {
						if (newVersion.getSubmissionAttachSet() == null ||
								!newVersion.getSubmissionAttachSet().contains(attach)) {
							// we need to delete this attachment
							dao.delete(attach);
							log.debug("Submission attachment deleted with id: " + attach.getId());
						} else if (newVersion.getSubmissionAttachSet() != null &&
								newVersion.getSubmissionAttachSet().contains(attach)) {
							revisedAttachSet.add(attach);
						}
					}
				}
				
				newVersion.setSubmissionAttachSet(revisedAttachSet);
			}
		}

	}
	
	private void updateFeedbackAttachments(AssignmentSubmissionVersion existingVersion, AssignmentSubmissionVersion updatedVersion) {
		if (updatedVersion == null) {
			throw new IllegalArgumentException("Null updatedVersion passed to updateFeedbackAttachments");
		}
		
		if (updatedVersion.getId() == null) {
			throw new IllegalArgumentException("the version passed to updateFeedbackAttachments must exist in db. id was null");
		}
		
		Set<AssignmentFeedbackAttachment> revisedAttachSet = new HashSet();
		
		if (updatedVersion.getFeedbackAttachSet() != null && !updatedVersion.getFeedbackAttachSet().isEmpty()) {
        	for (Iterator attachIter = updatedVersion.getFeedbackAttachSet().iterator(); attachIter.hasNext();) {
        		AssignmentFeedbackAttachment attach = (AssignmentFeedbackAttachment) attachIter.next();
        		if (attach != null && attach.getId() == null) {
        			// this is a new attachment and needs to be created
        			attach.setSubmissionVersion(updatedVersion);
        			dao.save(attach);
        			log.debug("New feedback attachment created: " + attach.getAttachmentReference() + "with attach id " + attach.getId());
        			revisedAttachSet.add(attach);
        		}
        	}
        }
		
		// now we need to handle the case in which existing attachments were removed
		if (existingVersion != null) {
			for (Iterator existingIter = existingVersion.getFeedbackAttachSet().iterator(); existingIter.hasNext();) {
				AssignmentFeedbackAttachment attach = (AssignmentFeedbackAttachment) existingIter.next();
				if (attach != null) {
					if (updatedVersion.getFeedbackAttachSet() == null ||
							!updatedVersion.getFeedbackAttachSet().contains(attach)) {
						// we need to delete this attachment
						dao.delete(attach);
						log.debug("Feedback attachment deleted with id: " + attach.getId());
					} else if (updatedVersion.getFeedbackAttachSet() != null &&
							updatedVersion.getFeedbackAttachSet().contains(attach)) {
						revisedAttachSet.add(attach);
					}
				}
			}
		}
		
		updatedVersion.setFeedbackAttachSet(revisedAttachSet);
	}
	
	public boolean submissionIsOpenForStudentForAssignment(String studentId, Assignment2 assignment) {
		if (studentId == null || assignment == null) {
			throw new IllegalArgumentException("null parameter passed to studentAbleToSubmit");
		} 

		// retrieve the submission history for this student for this assignment
		AssignmentSubmission submission = dao.getSubmissionWithVersionHistoryForStudentAndAssignment(studentId, assignment, Boolean.TRUE);
		
		// we need to determine if this is the first submission for the student
		boolean firstSubmission;
		if (submission == null) {
			firstSubmission = true;
		} else if (submission.getCurrentSubmissionVersion() == null
				|| submission.getSubmissionHistorySet() == null) {
			firstSubmission = true;
		} else {
			// we need to look at the submission history to determine if there
			// are any submission by the student (not drafts and not versions
			// created by instructor feedback when no submission)
			firstSubmission = true;
			for (Iterator versionIter = submission.getSubmissionHistorySet().iterator(); versionIter.hasNext();) {
				AssignmentSubmissionVersion version = (AssignmentSubmissionVersion) versionIter.next();
				if (version != null) {
					if (version.getSubmittedTime() != null) {
						firstSubmission = false;
					}
				}
			}
		}
		
		// there are several factors into whether a student may make a submission or not
		// 1) if student has not already made a submission:
		//   a) assignment is open AND if applicable, accept until date has not passed
		// 2) student does have a submission
		//	 a) assignment is open AND if applicable, accept until date has not passed
		//		AND instructor has allowed resubmission until accept until date
		//   OR
		//   b) instructor has allowed resubmission for this student and the
		//		resubmit until date has not passed
		
		boolean studentAbleToSubmit = false;
		boolean assignmentIsOpen = assignment.getOpenTime().before(new Date()) && 
		(assignment.getAcceptUntilTime() == null ||
					(assignment.getAcceptUntilTime() != null && 
							assignment.getAcceptUntilTime().after(new Date())));
		
		if (firstSubmission) {
			if (assignmentIsOpen) {
				studentAbleToSubmit = true;
			} else if(submission != null && submission.isAllowResubmit() != null && submission.isAllowResubmit()){
				// in this scenario, the instructor took some action on this student without there
				// being a submission, such as adding feedback or allowing this student to resubmit.
				// thus, there is a submission record, but the student hasn't submitted anything yet
				// in this case, we need to check if the student is allowed to resubmit
				if (submission.getResubmitCloseTime() != null && submission.getResubmitCloseTime().after(new Date())) {
					studentAbleToSubmit = true;
				}
			}
		} else {
			if (assignmentIsOpen && assignment.isAllowResubmit()) {
				studentAbleToSubmit = true;
			} else if (submission.isAllowResubmit() != null && submission.isAllowResubmit()) {
				if (submission.getResubmitCloseTime() != null && submission.getResubmitCloseTime().after(new Date())) {
					studentAbleToSubmit = true;
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
		
		// if the id is null, the method was passed a transient object, so there
		// is no current version
		if (submission.getId() != null) {
			AssignmentSubmissionVersion version = dao.getCurrentSubmissionVersionWithAttachments(submission, Boolean.FALSE);
	
			if (version != null && version.isDraft()) {
				currVersionIsDraft = true;
			}
		}

		return currVersionIsDraft;
	}
	
	public void releaseAllFeedbackForAssignment(Assignment2 assignment) {
		if (assignment == null) {
			throw new IllegalArgumentException("null assignment passed to releaseAllFeedbackForAssignment");
		}

		String contextId = externalLogic.getCurrentContextId();
		if (!gradebookLogic.isCurrentUserAbleToGrade(contextId)) {
			throw new SecurityException("User attempted to release feedback for assignment " + assignment.getId() + " without authorization");
		}
		
		List<String> gradableStudents = permissionLogic.getGradableStudentsForUserForItem(assignment);
		if (gradableStudents != null && !gradableStudents.isEmpty()) {
			List<AssignmentSubmission> submissionList = dao.getSubmissionsWithVersionHistoryForStudentListAndAssignment(
					gradableStudents, assignment, Boolean.FALSE);
			
			Date releasedTime = new Date();
			
			if (submissionList != null && !submissionList.isEmpty()) {
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
									dao.update(version);
									log.debug("Version " + version.getId() + " released by " + externalLogic.getCurrentUserId());
								}
							}
						}
					}
				}
			}
		}
	}
	
	public void releaseAllFeedbackForSubmission(AssignmentSubmission submission) {
		if (submission == null) {
			throw new IllegalArgumentException("null submission passed to releaseAllFeedbackForSubmission");
		}
		
		if (!permissionLogic.isUserAbleToProvideFeedbackForSubmission(submission)) {
			throw new SecurityException("User " + externalLogic.getCurrentUserId() + " attempted to release feedback" +
					" for student " + submission.getUserId() + " and assignment " + 
					submission.getAssignment().getId() + "without authorization");
		}
		
		// retrieve submission with history
		AssignmentSubmission subWithHistory = dao.getSubmissionWithVersionHistoryForStudentAndAssignment(
				submission.getUserId(), submission.getAssignment(), Boolean.FALSE);
		
		if (subWithHistory != null) {
			if (submission.getSubmissionHistorySet() != null &&
					!submission.getSubmissionHistorySet().isEmpty()) {
				// we need to iterate through all of the versions and
				// release them
				Date releasedTime = new Date();
				for (Iterator versionIter = submission.getSubmissionHistorySet().iterator(); versionIter.hasNext();) {
					AssignmentSubmissionVersion version = (AssignmentSubmissionVersion) versionIter.next();
					if (version != null) {
						version.setReleasedTime(releasedTime);
						dao.update(version);
						log.debug("Version " + version.getId() + " released by " + externalLogic.getCurrentUserId());
					}
				}
			}
		}
	}
	
	public void releaseFeedbackForVersion(AssignmentSubmissionVersion version) {
		if (version == null) {
			throw new IllegalArgumentException("Null version passed to releaseFeedbackForVersion");
		}
		
		AssignmentSubmission submission = version.getAssignmentSubmission();
		if (submission == null) {
			throw new IllegalArgumentException("No submission associated with passed version in releaseFeedbackForVersion");
		}
		
		if (!permissionLogic.isUserAbleToProvideFeedbackForSubmission(submission)) {
			throw new SecurityException("User " + externalLogic.getCurrentUserId() + " attempted to release feedback" +
					" for student " + submission.getUserId() + " and assignment " + 
					submission.getAssignment().getId() + "without authorization");
		}
		
		version.setReleasedTime(new Date());
		dao.update(version);
		log.debug("Version " + version.getId() + " released by " + externalLogic.getCurrentUserId());
	}
}
