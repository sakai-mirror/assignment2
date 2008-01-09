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
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.PermissionLogic;
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
    
    private PermissionLogic permissionLogic;
    public void setPermissionLogic(PermissionLogic permissionLogic) {
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
			
			//if (!assignment.isUngraded() && assignment.getGradableObjectId() != null) {
				// retrieve the grade information for this submission
			//	Double grade = gradebookLogic.getStudentGradeForItem(contextId, studentId, assignment.getGradableObjectId());
			//	String comment = gradebookLogic.getStudentGradeCommentForItem(contextId, studentId, assignment.getGradableObjectId());
			//	submission.setGradebookGrade(grade);
			//	submission.setGradebookComment(comment);
			//}
		}
		return submission;

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
				throw new SecurityException("Current user is not allowed to view submission for " + studentId + " for assignment " + assignment.getAssignmentId());
			}
			
			List<AssignmentSubmission> submissionRecs = dao.findByProperties(AssignmentSubmission.class, new String[] {"assignment", "userId"}, new Object[] {assignment, studentId});
			if (submissionRecs != null && !submissionRecs.isEmpty()) {
				submission = (AssignmentSubmission)submissionRecs.get(0);
			}
			
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
		
		if (!permissionLogic.isUserAbleToMakeSubmissionForAssignment(externalLogic.getCurrentContextId(), assignmentSubmission.getAssignment())) {
			log.warn("User " + externalLogic.getCurrentUserId() + " attempted to make a submission " +
					"without authorization for assignment " + assignmentSubmission.getAssignment().getAssignmentId());
			throw new SecurityException("User " + externalLogic.getCurrentUserId() + " attempted to make a submission " +
					"without authorization for assignment " + assignmentSubmission.getAssignment().getAssignmentId());
		}
		
		AssignmentSubmissionVersion currVersion = assignmentSubmission.getCurrentSubmissionVersion();
		Set<AssignmentSubmissionAttachment> submissionAttachments = currVersion.getSubmissionAttachSet();
		
		if (currVersion == null) {
			throw new IllegalArgumentException("null currentSubmissionVersion associated with the assignmentSubmission in saveStudentSubmission");
		}
		
		AssignmentSubmissionVersion newVersion = currVersion.clone();
		// we need to add the attachments after we have a persistent version
		newVersion.setSubmissionAttachSet(null);
		
		if (assignmentSubmission.getSubmissionId() == null) {
			dao.create(assignmentSubmission);
			log.debug("New student submission rec added for user " + assignmentSubmission.getUserId() + " for assignment " + assignmentSubmission.getAssignment().getTitle() + " ID: " + assignmentSubmission.getAssignment().getAssignmentId());
			
			newVersion.setAssignmentSubmission(assignmentSubmission);
			// populate the feedback text with the student's submitted text
			newVersion.setFeedbackText(newVersion.getSubmittedText()); 
			// wipe out any old feedback info
			newVersion.setFeedbackAttachSet(null);
			newVersion.setLastFeedbackSubmittedBy(null);
			newVersion.setLastFeedbackTime(null);
		}
		
		dao.create(newVersion);
		log.debug("New student submission version added for user " + assignmentSubmission.getUserId() + " for assignment " + assignmentSubmission.getAssignment().getTitle()+ " ID: " + assignmentSubmission.getAssignment().getAssignmentId());
		
		newVersion.setSubmissionAttachSet(submissionAttachments);
		updateStudentAttachments(newVersion);
	}
	
	public void saveInstructorFeedback(AssignmentSubmission submission) {
		if (submission == null) {
			throw new IllegalArgumentException("null submission passed to saveInstructorFeedback");
		}
		
		if (submission.getAssignment() == null) {
			throw new IllegalArgumentException("no assignment associated with the given submission");
		}
		
		if (!permissionLogic.isUserAbleToProvideFeedbackForSubmission(submission)) {
			throw new SecurityException("User " + externalLogic.getCurrentUserId() + " attempted to submit feedback for student " + submission.getUserId() + " without authorization");
		}
		
		AssignmentSubmissionVersion existingVersion = null;
		
		// the instructor is submitting feedback even though the student has
		// not made a submission
		if (submission.getSubmissionId() == null) {
			dao.create(submission);
			log.debug("New student submission rec added for user " + submission.getUserId() + " for assignment " + submission.getAssignment().getTitle() + " ID: " + submission.getAssignment().getAssignmentId()
						+ " added by " + externalLogic.getCurrentUserId() + " via saveInstructorFeedback");
		} else {
			// retrieve the most current non-draft version
			existingVersion = dao.getCurrentSubmissionVersionWithAttachments(submission, Boolean.TRUE);

			dao.update(submission);
			log.debug("Submission updated for user " + submission.getUserId() + " for assignment " + submission.getAssignment().getTitle() + " ID: " + submission.getAssignment().getAssignmentId()
					+ " by " + externalLogic.getCurrentUserId() + " via saveInstructorFeedback");
		}
		
		// now we need to handle the currentVersion information
		AssignmentSubmissionVersion currentVersion = submission.getCurrentSubmissionVersion();
		// if there was no currentVersion passed, the instructor may be updating an
		// AssignmentSubmission field that doesn't affect version info (such as resubmit info)
		// no need to save new version 
		Set<AssignmentFeedbackAttachment> feedbackAttachments =
			currentVersion.getFeedbackAttachSet();

		if (currentVersion != null) {
			// we need to handle attachments separately
			currentVersion.setFeedbackAttachSet(null);
			
			if (currentVersion.getSubmissionVersionId() != null) {
				// instructor is providing feedback on the student's current version
				dao.update(currentVersion);
			} else {
				// instructor is providing feedback but the student did not
				// have a submission yet
				currentVersion.setAssignmentSubmission(submission);
				dao.create(currentVersion);
			}
		}
		
		currentVersion.setFeedbackAttachSet(feedbackAttachments);
		updateFeedbackAttachments(existingVersion, currentVersion);	
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
							assignmentIdToSubmissionMap.put(assign.getAssignmentId(), submission);
						}
					}
				}
			}
			
			for (Iterator assignIter = assignments.iterator(); assignIter.hasNext();) {
				Assignment2 assign = (Assignment2)assignIter.next();

				if (assign != null) {
					AssignmentSubmission currSubmission = (AssignmentSubmission)assignmentIdToSubmissionMap.get(assign.getAssignmentId());
					int status = AssignmentConstants.SUBMISSION_NOT_STARTED;
					if (currSubmission == null) {
						status = AssignmentConstants.SUBMISSION_NOT_STARTED;
					} else if (currSubmission.getCurrentSubmissionVersion() == null) {
						status = AssignmentConstants.SUBMISSION_NOT_STARTED;
					} else if (currSubmission.getCurrentSubmissionVersion().isDraft()) {
						status = AssignmentConstants.SUBMISSION_IN_PROGRESS;
					} else if (currSubmission.getCurrentSubmissionVersion().getSubmittedTime() != null) {
						status = AssignmentConstants.SUBMISSION_SUBMITTED;
					}

					assign.setSubmissionStatusConstant(new Integer(status));
				}
			}
		}
	}
	
	public int getSubmissionStatus(AssignmentSubmission submission) {
		int status = AssignmentConstants.SUBMISSION_NOT_STARTED;
		
		if (submission == null) {
			status = AssignmentConstants.SUBMISSION_NOT_STARTED;
		} else if (submission.getCurrentSubmissionVersion() == null) {
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
	
	private void updateStudentAttachments(AssignmentSubmissionVersion newVersion) {
		if (newVersion == null) {
			throw new IllegalArgumentException("null version passed to updateStudentAttachments");
		}
		
		if (newVersion.getSubmissionVersionId() == null) {
			throw new IllegalArgumentException("the version passed to updateStudentAttachments must exist in db. id was null");
		}
		
		if (newVersion.getSubmissionAttachSet() != null) {
			for (Iterator attachIter = newVersion.getSubmissionAttachSet().iterator(); attachIter.hasNext();) {
				AssignmentSubmissionAttachment attach = (AssignmentSubmissionAttachment) attachIter.next();
				if (attach != null) {
					attach.setSubmissionAttachId(null);
					attach.setSubmissionVersion(newVersion);
					dao.create(attach);
					log.debug("SubmissionAttachment created with id " + attach.getSubmissionAttachId());
				}
			}
		}
	}
	
	private void updateFeedbackAttachments(AssignmentSubmissionVersion existingVersion, AssignmentSubmissionVersion updatedVersion) {
		if (updatedVersion == null) {
			throw new IllegalArgumentException("Null updatedVersion passed to updateFeedbackAttachments");
		}
		
		if (updatedVersion.getSubmissionVersionId() == null) {
			throw new IllegalArgumentException("the version passed to updateFeedbackAttachments must exist in db. id was null");
		}
		
		Set<AssignmentFeedbackAttachment> revisedAttachSet = new HashSet();
		
		if (updatedVersion.getFeedbackAttachSet() != null && !updatedVersion.getFeedbackAttachSet().isEmpty()) {
        	for (Iterator attachIter = updatedVersion.getFeedbackAttachSet().iterator(); attachIter.hasNext();) {
        		AssignmentFeedbackAttachment attach = (AssignmentFeedbackAttachment) attachIter.next();
        		if (attach != null && attach.getFeedbackAttachId() == null) {
        			// this is a new attachment and needs to be created
        			attach.setSubmissionVersion(updatedVersion);
        			dao.save(attach);
        			log.debug("New feedback attachment created: " + attach.getAttachmentReference() + "with attach id " + attach.getFeedbackAttachId());
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
						log.debug("Feedback attachment deleted with id: " + attach.getFeedbackAttachId());
					} else if (updatedVersion.getFeedbackAttachSet() != null &&
							updatedVersion.getFeedbackAttachSet().contains(attach)) {
						revisedAttachSet.add(attach);
					}
				}
			}
		}
		
		updatedVersion.setFeedbackAttachSet(revisedAttachSet);
	}
	
	public boolean studentAbleToSubmit(String studentId, Assignment2 assignment) {
		if (studentId == null || assignment == null) {
			throw new IllegalArgumentException("null parameter passed to studentAbleToSubmit");
		} 

		// retrieve the submission history for this student for this assignment
		AssignmentSubmission submission = dao.getSubmissionWithVersionHistoryForStudentAndAssignment(studentId, assignment);
		
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
					assignment.getAcceptUntilTime().after(new Date());
		
		if (firstSubmission) {
			if (assignmentIsOpen) {
				studentAbleToSubmit = true;
			} 
		} else {
			if (assignmentIsOpen && assignment.isAllowResubmit()) {
				studentAbleToSubmit = true;
			} else if (submission.isAllowResubmit()) {
				if (submission.getResubmitCloseTime() != null && submission.getResubmitCloseTime().after(new Date())) {
					studentAbleToSubmit = true;
				}
			}
		}
		
		return studentAbleToSubmit;
	}

}
