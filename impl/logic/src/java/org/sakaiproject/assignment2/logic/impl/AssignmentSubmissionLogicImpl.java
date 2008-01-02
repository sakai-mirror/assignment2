/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/api/logic/src/java/org/sakaiproject/assignment2/dao/AssignmentDao.java $
 * $Id: AssignmentDao.java 12544 2006-05-03 15:06:26Z wagnermr@iupui.edu $
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

import java.util.Date;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.PermissionLogic;
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
		//TODO -- Add Security stuff pllllz
		AssignmentSubmission submission =  (AssignmentSubmission) dao.findById(AssignmentSubmission.class, submissionId);
		Assignment2 assignment = submission.getAssignment();
		// if the submission rec exists, we need to grab the most current version
		if (submission != null) {
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
	
	public AssignmentSubmission getCurrentSubmissionByAssignmentIdAndStudentId(Long assignmentId, String studentId, boolean includeDraft) {
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
					// TODO - retrieve in the proper format!
					Double grade = gradebookLogic.getStudentGradeForItem(contextId, studentId, assignment.getGradableObjectId());
					String comment = gradebookLogic.getStudentGradeCommentForItem(contextId, studentId, assignment.getGradableObjectId());
					submission.setGradebookGrade(grade.toString());
					submission.setGradebookComment(comment);
				}
			}
		}
		
		return submission;
	}

	public void saveStudentSubmission(AssignmentSubmission assignmentSubmission) {
		if (assignmentSubmission == null) {
			throw new IllegalArgumentException("null assignmentSubmission passed to saveAssignmentSubmission");
		}
		
		AssignmentSubmissionVersion newVersion = assignmentSubmission.getCurrentSubmissionVersion();
		
		if (newVersion == null) {
			throw new IllegalArgumentException("null currentSubmissionVersion associated with the assignmentSubmission in saveStudentSubmission");
		}
		
		// we always save a new AssignmentSubmissionVersion
		if (newVersion.getSubmissionVersionId() != null) {
			newVersion.setSubmissionVersionId(null);
		}
		
		if (assignmentSubmission.getSubmissionId() == null) {
			dao.create(assignmentSubmission);
			log.debug("New student submission rec added for user " + assignmentSubmission.getUserId() + " for assignment " + assignmentSubmission.getAssignment().getTitle() + " ID: " + assignmentSubmission.getAssignment().getAssignmentId());
			
			newVersion.setAssignmentSubmission(assignmentSubmission);
			// wipe out any old feedback for this new version
			newVersion.setFeedbackText(null); 
			newVersion.setFeedbackAttachSet(null);
		}
		
		dao.create(newVersion);
		log.debug("New student submission version added for user " + assignmentSubmission.getUserId() + " for assignment " + assignmentSubmission.getAssignment().getTitle()+ " ID: " + assignmentSubmission.getAssignment().getAssignmentId());
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
	
	/**
	 * Given an assignment, returns the associated AssignmentSubmission records 
	 * without the current submission information populated
	 * @param assignment
	 * @return
	 */
	private List<AssignmentSubmission> getAllSubmissionsForAssignmentNoVersionData(Assignment2 assignment) {
    	if (assignment == null) {
    		throw new IllegalArgumentException("null assignmentId passed to getSubmissionsByAssignmentIdNoVersionData");
    	}
    	
    	List<AssignmentSubmission> submissionList = dao.findByProperties(AssignmentSubmission.class, 
    			new String[] {"assignment"}, new Object[] {assignment});
    	
    	return submissionList;
    }

}
