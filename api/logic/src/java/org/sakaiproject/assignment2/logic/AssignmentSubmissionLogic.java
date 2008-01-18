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

package org.sakaiproject.assignment2.logic;

import java.util.List;

import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;


/**
 * This is the interface for the AssignmentSubmission object
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public interface AssignmentSubmissionLogic {
	
	// sorting information
    public static final String SORT_BY_NAME = "name";
    public static final String SORT_BY_SUBMIT_DATE = "submitDate";
    public static final String SORT_BY_STATUS = "status";
    public static final String SORT_BY_GRADE = "grade";
    public static final String SORT_BY_RELEASED = "released";
    
	/**
	 * 
	 * @param submissionId
	 * @return Returns the AssignmentSubmission based on its assignmentSubmissionId.
	 * Does not populate current version information.
	 * @throws SecurityException if current user is not allowed to view the
	 * corresponding submission
	 */
	public AssignmentSubmission getAssignmentSubmissionById(Long submissionId);
	
	/**
	 * 
	 * @param submissionVersionId
	 * @return Returns the AssignmentSubmissionVersion with the given submissionVersionId.
	 * Will populate grading information. Returns null if no version with that id.
	 * @SecurityException if current user is not allowed to view the version
	 */
	public AssignmentSubmissionVersion getSubmissionVersionById(Long submissionVersionId);
	
	
	/**
	 * 
	 * @param assignmentId
	 * @param studentId
	 * @return AssignmentSubmission associated with the given Assignment and studentId.
	 * 		will return an empty record (with gb info populated, if appropriate)
	 * 		if there is no submission info for this student yet. if the
	 * 		most current submission is a draft, will flag the submission as "currentVersionIsDraft"
	 * 		and return the most recent non-draft version as the currentVersion
	 * @throws SecurityException if current user not allowed to view student's submission
	 */
	public AssignmentSubmission getCurrentSubmissionByAssignmentIdAndStudentIdForInstructorView(Long assignmentId, String studentId);
	
	/**
	 * 
	 * @param assignmentId
	 * @param studentId
	 * @return AssignmentSubmission associated with the given Assignment and studentId.
	 * 		will return an empty record (with gb info populated, if appropriate) 
	 * 		if there is no submission info for this student yet. populates
	 * 		currentVersion info
	 * @throws SecurityException if current user not allowed to view student's submission
	 */
	public AssignmentSubmission getCurrentSubmissionByAssignmentIdAndStudentIdForStudentView(Long assignmentId, String studentId);
	
	
	/**
	 * Create or update an AssignmentSubmission
	 * @param assignmentSubmission
	 * 			the AssignmentSubmission to create or update
	 */
	public void saveStudentSubmission(AssignmentSubmission assignmentSubmission);
	
	/**
	 * Create or update an AssignmentSubmission
	 * @param version
	 * 			the AssignmentSubmissionVersion to create. the parent AssignmentSubmission must be populated
	 * 
	 */
	public void saveStudentSubmission(AssignmentSubmissionVersion version);
	
	/**
	 * Save instructor feedback changes to the given submission rec
	 * @param submission
	 * @throws SecurityException if current user is not authorized to provide
	 * feedback for the given submission
	 */
	public void saveInstructorFeedback(AssignmentSubmission submission);
	
	/**
	 * Save instructor feedback changes to the given version. The passed
	 * version must have the associated AssignmentSubmission populated. If the
	 * submissionId on the associated submission is null, will create a new
	 * AssignmentSubmission record. If the id on the passed version is null, will
	 * create a new version
	 * @param version
	 * @throws SubmissionExistsException - if the AssignmentSubmission associated
	 * with the passed version has a null id, will attempt to create a new submission.
	 * throws exception if a submission rec already exists for the student and assignment
	 */
	public void saveInstructorFeedback(AssignmentSubmissionVersion version);
	
	/**
	 * 
	 * @param assignmentId
	 * @return all AssignmentSubmissions for this assignmentId that the current
	 * user is allowed to view or grade with the currentVersion information. If
	 * no submission exists yet, returns an empty AssigmentSubmission rec for the
	 * student
	 */
	public List<AssignmentSubmission> getViewableSubmissionsForAssignmentId(Long assignmentId);
	
	/**
	 * sets the submissionStatus (not persisted) for the given assignments
	 * ie submitted, not started, draft, etc
	 * @param assignments
	 * @param studentId
	 */
	public void setSubmissionStatusForAssignments(List<Assignment2> assignments, String studentId);
	
	/**
	 * 
	 * @param submission
	 * @return the constant equivalent for the given submission's status 
	 * ie In Progress, Submitted, etc
	 */
	public int getSubmissionStatus(AssignmentSubmission submission);
	
	/**
	 * We cannot rely on db sorting because we must sort by several properties that
	 * are not persisted in the A2 tables (ie status, grade, name, etc)
	 * @param submissionList
	 * @param sortBy
	 * @param ascending
	 */
	public void sortSubmissions(List<AssignmentSubmission> submissionList, String sortBy, boolean ascending);
	
	/**
	 * 
	 * @param studentId
	 * @param assignment
	 * @return true if the student is still able to make a submission for the given
	 * assignment at this time.  checks to see if assignment is open, if resubmission allowed,
	 * etc to determine if submission is still open
	 */
	public boolean submissionIsOpenForStudentForAssignment(String studentId, Assignment2 assignment);
	
	/**
	 * 
	 * @param submission
	 * @return true if the most recent AssignmentSubmissionVersion for this submission
	 * is a draft
	 */
	public boolean isMostRecentVersionDraft(AssignmentSubmission submission);
	
	/**
	 * set the version to "released" for all of the submissions that the current 
	 * user is able to submit feedback for. if the user is only authorized for
	 * a subset of the students for this assignment, the unauthorized students
	 * will not be affected
	 * @param assignment
	 * @throws SecurityException if user is not allowed to submit feedback
	 */
	public void releaseAllFeedbackForAssignment(Assignment2 assignment);
	
	/**
	 * set all of the non-draft versions for this submission to "released"
	 * @param submission
	 * @throws SecurityException if the current user is not authorized to 
	 * submit feedback for the given submission
	 */
	public void releaseAllFeedbackForSubmission(AssignmentSubmission submission);
	
	/**
	 * release the feedback for the given submission version to the submitter
	 * @param version
	 * @throws SecurityException if the current user is not authorized to 
	 * submit feedback for the given submission
	 */
	public void releaseFeedbackForVersion(AssignmentSubmissionVersion version);
	
}
