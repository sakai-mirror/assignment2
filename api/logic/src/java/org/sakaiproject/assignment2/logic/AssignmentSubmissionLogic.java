
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

package org.sakaiproject.assignment2.logic;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.FeedbackAttachment;
import org.sakaiproject.assignment2.model.FeedbackVersion;
import org.sakaiproject.assignment2.model.SubmissionAttachment;

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
	 * Populates current version information. If version is draft and current
	 * user is not submitter, submittedText and submissionAttachments will not
	 * be populated. If the curr user is the submitter but feedback has not 
	 * been released, will not populate	feedback.
	 * @throws SecurityException if current user is not allowed to view the
	 * corresponding submission
	 * @throws SubmissionNotFoundException if no submission exists with the given id
	 */
	public AssignmentSubmission getAssignmentSubmissionById(Long submissionId);
	
	/**
	 * 
	 * @param submissionVersionId
	 * @return Returns the AssignmentSubmissionVersion with the given submissionVersionId.
	 * Will populate grading information.
	 * If the version is draft and the submitter is not the current user, will not
	 * populate the submissionText or submissionAttachmentSet. If the curr user is
	 * the submitter but feedback has not been released, will not populate
	 * feedback.
	 * @throws SecurityException if current user is not allowed to view the version
	 * @throws VersionNotFoundException if no version exists with the given id
	 */
	public AssignmentSubmissionVersion getSubmissionVersionById(Long submissionVersionId);
	
	
	/**
	 * 
	 * @param assignmentId
	 * @param studentId
	 * @return AssignmentSubmission associated with the given assignmentId and studentId.
	 * 		will return an empty record (with gb info populated, if appropriate)
	 * 		if there is no submission info for this student yet. If the curr version 
	 * 		is draft and the submitter is not the current user, will not
	 * 		populate the submissionText or submissionAttachmentSet. If the curr user is
	 * 		the submitter but feedback has not been released, will not populate
	 * 		feedback.
	 * @throws SecurityException if current user not allowed to view student's submission
	 * @throws AssignmentNotFoundException if no assignment exists with the given assignmentId
	 */
	public AssignmentSubmission getCurrentSubmissionByAssignmentIdAndStudentId(Long assignmentId, String studentId);
	
	/**
	 * Create or update an AssignmentSubmission and AssignmentSubmissionVersion.
	 * Will retrieve the current submission and determine whether a new submission
	 * and/or version is required.  Versions are updated until they are submitted.
	 * Each submission creates a new version. There will be one AssignmentSubmission
	 * record per student per assignment.
	 * @param userId - the submitter's userId
	 * @param assignment - which assignment this submission is for
	 * @param draft - true if this submission is draft
	 * @param submittedText - the submitter's text
	 * @param subAttachSet - the set of SubmissionAttachments associated with the
	 * version. if this is an update, will delete any existing attachments associated
	 * with the version that aren't included in this set
	 * @throws SecurityException if current user is not allowed to make this submission
	 */
	public void saveStudentSubmission(String userId, Assignment2 assignment, Boolean draft, 
			String submittedText, Set<SubmissionAttachment> subAttachSet);
	
	
	/**
	 * Save instructor feedback for a particular version. If student has not made
	 * a submission, will create the submission and version
	 * @param versionId - id of the version that you want to update. if null, there must
	 * not be a student submission yet
	 * @param studentId
	 * @param assignment
	 * @param numSubmissionsAllowed
	 * @param resubmitCloseTime
	 * @param annotatedText
	 * @param feedbackNotes
	 * @param releasedTime
	 * @param feedbackAttachSet
	 * @throws SecurityException if user is not allowed to submit feedback for
	 * the given student and assignment
	 */
	public void saveInstructorFeedback(Long versionId, String studentId, Assignment2 assignment, 
			Integer numSubmissionsAllowed, Date resubmitCloseTime, String annotatedText, 
			String feedbackNotes, Date releasedTime, Set<FeedbackAttachment> feedbackAttachSet);

	/**
	 * 
	 * @param assignmentId
	 * @return Non-null list.  All AssignmentSubmissions for this assignmentId that the current
	 * user is allowed to view or grade with the currentVersion information. If
	 * no submission exists yet, returns an empty AssigmentSubmission rec for the
	 * student
	 * @throws SecurityException if not allowed to view or grade submissions
	 * @throws AssignmentNotFoundException if no assignment exists with the given assignmentId
	 */
	public List<AssignmentSubmission> getViewableSubmissionsForAssignmentId(Long assignmentId);
	
	/**
	 * @param assignments - list of assignments that you want this student's status for
	 * @param studentId
	 * @return a map of Assignment2 object to its associated submission status for the
	 * given student (ie submitted, not started, draft, etc)
	 */
	public Map<Assignment2, Integer> getSubmissionStatusConstantForAssignments(List<Assignment2> assignments, String studentId);
	
	/**
	 * 
	 * @param currentVersion
	 * @param dueDate - due date for the associated assignment. if null, assumes
	 * no due date
	 * @return the constant equivalent for the given submission's status 
	 * ie In Progress, Submitted, etc based upon the passed currentVersion. will
	 * return the constant for "Not started" if currentVersion is null
	 */
	public Integer getSubmissionStatusConstantForCurrentVersion(AssignmentSubmissionVersion currentVersion,
			Date dueDate);
	
	/**
	 * 
	 * @param studentId
	 * @param assignmentId
	 * @return true if the student is still able to make a submission for the given
	 * assignment at this time.  checks to see if assignment is open, if resubmission allowed,
	 * etc to determine if submission is still open
	 * @throws AssignmentNotFoundException if no assignment exists with given assignmentId
	 */
	public boolean submissionIsOpenForStudentForAssignment(String studentId, Long assignmentId);
	
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
	 * @param assignmentId
	 * @throws SecurityException if user is not allowed to submit feedback
	 * @throws AssignmentNotFoundException if no assignment with the given assignmentId
	 */
	public void releaseAllFeedbackForAssignment(Long assignmentId);
	
	/**
	 * set all of the non-draft versions for this submission to "released"
	 * @param submissionId
	 * @throws SecurityException if the current user is not authorized to 
	 * submit feedback for the given submission
	 * @throws SubmissionNotFoundException if no AssignmentSubmission with the given submissionId
	 */
	public void releaseAllFeedbackForSubmission(Long submissionId);
	
	/**
	 * release the feedback for the given submission version to the submitter
	 * @param submissionVersionId
	 * @throws SecurityException if the current user is not authorized to 
	 * submit feedback for the given submission
	 * @throws VersionNotFoundException if no version exists with the given submissionVersionId
	 */
	public void releaseFeedbackForVersion(Long submissionVersionId);

	/**
	 * 
	 * @param submission
	 * @return a list of all of the AssignmentSubmissionVersions associated with
	 * the given submission. If the version is draft and the submitter is not 
	 * the current user, will not populate the submissionText or 
	 * submissionAttachmentSet. If the curr user is	the submitter but feedback 
	 * has not been released, will not populate	feedback. if the passed submission
	 * does not have an id, will return an empty list. list is ordered by version
	 * create date
	 * 
	 */
	public List<AssignmentSubmissionVersion> getVersionHistoryForSubmission(AssignmentSubmission submission);

	/**
	 * 
	 * @param userId
	 * @param submittedTime
	 * @return
	 */
	public FeedbackVersion getFeedbackByUserIdAndSubmittedTime(String userId, Date submittedTime);

	/**
	 * Update the feedback aspects of a submission version. The submission version is expected to
	 * exist prior to using this method.
	 * 
	 * @param feedback
	 */
	public void updateFeedbackForVersion(FeedbackVersion feedback);
	
	/**
	 * 
	 * @param studentId
	 * @param assignmentId
	 * @return the total number of versions that the given student has submitted
	 * for the given assignment. does not count draft versions
	 */
	public int getNumSubmittedVersions(String studentId, Long assignmentId);
}