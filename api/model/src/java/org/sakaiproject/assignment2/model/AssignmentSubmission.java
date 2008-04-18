/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/api/model/src/java/org/sakaiproject/assignment2/model/AssignmentSubmission.java $
 * $Id: AssignmentSubmission.java 12544 2006-05-03 15:06:26Z wagnermr@iupui.edu $
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

package org.sakaiproject.assignment2.model;

import java.util.Set;
import java.util.Date;
import java.util.HashSet;

import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.entity.api.Entity;

/**
 * The AssignmentSubmission object
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class AssignmentSubmission {
	
	private Long id;
	private Assignment2 assignment;
	private String userId;
	private Date resubmitCloseTime;
	private Integer numSubmissionsAllowed;
	private Set<AssignmentSubmissionVersion> submissionHistorySet;
	private int revisionVersion;
	
	// non-persisted fields 
	
	// fields populated with gradebook data
	private String gradebookGrade;
	private String gradebookComment;
	private boolean gradebookGradeReleased;
	
	// the current submission version must be populated manually b/c we want
	// to retrieve the version rec with the highest id
	private AssignmentSubmissionVersion currentSubmissionVersion;
	
	private String submissionStatus;
	private Integer submissionStatusConstant;

	public AssignmentSubmission() {
	}
	
	public AssignmentSubmission(Assignment2 assignment, String userId) {
		this.assignment = assignment;
		this.userId = userId;
	}
	
	/**
	 * 
	 * @return assignment submission id
	 */
	public Long getId() {
		return id;
	}
	
	/**
	 * set assignment submission id
	 * @param id
	 */
	public void setId(Long id) {
		this.id = id;
	}
	
	/**
	 * 
	 * @return	the parent assignment
	 */
	public Assignment2 getAssignment() {
		return assignment;
	}

	/**
	 * set the parent assignment
	 * @param assignment
	 */
	public void setAssignment(Assignment2 assignment) {
		this.assignment = assignment;
	}
	
	/**
	 * 
	 * @return user id associated with this submission
	 */
	public String getUserId() {
		return userId;
	}
	
	/**
	 * set the user id associated with this submission
	 * @param userId
	 */
	public void setUserId(String userId) {
		this.userId = userId;
	}
	
	/**
	 * 
	 * @return a set of AssignmentSubmissionVersion recs that represent
	 * the submission history for this user
	 */
	public Set<AssignmentSubmissionVersion> getSubmissionHistorySet() {
		return submissionHistorySet;
	}

	/**
	 * the set of AssignmentSubmissionVersion recs that represent
	 * the submission history for this user
	 * @param submissionHistorySet
	 */
	public void setSubmissionHistorySet(Set<AssignmentSubmissionVersion> submissionHistorySet) {
		this.submissionHistorySet = submissionHistorySet;
	}
	
	/**
	 * 
	 * @return time after which the submitter may no longer submit this assignment
	 */
	public Date getResubmitCloseTime() {
		return resubmitCloseTime;
	}

	/**
	 * set the time after which no more submissions will be accepted
	 * @param resubmitCloseTime
	 */
	public void setResubmitCloseTime(Date resubmitCloseTime) {
		this.resubmitCloseTime = resubmitCloseTime;
	}
	
	/**
	 * 
	 * @return the number of submissions allowed for this assignment. if -1,
     * unlimited submissions.
	 */
	public Integer getNumSubmissionsAllowed() {
		return numSubmissionsAllowed;
	}

	/**
	 * the number of submissions allowed for this assignment. if -1,
     * unlimited submissions.
	 * @param numResubmissionsAllowed
	 */
	public void setNumSubmissionsAllowed(Integer numSubmissionsAllowed) {
		this.numSubmissionsAllowed = numSubmissionsAllowed;
	}
	
	/**
	 * the int value of the version number for this assignment. not
     * to be confused with submission version.
	 * @return
	 */
	public int getRevisionVersion() {
		return revisionVersion;
	}

	/**
	 * the int value of the version number for this assignment. not
     * to be confused with submission version.
	 * @param revisionVersion
	 */
	public void setRevisionVersion(int revisionVersion) {
		this.revisionVersion = revisionVersion;
	}

	// non-persisted fields
	
	// fields populated with data from the gradebook
	
	/**
	 * 
	 * @return the grade for this submission from the associated gb item. This
	 * grade will be returned in converted form according to the gradebook's
	 * grade entry type (ie letter grade, percentage, etc)
	 */
	public String getGradebookGrade() {
		return gradebookGrade;
	}
	
	/**
	 * set the grade for this submission to be stored in the associated gb item.
	 * This grade must be in the correct form according to the gradebook's
	 * grade entry type (ie letter grade, percentage, etc)
	 * @param gradebookGrade
	 */
	public void setGradebookGrade(String gradebookGrade) {
		this.gradebookGrade = gradebookGrade;
	}
	
	/**
	 * 
	 * @return the gradebook comment from the associated gb item for this student
	 */
	public String getGradebookComment() {
		return gradebookComment;
	}
	
	/**
	 * set the comment to be stored in the gradebook
	 * @param gradebookComment
	 */
	public void setGradebookComment(String gradebookComment) {
		this.gradebookComment = gradebookComment;
	}
	
	/**
	 * 
	 * @return true if this grade has been released to the student
	 */
	public boolean isGradebookGradeReleased() {
		return gradebookGradeReleased;
	}

	/**
	 * true if this grade has been released to the student
	 * @param gradebookGradeReleased
	 */
	public void setGradebookGradeReleased(boolean gradebookGradeReleased) {
		this.gradebookGradeReleased = gradebookGradeReleased;
	}
	
	// not persisted but convenient here for UI
	/**
	 * <b>Note</b> This is not a persisted field but must be handled specially
	 * when you want to retrieve or update this information
	 * @return The current AssignmentSubmissionVersion for this submission. Each
	 * modification to the submission will result in a new AssignmentSubmissionVersion
	 * record so we maintain a history.
	 */
	public AssignmentSubmissionVersion getCurrentSubmissionVersion() {
		return currentSubmissionVersion;
	}

	/**
	 * <b>Note</b> This is not a persisted field but must be handled specially
	 * when you want to retrieve or update this information
	 * 
	 * Set the current AssignmentSubmissionVersion for this submission. Each
	 * modification to the submission will result in a new AssignmentSubmissionVersion
	 * record so we maintain a history.
	 * @param currentSubmissionVersion
	 */
	public void setCurrentSubmissionVersion(AssignmentSubmissionVersion currentSubmissionVersion) {
		this.currentSubmissionVersion = currentSubmissionVersion;
	}

	/**
	 * NOT-PERSISTED - used for UI
	 * @return String representation of this student's most current submission
	 * status ie Submitted, In Progress, etc
	 */
	public String getSubmissionStatus() {
		return submissionStatus;
	}

	/**
	 * NOT-PERSISTED - used for UI
	 * String representation of this student's most current submission
	 * status ie Submitted, In Progress, etc
	 * @param submissionStatus
	 */
	public void setSubmissionStatus(String submissionStatus) {
		this.submissionStatus = submissionStatus;
	}
	
	/**
	 * NOT-PERSISTED - used for UI
	 * @return constant representation of this submission's status
	 * ie Not Started, In Progess, etc
	 */
	public Integer getSubmissionStatusConstant()
	{
		return submissionStatusConstant;
	}

	/**
	 * NOT-PERSISTED - used for UI
	 * constant representation of this submission's status
	 * ie Not Started, In Progess, etc
	 * @param submissionStatusConstant
	 */
	public void setSubmissionStatusConstant(Integer submissionStatusConstant)
	{
		this.submissionStatusConstant = submissionStatusConstant;
	}

	/**
	 * 
	 * @param subToCopy
	 * @param includeHistorySet - if true, will do a deep copy of the versions in the history set
	 * 			make sure this is false if you haven't initialized the historySet
	 * @param includeSubAttachSet - if true, will do a deep copy of the submissionAttachmentSet for
	 * 			currentVersion and history versions
	 * 			make sure this is false if you haven't initialized the submissionAttachmentSet
	 * @param includeFBAttachSet - if true, will do a deep copy of the feedbackAttachmentSet for
	 * 			currentVersion and historySet versions
	 * 			make sure this is false if you haven't initialized the feedbackAttachmentSet
	 * @return a copy of the given submission. does not copy non-persisted 
	 * fields except for the currentVersion. assignment is shallow copy. this is used largely
	 * b/c hsqldb doesn't like it when we change some fields without saving
	 */
	public static AssignmentSubmission deepCopy(AssignmentSubmission subToCopy, boolean includeHistorySet,
			boolean includeSubAttachSet, boolean includeFBAttachSet) {
		AssignmentSubmission submission = new AssignmentSubmission();
		submission.setId(subToCopy.getId()); 
		submission.setAssignment(subToCopy.getAssignment());
		submission.setNumSubmissionsAllowed(subToCopy.getNumSubmissionsAllowed());
		submission.setResubmitCloseTime(subToCopy.getResubmitCloseTime());
		submission.setUserId(subToCopy.getUserId());
		submission.setAssignment(subToCopy.assignment);
		submission.setCurrentSubmissionVersion(subToCopy.currentSubmissionVersion);
		submission.setNumSubmissionsAllowed(subToCopy.numSubmissionsAllowed);
		submission.setResubmitCloseTime(subToCopy.resubmitCloseTime);
		submission.setUserId(subToCopy.userId);
		
		if (subToCopy.getCurrentSubmissionVersion() != null) {
			AssignmentSubmissionVersion currVersion = 
				AssignmentSubmissionVersion.deepCopy(subToCopy.getCurrentSubmissionVersion(),
						includeSubAttachSet, includeFBAttachSet);
			submission.setCurrentSubmissionVersion(currVersion);
		}
		
		if (includeHistorySet) {
			Set<AssignmentSubmissionVersion> copiedHistory = new HashSet<AssignmentSubmissionVersion>();
			if (subToCopy.getSubmissionHistorySet() != null && !subToCopy.getSubmissionHistorySet().isEmpty()) {
				for (AssignmentSubmissionVersion version : subToCopy.getSubmissionHistorySet()) {
					AssignmentSubmissionVersion copiedVersion = AssignmentSubmissionVersion.deepCopy(version,
							includeSubAttachSet, includeFBAttachSet);
					copiedHistory.add(copiedVersion);
				}
			}
			submission.setSubmissionHistorySet(copiedHistory);
		}
		
		return submission;
	}
	
	public String getReference()
	{
		StringBuilder sb = new StringBuilder();
		sb.append(AssignmentConstants.REFERENCE_ROOT);
		sb.append(Entity.SEPARATOR);
		sb.append(AssignmentConstants.SUBMISSION_TYPE);
		sb.append(Entity.SEPARATOR);
		sb.append(getAssignment().getContextId());
		sb.append(Entity.SEPARATOR);
		sb.append(Long.toString(id));
		return sb.toString();
	}

}
