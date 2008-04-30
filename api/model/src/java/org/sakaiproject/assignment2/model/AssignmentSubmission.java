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

package org.sakaiproject.assignment2.model;

import java.util.Date;
import java.util.Set;

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
	
	/**
	 * the current submission version must be populated manually b/c we want
	 * to retrieve the most current version
	 */
	private AssignmentSubmissionVersion currentSubmissionVersion;

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
	
	// not persisted but convenient to have
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
