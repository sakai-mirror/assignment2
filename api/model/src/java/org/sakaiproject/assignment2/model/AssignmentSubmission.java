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


/**
 * The AssignmentSubmission object
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class AssignmentSubmission {
	
	private Long submissionId;
	private Assignment2 assignment;
	private String userId;
	private AssignmentSubmissionVersion currentSubmissionVersion;

	public AssignmentSubmission() {
	}
	
	/**
	 * 
	 * @return assignment submission id
	 */
	public Long getSubmissionId() {
		return submissionId;
	}
	
	/**
	 * set assignment submission id
	 * @param submissionId
	 */
	public void setSubmissionId(Long submissionId) {
		this.submissionId = submissionId;
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
	 * @return The current AssignmentSubmissionVersion for this submission. Each
	 * modification to the submission will result in a new AssignmentSubmissionVersion
	 * record so we maintain a history.
	 */
	public AssignmentSubmissionVersion getCurrentSubmissionVersion() {
		return currentSubmissionVersion;
	}

	/**
	 * Set the current AssignmentSubmissionVersion for this submission. Each
	 * modification to the submission will result in a new AssignmentSubmissionVersion
	 * record so we maintain a history.
	 * @param currentSubmissionVersion
	 */
	public void setCurrentSubmissionVersion(AssignmentSubmissionVersion currentSubmissionVersion) {
		this.currentSubmissionVersion = currentSubmissionVersion;
	}
	
}
