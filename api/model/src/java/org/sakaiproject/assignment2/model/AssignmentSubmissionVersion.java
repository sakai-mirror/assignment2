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

import java.util.Date;

/**
 * The AssignmentSubmissionVersion object
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class AssignmentSubmissionVersion {
	
	private Long submissionVersionId;
	private AssignmentSubmission assignmentSubmission;
	private Date submittedTime;
	private Date returnedTime;
	private String feedbackText;
	private String commentForUngraded;
	private String submittedText;
	private boolean draft;
	private boolean newSubmission;
	private boolean allowResubmit;
	private Date resubmitCloseTime;
	private String reviewReportUrl;
	private int reviewReportScore;
	private String reviewStatus;
	private String reviewIconUrl;
	private String createdBy;
	private Date createdTime;
	

	public AssignmentSubmissionVersion() {
	}
	
	public Long getSubmissionVersionId() {
		return submissionVersionId;
	}

	public void setSubmissionVersionId(Long submissionVersionId) {
		this.submissionVersionId = submissionVersionId;
	}
	
	/**
	 * @return time this assignment was submitted. If null, assignment has not
	 * been submitted for this user.
	 */
	public Date getSubmittedTime() {
		return submittedTime;
	}
	
	/**
	 * Set the time the assignment was submitted. Null if no submission yet.
	 * @param submittedTime
	 */
	public void setSubmittedTime(Date submittedTime) {
		this.submittedTime = submittedTime;
	}
	
	/**
	 * 
	 * @return text composed of the submission with grader-added annotation
	 */
	public String getFeedbackText() {
		return feedbackText;
	}

	/**
	 * set the feedback text
	 * @param feedbackText
	 */
	public void setFeedbackText(String feedbackText) {
		this.feedbackText = feedbackText;
	}
	
	/**
	 * 
	 * @return the text of the submission
	 */
	public String getSubmittedText() {
		return submittedText;
	}

	/**
	 * set the text of the submission
	 * @param submittedText
	 */
	public void setSubmittedText(String submittedText) {
		this.submittedText = submittedText;
	}
	
	/**
	 * 
	 * @return true if the submitter has started working on the submission
	 * but has not yet submitted it for review
	 */
	public boolean isDraft() {
		return draft;
	}

	/**
	 * set the draft status
	 * @param draft
	 */
	public void setDraft(boolean draft) {
		this.draft = draft;
	}
	
	/**
	 * 
	 * @return true if no grader action has been taken on this submission 
	 */
	public boolean isNewSubmission() {
		return newSubmission;
	}
	
	/**
	 * true if no grader action has been taken on this submission
	 * @param newSubmission
	 */
	public void setNewSubmission(boolean newSubmission) {
		this.newSubmission = newSubmission;
	}
	
	/**
	 * 
	 * @return true if the submitter is allowed to resubmit this assignment
	 */
	public boolean isAllowResubmit() {
		return allowResubmit;
	}

	/**
	 * set whether or not the submitter is allowed to resubmit this assignment
	 * @param allowResubmit
	 */
	public void setAllowResubmit(boolean allowResubmit) {
		this.allowResubmit = allowResubmit;
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
	 * @return the URL of the content review report (ie from turnitin)
	 */
	public String getReviewReportUrl() {
		return reviewReportUrl;
	}

	/**
	 * set the URL of the content review report (ie from turnitin)
	 * @param reviewReportUrl
	 */
	public void setReviewReportUrl(String reviewReportUrl) {
		this.reviewReportUrl = reviewReportUrl;
	}
	
	/**
	 * 
	 * @return the score from the content review service (ie from turnitin)
	 */
	public int getReviewReportScore() {
		return reviewReportScore;
	}

	/**
	 * set the score from the content review service (ie from turnitin)
	 * @param reviewReportScore
	 */
	public void setReviewReportScore(int reviewReportScore) {
		this.reviewReportScore = reviewReportScore;
	}
	
	/**
	 * 
	 * @return the status of the content review (ie from turnitin)
	 */
	public String getReviewStatus() {
		return reviewStatus;
	}

	/**
	 * set the status of the content review (ie from turnitin)
	 * @param reviewStatus
	 */
	public void setReviewStatus(String reviewStatus) {
		this.reviewStatus = reviewStatus;
	}
	
	/**
	 * 
	 * @return the URL of the content review icon associated with 
	 * this submission (ie from turnitin)
	 */
	public String getReviewIconUrl() {
		return reviewIconUrl;
	}

	/**
	 * set the URL of the content review icon associated with this 
	 * submission (ie from turnitin)
	 * @param reviewIconUrl
	 */
	public void setReviewIconUrl(String reviewIconUrl) {
		this.reviewIconUrl = reviewIconUrl;
	}

	/**
	 * Ungraded assignments will not be linked to the gb, so this field
	 * will be used in lieu of the gb comment
	 * @return comment
	 */
	public String getCommentForUngraded() {
		return commentForUngraded;
	}

	/**
	 * Ungraded assignments will not be linked to the gb, so this field
	 * will be used in lieu of the gb comment
	 * @param commentForUngraded
	 */
	public void setCommentForUngraded(String commentForUngraded) {
		this.commentForUngraded = commentForUngraded;
	}

	/**
	 * 
	 * @return Date this submission was returned to the submitter
	 */
	public Date getReturnedTime() {
		return returnedTime;
	}

	/**
	 * set the Date this submission was returned to the submitter
	 * @param returnedTime
	 */
	public void setReturnedTime(Date returnedTime) {
		this.returnedTime = returnedTime;
	}

	/**
	 * 
	 * @return the userId of the person who made this modification to
	 * the submission
	 */
	public String getCreatedBy() {
		return createdBy;
	}

	/**
	 * set the userId of the person who made this modification to
	 * the submission
	 * @param createdBy
	 */
	public void setCreatedBy(String createdBy) {
		this.createdBy = createdBy;
	}

	/**
	 * 
	 * @return the time this version was created
	 */
	public Date getCreatedTime() {
		return createdTime;
	}

	/**
	 * set the time this version was created
	 * @param createdTime
	 */
	public void setCreatedTime(Date createdTime) {
		this.createdTime = createdTime;
	}

	/**
	 * 
	 * @return the parent submission record associated with this version
	 */
	public AssignmentSubmission getAssignmentSubmission() {
		return assignmentSubmission;
	}

	/**
	 * set the parent submission record associated with this version
	 * @param assignmentSubmission
	 */
	public void setAssignmentSubmission(AssignmentSubmission assignmentSubmission) {
		this.assignmentSubmission = assignmentSubmission;
	}

}
