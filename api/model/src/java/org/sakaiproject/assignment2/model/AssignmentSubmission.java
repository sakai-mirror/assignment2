/**********************************************************************************
 * $URL: https://source.sakaiproject.org/svn/syllabus/trunk/syllabus-api/src/java/org/sakaiproject/api/app/syllabus/SyllabusItem.java $
 * $Id: SyllabusItem.java 8802 2006-05-03 15:06:26Z cwen@iupui.edu $
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
 * The AssignmentSubmission object
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class AssignmentSubmission {
	
	private String submissionId;
	private String assignmentId;
	private String userId;
	private String groupId;
	private Date submittedTime;
	private String feedbackText;
	private boolean pledgeFlag;
	private String submittedText;
	private boolean draft;
	private boolean allowResubmit;
	private Date resubmitCloseTime;
	private String reviewReportUrl;
	private int reviewReportScore;
	private String reviewStatus;
	private String reviewIconUrl;
	
	public AssignmentSubmission() {
	}
	
	/**
	 * 
	 * @return assignment submission id
	 */
	public String getSubmissionId() {
		return submissionId;
	}
	
	/**
	 * set assignment submission id
	 * @param submissionId
	 */
	public void setSubmissionId(String submissionId) {
		this.submissionId = submissionId;
	}
	
	/**
	 * 
	 * @return	assignment id for the parent assignment
	 */
	public String getAssignmentId() {
		return assignmentId;
	}

	/**
	 * set the assignment id for the parent assignment
	 * @param assignmentId
	 */
	public void setAssignmentId(String assignmentId) {
		this.assignmentId = assignmentId;
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
	 * Instead of requiring each student to submit, this will be populated
	 * to indicate there will be a single group submission. If null,
	 * it is an individual student submission.
	 * @return group id associated with this submission
	 */
	public String getGroupId() {
		return groupId;
	}

	/**
	 * Set the group id indicating that it is a group submission. If null,
	 * it is an individual student submission.
	 * @param groupId
	 */
	public void setGroupId(String groupId) {
		this.groupId = groupId;
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
	 * @return true if the submitter agreed to the honor pledge requirement
	 */
	public boolean getPledgeFlag() {
		return pledgeFlag;
	}
	
	/**
	 * Set the submitter's response to the honor pledge requirement
	 * @param pledgeFlag
	 */
	public void setPledgeFlag(boolean pledgeFlag) {
		this.pledgeFlag = pledgeFlag;
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
	public boolean getDraft() {
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
	 * @return true if the submitter is allowed to resubmit this assignment
	 */
	public boolean getAllowResubmit() {
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

}
