package org.sakaiproject.assignment2.tool.beans;

import org.sakaiproject.assignment2.model.AssignmentSubmission;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;

public class PreviewAssignmentSubmissionBean {

	private AssignmentSubmission assignmentSubmission;
	public AssignmentSubmission getAssignmentSubmission() {
		return this.assignmentSubmission;
	}
	public void setAssignmentSubmission(AssignmentSubmission assignmentSubmission) {
		this.assignmentSubmission = assignmentSubmission;
	}
	
	private String OTPKey = EntityBeanLocator.NEW_PREFIX + "1";
	public void setOTPKey(String key) {
		this.OTPKey = key;
	}
	public String getOTPKey() {
		return this.OTPKey;
	}
}
