package org.sakaiproject.assignment2.model;

public class UploadAllOptions
{
	public Long assignmentId;
	public boolean gradeFile;
	public boolean feedbackText;
	public boolean feedbackAttachments;
	public boolean comments;

	public UploadAllOptions()
	{
	}

	public UploadAllOptions(Long assignmentId, boolean gradeFile, boolean feedbackText,
			boolean feedbackAttachments, boolean comments)
	{
		this.assignmentId = assignmentId;
		this.gradeFile = gradeFile;
		this.feedbackText = feedbackText;
		this.feedbackAttachments = feedbackAttachments;
		this.comments = comments;
	}
}