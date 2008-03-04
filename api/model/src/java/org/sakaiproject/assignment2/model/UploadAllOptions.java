package org.sakaiproject.assignment2.model;

public class UploadAllOptions
{
	private boolean gradeFile;
	private boolean feedbackText;
	private boolean feedbackAttachments;
	private boolean comments;

	public UploadAllOptions()
	{
	}

	public UploadAllOptions(boolean gradeFile, boolean feedbackText, boolean feedbackAttachments,
			boolean comments)
	{
		this.gradeFile = gradeFile;
		this.feedbackText = feedbackText;
		this.feedbackAttachments = feedbackAttachments;
		this.comments = comments;
	}

	public boolean isGradeFile()
	{
		return gradeFile;
	}

	public void setGradeFile(boolean gradeFile)
	{
		this.gradeFile = gradeFile;
	}

	public boolean isFeedbackText()
	{
		return feedbackText;
	}

	public void setFeedbackText(boolean feedbackText)
	{
		this.feedbackText = feedbackText;
	}

	public boolean isFeedbackAttachments()
	{
		return feedbackAttachments;
	}

	public void setFeedbackAttachments(boolean feedbackAttachments)
	{
		this.feedbackAttachments = feedbackAttachments;
	}

	public boolean isComments()
	{
		return comments;
	}

	public void setComments(boolean comments)
	{
		this.comments = comments;
	}
}