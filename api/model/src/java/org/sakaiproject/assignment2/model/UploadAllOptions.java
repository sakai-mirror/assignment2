package org.sakaiproject.assignment2.model;

public class UploadAllOptions
{
	private boolean submissionText;
	private boolean submissionAttachments;
	private boolean gradeFile;
	private boolean feedbackText;
	private boolean feedbackAttachments;
	private boolean releaseGrades;
	private boolean comments;

	public UploadAllOptions()
	{
	}

	public UploadAllOptions(boolean submissionText, boolean submissionAttachments,
			boolean gradeFile, boolean feedbackText, boolean feedbackAttachments,
			boolean releaseGrades, boolean comments)
	{
		this.submissionText = submissionText;
		this.submissionAttachments = submissionAttachments;
		this.gradeFile = gradeFile;
		this.feedbackText = feedbackText;
		this.feedbackAttachments = feedbackAttachments;
		this.releaseGrades = releaseGrades;
		this.comments = comments;
	}

	public boolean isSubmissionText()
	{
		return submissionText;
	}

	public void setSubmissionText(boolean submissionText)
	{
		this.submissionText = submissionText;
	}

	public boolean isSubmissionAttachments()
	{
		return submissionAttachments;
	}

	public void setSubmissionAttachments(boolean submissionAttachments)
	{
		this.submissionAttachments = submissionAttachments;
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

	public boolean isReleaseGrades()
	{
		return releaseGrades;
	}

	public void setReleaseGrades(boolean releaseGrades)
	{
		this.releaseGrades = releaseGrades;
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