package org.sakaiproject.assignment2.model;

public class UploadAllOptions
{
	public UploadAllOptions(boolean submissionText, boolean submissionAttachments,
			boolean gradeFile, boolean feedbackText, boolean feedbackAttachments, boolean release){
		this.submissionText = submissionText;
		this.submissionAttachments = submissionAttachments;
		this.gradeFile = gradeFile;
		this.feedbackText = feedbackText;
		this.feedbackAttachments = feedbackAttachments;
		this.release = release;
	}
	
	public UploadAllOptions(){
		this.submissionText = false;
		this.submissionAttachments = false;
		this.gradeFile = false;
		this.feedbackText = false;
		this.feedbackAttachments = false;
		this.release = false;
	}
	
	private boolean submissionText;
	private boolean submissionAttachments;
	private boolean gradeFile;
	private boolean feedbackText;
	private boolean feedbackAttachments;
	private boolean release;

	public boolean getSubmissionText()
	{
		return submissionText;
	}

	public void setSubmissionText(boolean submissionText)
	{
		this.submissionText = submissionText;
	}

	public boolean getSubmissionAttachments()
	{
		return submissionAttachments;
	}

	public void setSubmissionAttachments(boolean submissionAttachments)
	{
		this.submissionAttachments = submissionAttachments;
	}

	public boolean getGradeFile()
	{
		return gradeFile;
	}

	public void setGradeFile(boolean gradeFile)
	{
		this.gradeFile = gradeFile;
	}

	public boolean getFeedbackText()
	{
		return feedbackText;
	}

	public void setFeedbackText(boolean feedbackText)
	{
		this.feedbackText = feedbackText;
	}

	public boolean getFeedbackAttachments()
	{
		return feedbackAttachments;
	}

	public void setFeedbackAttachments(boolean feedbackAttachments)
	{
		this.feedbackAttachments = feedbackAttachments;
	}

	public boolean getRelease()
	{
		return release;
	}

	public void setRelease(boolean release)
	{
		this.release = release;
	}
}
