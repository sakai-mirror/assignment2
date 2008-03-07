package org.sakaiproject.assignment2.model;

import java.util.Date;
import java.util.Set;

public interface FeedbackVersion
{
	Long getId();

	String getAnnotatedText();

	String getAnnotatedTextFormatted();

	String getLastFeedbackSubmittedBy();

	Date getLastFeedbackTime();

	Set<FeedbackAttachment> getFeedbackAttachSet();

	String getFeedbackNotes();

	void setAnnotatedText(String annotatedText);

	void setLastFeedbackSubmittedBy(String feedbackSubmittedBy);

	void setLastFeedbackTime(Date feebackTime);

	void setFeedbackAttachSet(Set<FeedbackAttachment> feedbackAttachSet);

	void setFeedbackNotes(String feedbackNotes);
}