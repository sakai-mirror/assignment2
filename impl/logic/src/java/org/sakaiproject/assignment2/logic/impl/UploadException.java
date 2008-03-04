package org.sakaiproject.assignment2.logic.impl;

import java.util.ArrayList;
import java.util.List;

//import uk.org.ponder.messageutil.MessageLocator;

public class UploadException extends Exception
{
	List<String> messageKeys;

	public UploadException(List<String> messageKeys)
	{
		this.messageKeys = messageKeys;
	}

	public UploadException(String messageKey)
	{
		this.messageKeys = new ArrayList<String>();
		messageKeys.add(messageKey);
	}

	public List<String> getMessageKeys()
	{
		return messageKeys;
	}

//	public List<String> getPopulatedMessages(MessageLocator messageLocator)
//	{
//		return null;
//	}
}