package org.sakaiproject.assignment2.logic;

import java.util.List;

import org.sakaiproject.util.StringUtil;

public class UploadException extends Exception
{
	public UploadException(List<String> msgs)
	{
		super(StringUtil.unsplit(msgs.toArray(new String[msgs.size()]), ","));
	}

	public UploadException(String key, String msg)
	{
		super("[" + key + "] " + msg);
	}

	public UploadException(String msg, Throwable t)
	{
		super(msg, t);
	}

	public UploadException(String key, String msg, Throwable t)
	{
		super("[" + key + "] " + msg, t);
	}
}