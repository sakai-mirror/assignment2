package org.sakaiproject.assignment2.logic;

public class UploadException extends Exception
{
	public UploadException(String msg)
	{
		super(msg);
	}
	
	public UploadException(String msg, Throwable t)
	{
		super(msg, t);
	}
}