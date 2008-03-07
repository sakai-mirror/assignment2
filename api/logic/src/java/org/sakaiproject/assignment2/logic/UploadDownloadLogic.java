package org.sakaiproject.assignment2.logic;

import java.util.zip.ZipFile;

import org.sakaiproject.assignment2.model.UploadAllOptions;

public interface UploadDownloadLogic
{

	/**
	 * Process uploaded zip file.
	 * 
	 * @param assignmentId
	 * @param options
	 * @param fileFromUpload
	 */
	public abstract void uploadAll(Long assignmentId, UploadAllOptions options, ZipFile zipFile)
			throws UploadException;

}