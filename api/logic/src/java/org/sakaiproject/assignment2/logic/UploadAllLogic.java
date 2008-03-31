package org.sakaiproject.assignment2.logic;

import java.util.zip.ZipFile;

import org.sakaiproject.assignment2.model.UploadAllOptions;

public interface UploadAllLogic
{
	/**
	 * Process uploaded zip file.
	 * 
	 * @param assignmentId
	 * @param options
	 * @param fileFromUpload
	 */
	void uploadAll(UploadAllOptions options, ZipFile zipFile)
			throws UploadException;

}