package org.sakaiproject.assignment2.logic;

import java.io.File;

import org.sakaiproject.assignment2.exception.UploadException;
import org.sakaiproject.assignment2.model.UploadAllOptions;

public interface UploadAllLogic
{
	/**
	 * Process uploaded zip file.
	 * 
	 * @param options
	 * @param file
	 */
	void uploadAll(UploadAllOptions options, File file)
			throws UploadException;

	/**
	 * Process uploaded csv file.
	 */
	void uploadCSV(UploadAllOptions options, File file)
			throws UploadException;
}