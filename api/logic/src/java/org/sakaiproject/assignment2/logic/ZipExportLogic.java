package org.sakaiproject.assignment2.logic;

import java.io.OutputStream;

import org.sakaiproject.exception.PermissionException;

public interface ZipExportLogic
{
	void getSubmissionsZip(OutputStream outputStream, Long assignmentId) throws PermissionException;
}