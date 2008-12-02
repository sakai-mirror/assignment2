package org.sakaiproject.assignment2.logic;

import java.io.OutputStream;

public interface ZipExportLogic
{
	/**
	 * Zip up the submissions for the given assignment.  Will only include
	 * submissions that the current user is allowed to view or grade. If
	 * assignment is graded, includes a csv file containing the grade information.
	 * Each student has a folder that contains every submission version. Folders
	 * only exist for students who have submission data. Only gradable students
	 * are include in the grades csv file.
	 * @param outputStream
	 * @param assignmentId
	 * @throws SecurityException if current user is not authorized to access
	 * the given assignment from a grading perspective
	 */
    void getSubmissionsZip(OutputStream outputStream, Long assignmentId);
}