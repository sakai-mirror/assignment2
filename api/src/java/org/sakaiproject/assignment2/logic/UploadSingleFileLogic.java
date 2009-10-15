package org.sakaiproject.assignment2.logic;

import java.io.File;
import java.util.List;
import java.util.Map;

import org.sakaiproject.assignment2.exception.UploadException;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.springframework.web.multipart.MultipartFile;

public interface UploadSingleFileLogic
{
    /**
     * 
     * Values that indicate some information related to a user's upload that
     * might be useful to return
     *
     */
    public enum UploadSingleFileInfo {
        /**
         * nothing being uploaded
         */
    	EMPTY_FILE,
    	/**
    	 * upload file too big
    	 */
    	EXCEED_MAX_UPLOAD_FILE_SIZE,
    	/**
    	 * empty file title for uploaded file
    	 */
    	EMPTY_FILE_TITLE,
    	/**
    	 * don't have permission to add attachment
    	 */
    	NO_PERMISSION_TO_ADD_ATTACHMENT,
    	/**
    	 * file name is too long that cannot be used as resource id afterwards
    	 */
    	FILE_NAME_TOO_LONG,
    	RUNTIME_EXCEPTION,
    	IO_EXCEPTION,
    	GENERAL_EXCEPTION
    	
    }

    /**
     * used to derive the String representation of the {@link UploadInfo}
     * from the map returned by {@link #uploadAll(UploadAllOptions, File)}
     */
    public static final String UPLOAD_SINGLE_FILE_INFO = "info";
    /**
     * used to derive the parameter value in the map returned by
     * {@link #uploadAll(UploadAllOptions, File)}. for instance, may be
     * the problematic folder name when {@link UploadInfo#UNABLE_TO_EXTRACT_USERNAME} 
     * is encountered
     */
    public static final String UPLOAD_SINGLE_FILE_PARAM = "param";

    /**
     * Upload single file
     * @param assignment
     * @param assignmentSubmission
     * @param studentSubmissionPreviewVersion
     * @param asv
     * @param uploads
     * @return
     * @throws UploadException
     */
    public List<String> uploadSingleFile(Assignment2 assignment, AssignmentSubmission assignmentSubmission, AssignmentSubmissionVersion studentSubmissionPreviewVersion, AssignmentSubmissionVersion asv, Map<String, MultipartFile> uploads) throws UploadException;
}