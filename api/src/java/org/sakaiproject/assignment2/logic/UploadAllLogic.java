package org.sakaiproject.assignment2.logic;

import java.io.File;
import java.util.List;
import java.util.Map;

import org.sakaiproject.assignment2.exception.UploadException;
import org.sakaiproject.assignment2.model.UploadAllOptions;

public interface UploadAllLogic
{
    /**
     * 
     * Values that indicate some information related to a user's upload that
     * might be useful to return
     *
     */
    public enum UploadInfo {
        /**
         * Unable to extract the student's username from the submission folder
         */
        UNABLE_TO_EXTRACT_USERNAME,
        /**
         * Unable to extract the submission version id from the version folder for a student
         */
        UNABLE_TO_EXTRACT_VERSION,
        /**
         * An error occurred while trying to add a feedback attachment
         */
        FEEDBACK_ATTACHMENT_ERROR,
        /**
         * An error occurred trying to extract the content of the feedback notes
         * file
         */
        FEEDBACK_FILE_UPLOAD_ERROR,
        /**
         * An error occurred trying to extract the content of the annotated
         * text file
         */
        ANNOTATED_TEXT_UPLOAD_ERROR,
        /**
         * The current user does not have permission to grade and/or provide
         * feedback for the given student so that student was not processed
         * in the upload
         */
        NO_GRADING_PERM_FOR_STUDENT,
        /**
         * The total num students with updated feedback 
         */
        NUM_STUDENTS_UPDATED,

        /**
         * The total num students identified for update from the archive.
         * each student will only be updated if there are actual changes
         * to their existing feedback
         */
        NUM_STUDENTS_IDENTIFIED_FOR_UPDATE,
        /**
         * A student username in the grades csv file is not a member of the site
         */
        INVALID_STUDENT_IN_CSV,
        /**
         * A student's grade in the grades csv file is invalid
         */
        STUDENT_WITH_INVALID_GRADE_IN_CSV
    }

    /**
     * used to derive the String representation of the {@link UploadInfo}
     * from the map returned by {@link #uploadAll(UploadAllOptions, File)}
     */
    public static final String UPLOAD_INFO = "info";
    /**
     * used to derive the parameter value in the map returned by
     * {@link #uploadAll(UploadAllOptions, File)}. for instance, may be
     * the problematic folder name when {@link UploadInfo#UNABLE_TO_EXTRACT_USERNAME} 
     * is encountered
     */
    public static final String UPLOAD_PARAM = "param";

    /**
     * 
     * @param options
     * @param file
     * @return uploads the feedback derived from the given zip file. if errors are
     * encountered along the way, these errors will be added to a List of Maps. The maps
     * may also include informational pieces like # students identified for update.  The maps
     * consist of two keys: {@link #UPLOAD_INFO} and {{@link #UPLOAD_PARAM}. This
     * will allow you to construct meaningful (info or error) messages to display for your user.
     * The upload will attempt to continue, if possible, after an error is encountered.
     * The reason for the error should be returned in the map unless error is
     * so severe it causes failure
     * @throws UploadException an exception occurred that prevented the upload
     * from continuing
     */
    public List<Map<String, String>> uploadAll(UploadAllOptions options, File file)
    throws UploadException;

}