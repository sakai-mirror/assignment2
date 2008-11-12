package org.sakaiproject.assignment2.tool.beans.locallogic;

/**
 * Because our workflow of POST forms goes between the Instructor 
 * and Student layer (such as an Instructor previewing what an asnn
 * would look like as a Student) we are centralizing all the possible
 * action returns here.
 * 
 * @author sgithens
 *
 */
public enum WorkFlowResult {
    STUDENT_SUBMIT_SUBMISSION,
    STUDENT_CONTINUE_EDITING_SUBMISSION,
    STUDENT_PREVIEW_SUBMISSION,
    STUDENT_SAVE_DRAFT_SUBMISSION,
    STUDENT_SUBMISSION_FAILURE,
    
    INSTRUCTOR_POST_ASSIGNMENT,
    INSTRUCTOR_PREVIEW_ASSIGNMENT,
    INSTRUCTOR_SAVE_DRAFT_ASSIGNMENT,
    INSTRUCTOR_CANCEL_ASSIGNMENT,
    INSTRUCTOR_ASSIGNMENT_FAILURE,
    INSTRUCTOR_CONTINUE_EDITING_ASSIGNMENT
}
