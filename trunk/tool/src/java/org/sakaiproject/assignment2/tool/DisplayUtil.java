package org.sakaiproject.assignment2.tool;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;

/**
 * A central location for some display formatting, since some of display 
 * information will need to also come from EB feeds for newer javascript UI 
 * work.
 * 
 * @author sgithens
 *
 */
public class DisplayUtil {

    //private MessageLocator messageLocator;
    //public void setMessageLocator(MessageLocator messageLocator) {
    //    this.messageLocator = messageLocator;
    // }

    /**
     * map key used by {@link #getSubmissionStatusForAssignment(Assignment2, List)}.
     * the number of submissions that exist for the given assignment and students.
     * if no submission required, set to "N/A"
     */
    public static final String NUM_SUB = "numSub";
    /**
     * map key used by {@link #getSubmissionStatusForAssignment(Assignment2, List)}.
     * the number of "new" submissions that exist for the given assignment and
     * students. if no submission required, set to "N/A"
     */
    public static final String NUM_NEW_SUB = "numNewSub";

    /**
     * map key used by {@link #getSubmissionStatusForAssignment(Assignment2, List)}.
     * The In/New display for the UI: total submitted / new submissions.
     * if no submission required, set to "N/A"
     */
    public static final String IN_NEW_DISPLAY = "inNewDisplay";

    private AssignmentSubmissionLogic submissionLogic;
    public void setSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
        this.submissionLogic = submissionLogic;
    }

    /**
     * 
     * @param assignment
     * @param viewableStudents
     * @return a map of a submission property ({@link #NUM_SUB},{@link #NUM_NEW_SUB},{@link #IN_NEW_DISPLAY}) to
     * its value for the given assignment and student list. if assignment does not
     * accept submissions, sets all properties to "N/A"
     */
    public Map<String, String> getSubmissionStatusForAssignment(Assignment2 assignment, List<String> viewableStudents) {
        Map<String, String> subStatusMap = new HashMap<String, String>();

        if (assignment.isRequiresSubmission() && 
                assignment.getSubmissionType() != AssignmentConstants.SUBMIT_NON_ELECTRONIC) {
            // In/New display
            int submitted = 0;
            int newSubmissions = 0;

            if (viewableStudents != null && !viewableStudents.isEmpty()) {
                submitted = submissionLogic.getNumStudentsWithASubmission(assignment, viewableStudents);
                if (submitted > 0) {
                    newSubmissions = submissionLogic.getNumNewSubmissions(assignment, viewableStudents);
                }
            }

            subStatusMap.put(NUM_SUB, submitted + "");
            subStatusMap.put(NUM_NEW_SUB, newSubmissions + "");
            subStatusMap.put(IN_NEW_DISPLAY, submitted + "/" + newSubmissions); // messageLocator.getMessage("assignment2.list.submissions_link", new Object[]{ submitted, newSubmissions});
        } else {
            String notApplicable = "N/A"; //messageLocator.getMessage("assignment2.list.no_sub_required");
            subStatusMap.put(NUM_SUB, notApplicable);
            subStatusMap.put(NUM_NEW_SUB, notApplicable);
            subStatusMap.put(IN_NEW_DISPLAY, notApplicable);  
        }

        return subStatusMap;
    }
    
    /**
     * Given the score in the {@link AssignmentConstants#PROP_REVIEW_SCORE} property,
     * returns the appropriate style class for displaying this score. Used only
     * if content review is enabled
     * @param score
     * @return
     */
    public static String getCssClassForReviewScore(Integer score) {
        String cssClass = "reportStatus4";
        if (score != null) {
            if (score == 0) {
                cssClass = "reportStatus0";
            } else if (score < 25) {
                cssClass = "reportStatus1";
            } else if (score < 50) {
                cssClass = "reportStatus2";
            } else if (score < 75) {
                cssClass = "reportStatus3";
            } else {
                cssClass = "reportStatus4";
            }
        }

        return cssClass;
    }
}
