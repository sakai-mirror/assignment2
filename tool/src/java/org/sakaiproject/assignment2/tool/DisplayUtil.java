package org.sakaiproject.assignment2.tool;

import java.util.List;

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
    
    private AssignmentSubmissionLogic submissionLogic;
    public void setSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
        this.submissionLogic = submissionLogic;
    }
    
    public String getSubmissionStatusForAssignment(Assignment2 assignment, List<String> viewableStudents) {
        String togo;

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

            togo = submitted + "/" + newSubmissions; // messageLocator.getMessage("assignment2.list.submissions_link", new Object[]{ submitted, newSubmissions});
        } else {
            togo = "N/A";  //messageLocator.getMessage("assignment2.list.no_sub_required");
        }
        
        return togo;
    }
}
