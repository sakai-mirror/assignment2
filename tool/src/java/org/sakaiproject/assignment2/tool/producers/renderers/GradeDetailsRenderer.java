package org.sakaiproject.assignment2.tool.producers.renderers;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.exception.GradebookItemNotFoundException;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.GradeInformation;
import org.sakaiproject.assignment2.logic.GradebookItem;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.user.api.User;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.decorators.UIFreeAttributeDecorator;
import uk.org.ponder.rsf.producers.BasicProducer;

public class GradeDetailsRenderer implements BasicProducer {
    private static Log log = LogFactory.getLog(GradeDetailsRenderer.class);

    // Dependency
    private User currentUser;
    public void setCurrentUser(User currentUser) {
        this.currentUser = currentUser;
    }

    // Dependency
    private MessageLocator messageLocator;
    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }

    // Dependency
    private ExternalGradebookLogic externalGradebookLogic;
    public void setExternalGradebookLogic(ExternalGradebookLogic externalGradebookLogic) {
        this.externalGradebookLogic = externalGradebookLogic;
    }  

    // Dependency
    private String curContext;
    public void setCurContext(String curContext) {
        this.curContext = curContext;
    }
    
    // Dependency
    private AsnnToggleRenderer toggleRenderer;
    public void setAsnnToggleRenderer(AsnnToggleRenderer toggleRenderer) {
        this.toggleRenderer = toggleRenderer;
    }

    /**
     * Renders the grade details at the top of the Student Submission
     * Page(s)
     * 
     * @param parent
     * @param clientID
     * @param assignmentSubmission
     * @param includeToggle
     */
    public void fillComponents(UIContainer parent, String clientID, AssignmentSubmission assignmentSubmission, boolean includeToggle) {
        /***
         * Grade Details including:
         *   - Grade
         *   - Comments
         */

        Assignment2 assignment = assignmentSubmission.getAssignment();
        //String title = assignment.getTitle();
        
        UIJointContainer joint = new UIJointContainer(parent, clientID, "grade-details-widget:");
        
        // Title
        if (includeToggle) {
            String hoverText = messageLocator.getMessage("grade.details.toggle.hover");
            String heading = messageLocator.getMessage("grade.details.heading");

            toggleRenderer.makeToggle(joint, "grade_toggle_section:", null, true, 
                    heading, hoverText, false, false, false, false, null);
        } else {
            UIMessage.make(joint, "grade-details-header", "assignment2.student-submit.grade_details_title");
        }
        
        UIOutput gradeSection = UIOutput.make(joint, "gradeSection");
          if (includeToggle) {
              // everything below the toggle is a subsection
              gradeSection.decorate(new UIFreeAttributeDecorator("class", "toggleSubsection subsection1"));
              gradeSection.decorate(new UIFreeAttributeDecorator("style", "display: none;"));
          }

          // Details Table
          UIOutput.make(joint, "grade-details-table");
        /*
         * Points possible : Display if the assignment is 
         *
         *  1) graded 
         *  2) associated with a gradebook item
         *  3) gradebook entry type is "Points"
         *  
         *  TODO FIXME Needs checks for whether the graded item is point based 
         *  (rather than percentage, pass/fail, etc). We are still working on 
         *  the best way to integrate this with the gradebook a reasonable
         *  amount of coupling.
         */
        if (assignment.isGraded() && assignment.getGradebookItemId() != null) {
            try {
                // make sure this gradebook item still exists
                GradebookItem gradebookItem;
                try {
                    gradebookItem = 
                        externalGradebookLogic.getGradebookItemById(curContext, 
                                assignment.getGradebookItemId());
                } catch (GradebookItemNotFoundException ginfe) {
                    if (log.isDebugEnabled()) log.debug("Student attempting to access assignment " + 
                            assignment.getId() + " but associated gb item no longer exists!");
                    gradebookItem = null;
                }
                // only display points possible if grade entry by points
                if (gradebookItem != null && externalGradebookLogic.getGradebookGradeEntryType(assignment.getContextId()) == ExternalGradebookLogic.ENTRY_BY_POINTS) {
                    UIOutput.make(joint, "points-possible-row");

                    String pointsDisplay;
                    if (gradebookItem.getPointsPossible() == null) {
                        pointsDisplay = messageLocator.getMessage("assignment2.student-submit.points_possible.none");
                    } else {
                        pointsDisplay = gradebookItem.getPointsPossible().toString();
                    }
                    UIOutput.make(joint, "points-possible", pointsDisplay); 
                }

                // Render the graded information if it's available.
                String grade = null;
                String comment = null;
                if (gradebookItem != null) {
                    GradeInformation gradeInfo = externalGradebookLogic.getGradeInformationForStudent(curContext, assignment.getGradebookItemId(), currentUser.getId());
                    if (gradeInfo != null) {
                        grade = gradeInfo.getGradebookGrade();
                        comment = gradeInfo.getGradebookComment();
                    }
                }

                if (grade != null) {
                    UIOutput.make(joint, "grade-row");
                    UIOutput.make(joint, "grade", grade);
                }
                else
                {
                    UIOutput.make(joint, "grade-row");
                    UIOutput.make(joint, "grade", messageLocator.getMessage("assignment2.student-submit.grade.na"));
                }

                if (comment != null) {
                    UIOutput.make(joint, "comment-row");
                    UIOutput.make(joint, "comment", comment);
                }
                else
                {
                    UIOutput.make(joint, "comment-row");
                    UIOutput.make(joint, "comment", messageLocator.getMessage("assignment2.student-submit.grade.none"));
                }

            } catch (IllegalArgumentException iae) {
                log.warn("Trying to look up grade object that doesn't exist" 
                        + "context: " + curContext 
                        + " gradeObjectId: " + assignment.getGradebookItemId() 
                        + "asnnId: " + assignment.getId());
            }
        }
    }

    public void fillComponents(UIContainer parent, String clientID) {
        // TODO Auto-generated method stub

    }
}