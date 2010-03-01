/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/branches/ASNN-521/tool/src/java/org/sakaiproject/assignment2/tool/producers/GradeProducer.java $
 * $Id: GradeProducer.java 66084 2010-02-09 21:48:28Z wagnermr@iupui.edu $
 ***********************************************************************************
 *
 * Copyright (c) 2007, 2008 The Sakai Foundation.
 *
 * Licensed under the Educational Community License, Version 1.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.opensource.org/licenses/ecl1.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 **********************************************************************************/

package org.sakaiproject.assignment2.tool.producers;

import java.text.DateFormat;
import java.util.Locale;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.exception.GradebookItemNotFoundException;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.ExternalContentReviewLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.GradebookItem;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.tool.params.SimpleAssignmentViewParams;
import org.sakaiproject.assignment2.tool.producers.renderers.AsnnInstructionsRenderer;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

/**
 * This view is a read-only view of the assignment details
 * 
 *
 */
public class ViewAssignmentProducer implements ViewComponentProducer, ViewParamsReporter {

    public static final String VIEW_ID = "view-assignment";
    public String getViewID() {
        return VIEW_ID;
    }
    
    private static Log log = LogFactory.getLog(ViewAssignmentProducer.class);

    private MessageLocator messageLocator;
    private AssignmentLogic assignmentLogic;
    private Locale locale;
    private AssignmentPermissionLogic permissionLogic;
    private ExternalGradebookLogic gradebookLogic;
    private ExternalContentReviewLogic contentReviewLogic;
    private AsnnInstructionsRenderer asnnInstructionsRenderer;

    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
        
        //Get Params
        SimpleAssignmentViewParams params = (SimpleAssignmentViewParams) viewparams;
        Long assignmentId = params.assignmentId;
        if (assignmentId == null){
            //handle error
            return;
        }
        
        if (!permissionLogic.isUserAbleToViewAssignment(assignmentId, params.tagReference)) {
            throw new SecurityException("Attempt to view assignment without permission");
        }

        Assignment2 assignment = assignmentLogic.getAssignmentByIdWithAssociatedData(assignmentId, params.tagReference);

        // use a date which is related to the current users locale
        DateFormat df = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT, locale);

        boolean instructorView = permissionLogic.isUserAbleToAccessInstructorView(assignment.getContextId());
        
        UIOutput.make(tofill, "title", assignment.getTitle());
        
        String dueDateText = assignment.getDueDate() == null ? messageLocator.getMessage("assignment2.details.due_date.none") : 
            messageLocator.getMessage("assignment2.details.due_date", new Object[] {df.format(assignment.getDueDate())});
        UIOutput.make(tofill, "due-date", dueDateText);
        UIOutput.make(tofill, "open-date", df.format(assignment.getOpenDate()));
        
        // we also display the accept until date if it exists and the current user has instructor privileges
        if (instructorView && assignment.getAcceptUntilDate() != null) {
            UIOutput.make(tofill, "accept-until-row");
            UIOutput.make(tofill, "accept-until-date", df.format(assignment.getAcceptUntilDate()));
        }

        // Grading
        // We include whether this assignment is graded and, if it is,
        // we include points possible if it is graded by points.
        String gradedString = assignment.isGraded() ?  messageLocator.getMessage("assignment2.details.graded.yes") :
            messageLocator.getMessage("assignment2.details.graded.no");
        UIOutput.make(tofill, "is-graded", gradedString);
        
        if (assignment.isGraded() && assignment.getGradebookItemId() != null) {
            // make sure this gradebook item still exists
            GradebookItem gradebookItem;
            try {
                gradebookItem = 
                    gradebookLogic.getGradebookItemById(assignment.getContextId(), 
                            assignment.getGradebookItemId());
            } catch (GradebookItemNotFoundException ginfe) {
                if (log.isDebugEnabled()) log.debug("Attempt to access assignment " + 
                        assignment.getId() + " but associated gb item no longer exists!");
                gradebookItem = null;
            }
            // only display points possible if grade entry by points
            if (gradebookItem != null && gradebookLogic.getGradebookGradeEntryType(assignment.getContextId()) == ExternalGradebookLogic.ENTRY_BY_POINTS) {
                UIOutput.make(tofill, "points-possible-row");

                String pointsDisplay;
                if (gradebookItem.getPointsPossible() == null) {
                    pointsDisplay = messageLocator.getMessage("assignment2.details.gradebook.points_possible.none");
                } else {
                    pointsDisplay = gradebookItem.getPointsPossible().toString();
                }
                UIOutput.make(tofill, "points-possible", pointsDisplay); 
            }
        }

        // Content Review integration
        if (assignment.isContentReviewEnabled() && contentReviewLogic.isContentReviewAvailable(assignment.getContextId())) {
            UIOutput.make(tofill, "plagiarism-check-row");
            UIOutput.make(tofill, "plagiarism-check-enabled", messageLocator.getMessage("assignment2.details.plagiarism.check.yes"));
        }
        
        // if this assignment requires submission, we'll see if resubmission is allowed
        if (assignment.isRequiresSubmission()) {
            UIOutput.make(tofill, "resubmission-allowed-row");
            String resubmissionAllowedString;
            if (assignment.getNumSubmissionsAllowed() > 1 || 
                    assignment.getNumSubmissionsAllowed() == AssignmentConstants.UNLIMITED_SUBMISSION) {
                UIOutput.make(tofill, "assign-num-submissions-allowed-row");
                resubmissionAllowedString = messageLocator.getMessage("assignment2.details.resubmission.allowed.yes");
                
                if (assignment.getNumSubmissionsAllowed() == AssignmentConstants.UNLIMITED_SUBMISSION) {
                    UIOutput.make(tofill, "assign-num-submissions-allowed", messageLocator.getMessage("assignment2.details.resubmission.unlimited"));
                } else {
                    UIOutput.make(tofill, "assign-num-submissions-allowed", assignment.getNumSubmissionsAllowed() + "");
                }
            } else {
                resubmissionAllowedString = messageLocator.getMessage("assignment2.details.resubmission.allowed.no");
            }
            
            UIOutput.make(tofill, "resubmissions-allowed", resubmissionAllowedString);
        }
        
        //render the instructions
        asnnInstructionsRenderer.makeInstructions(tofill, "instructions_section:", assignment, 
                false, false, false);
    }

    public ViewParameters getViewParameters() {
        return new SimpleAssignmentViewParams();
    }

    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }

    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
        this.assignmentLogic = assignmentLogic;
    }

    public void setLocale(Locale locale) {
        this.locale = locale;
    }

    public void setAssignmentPermissionLogic(AssignmentPermissionLogic permissionLogic) {
        this.permissionLogic = permissionLogic;
    }
    
    public void setExternalGradebookLogic(ExternalGradebookLogic gradebookLogic) {
        this.gradebookLogic = gradebookLogic;
    }
    
    public void setExternalContentReviewLogic(ExternalContentReviewLogic contentReviewLogic) {
        this.contentReviewLogic = contentReviewLogic;
    }
    
    public void setAsnnInstructionsRenderer(AsnnInstructionsRenderer asnnInstructionsRenderer) {
        this.asnnInstructionsRenderer = asnnInstructionsRenderer;
    }

}
