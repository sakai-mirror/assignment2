/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/tool/src/java/org/sakaiproject/assignment2/tool/producers/RemoveAssignmentConfirmProducer.java $
 * $Id: RemoveAssignmentConfirmProducer.java $
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

import java.util.ArrayList;
import java.util.List;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;

import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIForm;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCaseReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

/**
 * This is responsible for rendering the Remove Assignments Confirmation
 * page/dialog.
 * 
 * TODO FIXME Add this to the verifiable assignment params.
 * 
 * @author sgithens
 */
public class RemoveAssignmentConfirmProducer implements ViewComponentProducer,
ViewParamsReporter, NavigationCaseReporter {
    public static final String VIEW_ID = "remove-assignment-confirm";
    
    private AssignmentLogic assignmentLogic;
    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
        this.assignmentLogic = assignmentLogic;
    }
    
    private AssignmentSubmissionLogic submissionLogic;
    public void setSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
        this.submissionLogic = submissionLogic;
    }
    
    private AssignmentPermissionLogic permissionLogic;
    public void setPermissionLogic(AssignmentPermissionLogic permissionLogic) {
        this.permissionLogic = permissionLogic;
    }
    
    private String curUserId;
    public void setCurUserId(String curUserId) {
        this.curUserId = curUserId;
    }

    public void fillComponents(UIContainer tofill, ViewParameters viewparams,
            ComponentChecker checker) {
        AssignmentViewParams params = (AssignmentViewParams) viewparams;
        
        Assignment2 assignment = assignmentLogic.getAssignmentById(params.assignmentId);
        
        UIOutput.make(tofill, "assignment-list-table");
        UIBranchContainer tablerow = UIBranchContainer.make(tofill, "asnn-row:");
        UIOutput.make(tablerow, "asnn-title", assignment.getTitle());
        if (assignment.getDueDate() == null) {
            UIMessage.make(tablerow, "due", "assignment2.remove.assn.no_due_date"); 
        } else {
            UIOutput.make(tablerow, "due", assignment.getDueDate().toLocaleString()); // TODO FIXME
        }
            
        List<String> viewableStudents = permissionLogic.getViewableStudentsForUserForItem(curUserId, assignment);
        int totalSubmissions = submissionLogic.getNumStudentsWithASubmission(assignment, viewableStudents);

        UIOutput.make(tablerow, "submissions", totalSubmissions+"");
        
        UIForm removeForm = UIForm.make(tofill, "confirm-remove-form");
        
    }

    public String getViewID() {
        return VIEW_ID;
    }

    public ViewParameters getViewParameters() {
        return new AssignmentViewParams();
    }

    public List reportNavigationCases() {
        List cases = new ArrayList();
        return cases;
    }

}
