/**********************************************************************************
 * $URL$
 * $Id$
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

package org.sakaiproject.assignment2.tool.producers.renderers;

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.List;
import java.util.Locale;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.tool.params.FilePickerHelperViewParams;
import org.sakaiproject.assignment2.tool.params.FragmentViewSubmissionViewParams;
import org.sakaiproject.assignment2.tool.producers.AddAttachmentHelperProducer;
import org.sakaiproject.assignment2.tool.producers.StudentAssignmentListProducer;
import org.sakaiproject.assignment2.tool.producers.fragments.FragmentViewSubmissionProducer;
import org.sakaiproject.assignment2.tool.producers.evolvers.AttachmentInputEvolver;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIBoundBoolean;
import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UICommand;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIELBinding;
import uk.org.ponder.rsf.components.UIForm;
import uk.org.ponder.rsf.components.UIInput;
import uk.org.ponder.rsf.components.UIInputMany;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.components.decorators.DecoratorList;
import uk.org.ponder.rsf.components.decorators.UIFreeAttributeDecorator;
import uk.org.ponder.rsf.evolvers.TextInputEvolver;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;


/**
 * Renders the Students view of an assignments details and instructions, as well
 * as completing the material and viewing previous submissions and feedback.
 * 
 * @author rjlowe
 * @author sgithens
 *
 */
public class StudentViewAssignmentRenderer {
    private static Log log = LogFactory.getLog(StudentViewAssignmentRenderer.class);

    // Dependency
    private Locale locale;
    public void setLocale(Locale locale) {
        this.locale = locale;
    }

    // Dependency
    private MessageLocator messageLocator;
    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }

    // Dependency
    private AssignmentSubmissionLogic submissionLogic;
    public void setSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
        this.submissionLogic = submissionLogic;
    }
    
    // Dependency
    private AsnnSubmissionDetailsRenderer asnnSubmissionDetailsRenderer;
    public void setAsnnSubmissionDetailsRenderer(AsnnSubmissionDetailsRenderer asnnSubmissionDetailsRenderer) {
        this.asnnSubmissionDetailsRenderer = asnnSubmissionDetailsRenderer;
    }
    
    // Dependency
    private AsnnInstructionsRenderer asnnInstructionsRenderer;
    public void setAsnnInstructionsRenderer(AsnnInstructionsRenderer asnnInstructionsRenderer) {
        this.asnnInstructionsRenderer = asnnInstructionsRenderer;
    }

    // Dependency
    private AsnnSubmitEditorRenderer asnnSubmitEditorRenderer;
    public void setAsnnSubmitEditorRenderer(AsnnSubmitEditorRenderer asnnSubmitEditorRenderer) {
        this.asnnSubmitEditorRenderer = asnnSubmitEditorRenderer;
    }
    
    public void makeStudentView(UIContainer tofill, String divID, AssignmentSubmission assignmentSubmission, 
            Assignment2 assignment, ViewParameters params, String ASOTPKey, Boolean preview) {
        System.out.println("THE STUDENT VIEW PASSED IN ASOTPKey: " + ASOTPKey);
        /**
         * Breadcrumbs
         */
        if (!preview) {
            UIInternalLink.make(tofill, "breadcrumb", 
                    messageLocator.getMessage("assignment2.student-assignment-list.heading"),
                    new SimpleViewParameters(StudentAssignmentListProducer.VIEW_ID));
        } else {
            UIMessage.make(tofill, "breadcrumb", "assignment2.student-assignment-list.heading");
        }
        
        
        


        if (assignmentSubmission != null) {
            assignmentSubmission.setAssignment(assignment);
        }
        UIJointContainer joint = new UIJointContainer(tofill, divID, "portletBody:", ""+1);

        // use a date which is related to the current users locale
        DateFormat df = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT, locale);

        //For preview, get a decorated list of disabled="disabled"
        Map<String, String> disabledAttr = new HashMap<String, String>();
        disabledAttr.put("disabled", "disabled");

        Map<String, String> disabledLinkAttr = new HashMap<String, String>();
        disabledLinkAttr.put("onclick", "return false;");

        // set the textual representation of the submission status
        String status = "";
        int statusConstant = AssignmentConstants.SUBMISSION_NOT_STARTED;
        if (assignmentSubmission != null) {
            statusConstant = submissionLogic.getSubmissionStatusConstantForCurrentVersion(
                    assignmentSubmission.getCurrentSubmissionVersion(), assignment.getDueDate());
            status = messageLocator.getMessage(
                    "assignment2.student-submit.status." + 
                    statusConstant);
        }
        
        asnnSubmissionDetailsRenderer.fillComponents(joint, "assignment-details:", assignmentSubmission);
        
        asnnInstructionsRenderer.fillComponents(joint, "assignment-instructions:", assignment);
        
        asnnSubmitEditorRenderer.fillComponents(joint, "assignment-edit-submission:", assignmentSubmission, preview);

        //Begin Looping for previous submissions
        List<AssignmentSubmissionVersion> history = new ArrayList<AssignmentSubmissionVersion>();
        if (!preview) {
            history = submissionLogic.getVersionHistoryForSubmission(assignmentSubmission);
        }

        for (AssignmentSubmissionVersion asv : history){
            if (asv.isDraft()) { 
                continue;
            }

            UIBranchContainer loop = UIBranchContainer.make(joint, "previous_submissions:");
            UIOutput.make(loop, "previous_date", (asv.getSubmittedDate() != null ? df.format(asv.getSubmittedDate()) : ""));
//            if (asvOTPKey.equals(asv.getId().toString())){
                //we are editing this version
  //              UIMessage.make(loop, "current_version", "assignment2.student-submit.current_version");
    //        } else {
                //else add link to edit this submission
      //          UIInternalLink.make(loop, "previous_link", 
        //                messageLocator.getMessage("assignment2.assignment_grade.view_submission"),
        //                new FragmentViewSubmissionViewParams(FragmentViewSubmissionProducer.VIEW_ID, asv.getId()));
         //   }
        }
        if (history == null || history.size() == 0) {
            //no history, add dialog
            UIMessage.make(joint, "no_history", "assignment2.student-submit.no_history");
        }

        
    }
}