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
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.exception.AssignmentNotFoundException;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.tool.DisplayUtil;
import org.sakaiproject.assignment2.tool.params.ViewSubmissionParams;
import org.sakaiproject.assignment2.tool.producers.renderers.AsnnInstructionsRenderer;
import org.sakaiproject.assignment2.tool.producers.renderers.AsnnSubmissionVersionRenderer;
import org.sakaiproject.assignment2.tool.producers.renderers.AsnnToggleRenderer;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIMessage;
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
public class ViewStudentSubmissionProducer implements ViewComponentProducer, ViewParamsReporter {

    public static final String VIEW_ID = "view-submission";
    public String getViewID() {
        return VIEW_ID;
    }
    
    private static Log log = LogFactory.getLog(ViewStudentSubmissionProducer.class);

    private AssignmentLogic assignmentLogic;
    private Locale locale;
    private AssignmentPermissionLogic permissionLogic;
    private AsnnInstructionsRenderer asnnInstructionsRenderer;
    private AssignmentSubmissionLogic submissionLogic;
    private AsnnSubmissionVersionRenderer asnnSubmissionVersionRenderer;
    private MessageLocator messageLocator;
    private DisplayUtil displayUtil;
    private AsnnToggleRenderer toggleRenderer;

    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
          // Get Params
          ViewSubmissionParams params = (ViewSubmissionParams) viewparams;
          Long assignmentId = params.assignmentId;
          String studentUserId = params.userId;
          if (assignmentId == null || studentUserId == null){
              //handle error
              return;
          }
          if (!permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(studentUserId, assignmentId))
          {
              // user is not allowed to view the submission for this studentId
              throw new SecurityException("Attempt to view a submission without permission");
          }
          // validate assignmentId
          Assignment2 assignment;
          try
          {
              assignment = assignmentLogic.getAssignmentByIdWithAssociatedData(assignmentId);
          }
          catch (AssignmentNotFoundException anfe)
          {
              // AssignmentId is invalid, so return to handle error
              log.warn("User attempted to use an invalid assignmentId", anfe);
              return;
          }
          
          AssignmentSubmission assignmentSubmission = submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(assignmentId, studentUserId);
          
          // make sure the assignment is set correctly in the assignmentSubmission object, or it make cause problems later
          if (assignmentSubmission != null) {
              assignmentSubmission.setAssignment(assignment);
          }

          // use a date which is related to the current users locale
          DateFormat df = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT, locale);
          
          UIOutput.make(tofill, "title", assignment.getTitle());
          
          String dueDate;
          if (assignmentSubmission.getResubmitCloseDate()!=null)
          {
              dueDate = messageLocator.getMessage("assignment2.view-submission.due_date", new Object[]{df.format(assignmentSubmission.getResubmitCloseDate())});
          }
          else if (assignment.getDueDate()!=null)
          {
              dueDate = messageLocator.getMessage("assignment2.view-submission.due_date", new Object[]{df.format(assignment.getDueDate())});
          }
          else
          {
              dueDate = messageLocator.getMessage("assignment2.view-submission.no_due_date");
          }
          
          UIOutput.make(tofill, "dueDate", dueDate);
          
          // These are checks to display various statuses if there are no submissions for this page
          List<AssignmentSubmissionVersion> submittedVersions = new ArrayList<AssignmentSubmissionVersion>();
          if (assignment.getSubmissionType() == AssignmentConstants.SUBMIT_NON_ELECTRONIC)
          {
              // This is a non-electronic assignment, so display an appropriate message
              UIMessage.make(tofill, "submissionStatus", "assignment2.view-submission.status.non_electronic");
          }
          else if (!assignment.isRequiresSubmission())
          {
              // There are no submissions required for this assignment, so display an appropriate message
              UIMessage.make(tofill, "submissionStatus", "assignment2.view-submission.status.no_submission_required");
          }
          else
          {
              // We are filtering out versions that have not been submitted yet (e.g. drafts)
              if (assignmentSubmission.getSubmissionHistorySet()!=null)
              {
                  for (AssignmentSubmissionVersion asv : assignmentSubmission.getSubmissionHistorySet())
                  {
                      if (asv.getSubmittedDate()!=null)
                      {
                          submittedVersions.add(asv);
                      }
                  }
              }
              if (submittedVersions.isEmpty())
              {
                  // There are no submissions from the user yet, so display the appropriate message
                  UIMessage.make(tofill, "submissionStatus", "assignment2.view-submission.status.no_submission");
              }
          }
          
          asnnInstructionsRenderer.makeInstructions(tofill, "instructions:", assignment, false, false, false);
          
          if (!submittedVersions.isEmpty())
          {
              for (AssignmentSubmissionVersion single : submittedVersions)
              {
                  // figure out the status so we can determine what the heading should be
                  int status = submissionLogic.getSubmissionStatusForVersion(single, assignment.getDueDate(), assignmentSubmission.getResubmitCloseDate());
                  String headerText;
                  if (single.getSubmittedVersionNumber() == AssignmentSubmissionVersion.FEEDBACK_ONLY_VERSION_NUMBER) {
                      headerText = messageLocator.getMessage("assignment2.version.toggle.status.feedback_only_version");
                  } else {
                      headerText = displayUtil.getVersionStatusText(status, single.getStudentSaveDate(), single.getSubmittedDate());
                  }
                  String toggleHoverText = messageLocator.getMessage("assignment2.version.toggle.hover");
                  UIBranchContainer versionDiv = UIBranchContainer.make(tofill, "toggle-wrapper:");
                  toggleRenderer.makeToggle(versionDiv, "version_toggle:", null, true, headerText, toggleHoverText, false, false, false, false, null);
                  asnnSubmissionVersionRenderer.fillComponents(versionDiv, "submission:", single, true);
              }
          }
    }

    public ViewParameters getViewParameters() {
        return new ViewSubmissionParams();
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
    
    public void setAsnnInstructionsRenderer(AsnnInstructionsRenderer asnnInstructionsRenderer) {
        this.asnnInstructionsRenderer = asnnInstructionsRenderer;
    }

    public void setSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
        this.submissionLogic = submissionLogic;
    }
    
    public void setAsnnSubmissionVersionRenderer(AsnnSubmissionVersionRenderer asnnSubmissionVersionRenderer) {
        this.asnnSubmissionVersionRenderer = asnnSubmissionVersionRenderer;
    }
    
    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }
    
    public void setDisplayUtil(DisplayUtil displayUtil) {
        this.displayUtil = displayUtil;
    }
    
    public void setAsnnToggleRenderer(AsnnToggleRenderer toggleRenderer) {
        this.toggleRenderer = toggleRenderer;
    }
}
