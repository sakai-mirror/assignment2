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

package org.sakaiproject.assignment2.tool.producers;

import java.awt.Color;
import java.text.DateFormat;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.tool.beans.Assignment2Bean;
import org.sakaiproject.assignment2.tool.beans.locallogic.LocalAssignmentLogic;
import org.sakaiproject.assignment2.tool.params.AssignmentListSortViewParams;
import org.sakaiproject.assignment2.tool.params.SimpleAssignmentViewParams;
import org.sakaiproject.assignment2.tool.producers.renderers.PagerRenderer;
import org.sakaiproject.assignment2.tool.producers.renderers.SortHeaderRenderer;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIBoundBoolean;
import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UICommand;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIELBinding;
import uk.org.ponder.rsf.components.UIForm;
import uk.org.ponder.rsf.components.UIInput;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UILink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.components.decorators.UIColourDecorator;
import uk.org.ponder.rsf.components.decorators.UIDecorator;
import uk.org.ponder.rsf.components.decorators.UIFreeAttributeDecorator;
import uk.org.ponder.rsf.components.decorators.UIStyleDecorator;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;
import uk.org.ponder.htmlutil.HTMLUtil;

/**
 * This view is responsible for showing the Student Landing page which will have
 * the To Do style list of assignments that need to be completed.
 * 
 * @author rjlowe
 * @author sgithens
 *
 */
public class StudentAssignmentListProducer implements ViewComponentProducer, ViewParamsReporter {

    public static final String VIEW_ID = "student-assignment-list";
    public String getViewID(){
        return VIEW_ID;
    }

    private PagerRenderer pagerRenderer;
    private AssignmentLogic assignmentLogic;
    private AssignmentSubmissionLogic submissionLogic;
    private Locale locale;
    private Assignment2Bean assignment2Bean;
    private SortHeaderRenderer sortHeaderRenderer;
    private LocalAssignmentLogic localAssignmentLogic;
    private ExternalLogic externalLogic;
    private MessageLocator messageLocator;
    private ExternalGradebookLogic externalGradebookLogic;

    public static final String DEFAULT_SORT_DIR = AssignmentLogic.SORT_DIR_ASC;
    public static final String DEFAULT_OPPOSITE_SORT_DIR = AssignmentLogic.SORT_DIR_DESC;
    public static final String DEFAULT_SORT_BY = AssignmentLogic.SORT_BY_INDEX;

    private String current_sort_by = DEFAULT_SORT_BY;
    private String current_sort_dir = DEFAULT_SORT_DIR;
    private String opposite_sort_dir = DEFAULT_OPPOSITE_SORT_DIR;

    //images
    public static final String BULLET_UP_IMG_SRC = "/sakai-assignment2-tool/content/images/bullet_arrow_up.png";
    public static final String BULLET_DOWN_IMG_SRC = "/sakai-assignment2-tool/content/images/bullet_arrow_down.png";
    public static final String ATTACH_IMG_SRC = "/sakai-assignment2-tool/content/images/attach.png";


    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {

        // use a date which is related to the current users locale
        DateFormat df = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT, locale);

        //get parameters
        AssignmentListSortViewParams params = (AssignmentListSortViewParams) viewparams;
        if (params.sort_by == null) params.sort_by = DEFAULT_SORT_BY;
        if (params.sort_dir == null) params.sort_dir = DEFAULT_SORT_DIR;
        current_sort_by = params.sort_by;
        current_sort_dir = params.sort_dir;
        opposite_sort_dir = (AssignmentLogic.SORT_DIR_ASC.equals(current_sort_dir) ? AssignmentLogic.SORT_DIR_DESC : AssignmentLogic.SORT_DIR_ASC);
        UIVerbatim.make(tofill, "defaultSortBy", HTMLUtil.emitJavascriptVar("defaultSortBy", DEFAULT_SORT_BY));

        //check if we need to duplicate an assignment, params.assignmentIdToDuplicate is not null
        if (params.assignmentIdToDuplicate != null) {
            assignment2Bean.createDuplicate(params.assignmentIdToDuplicate);
            params.assignmentIdToDuplicate = null;
        }

        //get paging data
        //List<Assignment2> entries = assignmentLogic.getViewableAssignments();
        List<AssignmentSubmission> submissionEntries = submissionLogic.getSubmissionsForCurrentUser();

        //Breadcrumbs
        UIMessage.make(tofill, "last_breadcrumb", "assignment2.student-assignment-list.heading");

        UIMessage.make(tofill, "page-title", "assignment2.student-assignment-list.title");
        
        //pagerRenderer.makePager(tofill, "pagerDiv:", VIEW_ID, viewparams, entries.size());

        //table headers and sorting links
        /*
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.assignment", viewparams, 
                AssignmentLogic.SORT_BY_TITLE, "assignment2.student-assignment-list.tableheader.assignment");
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.for", viewparams, 
                LocalAssignmentLogic.SORT_BY_FOR, "assignment2.student-assignment-list.tableheader.for");
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.status", viewparams, 
                LocalAssignmentLogic.SORT_BY_STATUS, "assignment2.student-assignment-list.tableheader.status");
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.open", viewparams, 
                AssignmentLogic.SORT_BY_OPEN, "assignment2.student-assignment-list.tableheader.open");
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.due", viewparams, 
                AssignmentLogic.SORT_BY_DUE, "assignment2.student-assignment-list.tableheader.due");
        */

        if (submissionEntries.size() <= 0) {
            UIMessage.make(tofill, "assignment_empty", "assignment2.student-assignment-list.assignment_empty");
            return;
        }

        // retrieve groups here for display of group restrictions
        //Map<String, String> groupIdToNameMap = externalLogic.getGroupIdToNameMapForSite(externalLogic.getCurrentContextId());

        // retrieve the statuses for the assignments
        //Map<Assignment2, Integer> assignToStatusMap = submissionLogic.getSubmissionStatusConstantForAssignments(entries, externalLogic.getCurrentUserId());

        //Fill out Table
        for (AssignmentSubmission assignmentSubmission : submissionEntries){
            UIBranchContainer row = UIBranchContainer.make(tofill, "assignment-row:");

            Assignment2 assignment = assignmentSubmission.getAssignment();
            
            //UILink.make(row, "attachments", ATTACH_IMG_SRC);

            // Access
            //String restrictedToText = messageLocator.getMessage("assignment2.assignment_restrict_to_site");
            //if (assignment.getAssignmentGroupSet() != null && !assignment.getAssignmentGroupSet().isEmpty()) {
                // we need to display a comma-delimited list of groups
            //    restrictedToText = localAssignmentLogic.getListOfGroupRestrictionsAsString(
            //            assignment.getAssignmentGroupSet(), groupIdToNameMap);
            //}
            //UIOutput.make(row, "assignment_row_for", restrictedToText);

            //Integer status = assignToStatusMap.get(assignment);
            //String statusText = messageLocator.getMessage("assignment2.submission_status." + status.intValue());
            //UIOutput.make(row, "assignment_row_status", statusText);

            //UIOutput.make(row, "assignment_row_open", df.format(assignment.getOpenDate()));
   
            boolean assignmentCompleted = assignmentSubmission.isCompleted();
            
            // Todo
            UIForm markTodoForm = UIForm.make(row, "todo-check-form");
            UIBoundBoolean todoCheck = UIBoundBoolean.make(markTodoForm, "todo-checkbox", "MarkTodoBean.checkTodo", assignmentCompleted);
            UICommand hiddenSubmit = UICommand.make(markTodoForm, "submit-button", "MarkTodoBean.markTodo");
            todoCheck.decorate(new UIFreeAttributeDecorator("onclick", "document.getElementById('"+hiddenSubmit.getFullID()+"').click()"));
            hiddenSubmit.addParameter(new UIELBinding("MarkTodoBean.assignmentId", assignment.getId()));
            
            /*
             * TODO FIXME I'm having major issues getting the CSS style to take
             * effect here, so wer are creating this decorated color for now.
             * If you pass null to the UIColourDecorator it doesn't do anything,
             * so this lets us create a color to pass to each item that needs
             * greyed out (or doesn't, in which case we just leave it null).
             */
            Color decoratedColor = assignmentCompleted ? Color.gray : null;
            UIDecorator assnItemDecorator = new UIColourDecorator(decoratedColor, null);
            
            /*
             * Title and Action Links
             * 
             * There are 4 options for the string on the action link:
             * 
             * 1) View Details and Submit
             *    - Assignment has not been submitted
             *    - accept until date has not passed
             *    - submission type of text only, attachments only, or text and attachments
             * 
             * 2) View Details
             *    - submission type of non-electronic or do not require a submission
             * 
             * 3) Resubmit
             *    - assignment allows resubmission
             *    - accept until date has not passed
             * 
             * 4) View Submission / View Submissions
             *    - shown for assignments 
             * 
             */            
            UIOutput.make(row, "assignment-title", assignment.getTitle())
                    .decorate(assnItemDecorator);
            
            /*
             * If the assignment has been deleted, we are suppose to show this
             * bit of text.
             */
            if (assignment.isRemoved()) {
                UIMessage.make(row, "assignment-deleted", "assignment2.student-assignment-list.assignment-deleted");
            }
            
            boolean isOpenForSubmission = submissionLogic.isSubmissionOpenForStudentForAssignment(
                    assignmentSubmission.getUserId(), assignment.getId());
            
            int numberOfRealSubmissions = 0;
            for (AssignmentSubmissionVersion version: assignmentSubmission.getSubmissionHistorySet()) {
                if (version.getSubmittedVersionNumber() > 0) {
                    numberOfRealSubmissions++;
                }
            }
            
            String actionmsgkey;
            // 1. View Details and Submit
            if (isOpenForSubmission && numberOfRealSubmissions < 1) {
                actionmsgkey = "assignment2.student-assignment-list.action.view-details-and-submit";
            }
            // 3. Resubmit
            else if (isOpenForSubmission && numberOfRealSubmissions >= 1) {
                actionmsgkey = "assignment2.student-assignment-list.action.view-details-and-resubmit";
            }
            // 4a View Submission
            else if (numberOfRealSubmissions == 1) {
                actionmsgkey = "assignment2.student-assignment-list.action.view-submission";
            }
            // 4b View Submissions
            else if (numberOfRealSubmissions > 1) {
                actionmsgkey = "assignment2.student-assignment-list.action.view-submissions";
            }
            // 2 View Details
            else {
                actionmsgkey = "assignment2.student-assignment-list.action.view-details";
            }
            
            UIInternalLink.make(row, "assignment-action-link", UIMessage.make(actionmsgkey),  
                new SimpleAssignmentViewParams(StudentSubmitProducer.VIEW_ID, assignment.getId()));
            
            // Due date
            if (assignment.getDueDate() != null) {
                UIOutput.make(row, "assignment_row_due", df.format(assignment.getDueDate())).decorate(assnItemDecorator);
            } 
            else {
                UIMessage.make(row, "assignment_row_due", "assignment2.student-assignment-list.no_due_date").decorate(assnItemDecorator);
            }
            
            /*
             *  Feedback
             */
            boolean feedbackExists = false;
            boolean unreadFeedbackExists = false;
            Set<AssignmentSubmissionVersion> submissions = assignmentSubmission.getSubmissionHistorySet();
            for (AssignmentSubmissionVersion version: submissions) {
                if (version.isFeedbackReleased()) {
                    feedbackExists = true;
                }
                if (!version.isFeedbackRead()) {
                    unreadFeedbackExists = true;
                }
            }
            
            if (feedbackExists && unreadFeedbackExists) {
                UIInternalLink.make(row, "unread-feedback-link",
                        new SimpleAssignmentViewParams(StudentSubmitProducer.VIEW_ID, assignment.getId()));
            }
            else if (feedbackExists) {
                UIInternalLink.make(row, "read-feedback-link",
                        new SimpleAssignmentViewParams(StudentSubmitProducer.VIEW_ID, assignment.getId()));
            }
            // else.  TODO FIXME
            // I know you're always supposed to have an ending else
            // but I can't think of what to put here at the moment.
            // We should probably put an accessible note.
            
            /*
             * Grade
             */
            if (!assignment.isGraded()) {
                UIMessage.make(row, "grade", "assignment2.student-assignment-list.not-graded").decorate(assnItemDecorator);
            } else {
                String grade = externalGradebookLogic.getStudentGradeForItem(
                        assignment.getContextId(), 
                        assignmentSubmission.getUserId(), 
                        assignment.getGradableObjectId());
                
                if (grade == null) {
                    UIMessage.make(row, "grade", "assignment2.student-assignment-list.no-grade-yet").decorate(assnItemDecorator);
                } else {
                    UIOutput.make(row, "grade", grade).decorate(assnItemDecorator);
                }
                
            }

            //For JS Sorting
            //UIOutput.make(row, "open_timestamp", assignment.getOpenDate() != null ? String.valueOf(assignment.getOpenDate().getTime()) : "");
            //UIOutput.make(row, "due_timestamp", assignment.getDueDate() != null ? String.valueOf(assignment.getDueDate().getTime()) : "");
            //UIOutput.make(row, "sortIndex", String.valueOf(assignment.getSortIndex()));
        }
    }

    public ViewParameters getViewParameters() {
        return new AssignmentListSortViewParams();
    }

    public void setPagerRenderer(PagerRenderer pagerRenderer) {
        this.pagerRenderer = pagerRenderer;
    }

    public void setAssignmentLogic (AssignmentLogic assignmentLogic) {
        this.assignmentLogic = assignmentLogic;
    }

    public void setAssignmentSubmissionLogic (AssignmentSubmissionLogic submissionLogic) {
        this.submissionLogic = submissionLogic;
    }

    public void setLocale(Locale locale) {
        this.locale = locale;
    }

    public void setAssignment2Bean(Assignment2Bean assignment2Bean) {
        this.assignment2Bean = assignment2Bean;
    }

    public void setSortHeaderRenderer(SortHeaderRenderer sortHeaderRenderer) {
        this.sortHeaderRenderer = sortHeaderRenderer;
    }

    public void setLocalAssignmentLogic(LocalAssignmentLogic localAssignmentLogic) {
        this.localAssignmentLogic = localAssignmentLogic;
    }

    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }

    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }

    public void setExternalGradebookLogic(
            ExternalGradebookLogic externalGradebookLogic) {
        this.externalGradebookLogic = externalGradebookLogic;
    }
}