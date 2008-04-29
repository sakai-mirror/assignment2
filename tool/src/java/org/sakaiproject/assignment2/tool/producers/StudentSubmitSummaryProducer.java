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

import java.text.DateFormat;
import java.util.List;
import java.util.Locale;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.tool.params.SimpleAssignmentViewParams;
import org.sakaiproject.assignment2.tool.producers.renderers.AttachmentListRenderer;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class StudentSubmitSummaryProducer implements ViewComponentProducer, ViewParamsReporter {
	
	public static final String VIEW_ID = "student-submit-summary";
	public String getViewID() {
		return StudentSubmitSummaryProducer.VIEW_ID;
	}
	
	private AssignmentSubmissionLogic submissionLogic;
	private AssignmentLogic assignmentLogic;
	private ExternalLogic externalLogic;
	private Locale locale;
	private AttachmentListRenderer attachmentListRenderer;
	private MessageLocator messageLocator;
	
	public void fillComponents(UIContainer tofill, ViewParameters viewparams,
			ComponentChecker checker) {
		
		SimpleAssignmentViewParams params = (SimpleAssignmentViewParams) viewparams;
		
		// use a date which is related to the current users locale
		DateFormat df = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT, locale);

		Assignment2 assignment = assignmentLogic.getAssignmentByIdWithAssociatedData(params.assignmentId);
        
		AssignmentSubmission assignmentSubmission = 
			submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(params.assignmentId, externalLogic.getCurrentUserId());
		
		// set the textual representation of the submission status
		String status = "";
		int statusConstant = AssignmentConstants.SUBMISSION_NOT_STARTED;
    	if (assignmentSubmission != null) {
    		statusConstant = submissionLogic.getSubmissionStatusConstantForCurrentVersion(
    				assignmentSubmission.getCurrentSubmissionVersion(), assignment.getDueDate());
    		status = messageLocator.getMessage(
    				"assignment2.assignment_grade-assignment.submission_status." + 
    				statusConstant);
    	}
		
    	//Breadcrumbs
        UIInternalLink.make(tofill, "breadcrumb", 
        		messageLocator.getMessage("assignment2.student-assignment-list.heading"),
        		new SimpleViewParameters(StudentAssignmentListProducer.VIEW_ID));
        UIMessage.make(tofill, "last_breadcrumb", "assignment2.student-submit-summary.heading", new Object[] { assignment.getTitle() });
		
		//Display Assignment Info
    	UIOutput.make(tofill, "header.title", assignment.getTitle());

    	UIOutput.make(tofill, "header.due_date", (assignment.getDueDate() != null ? df.format(assignment.getDueDate()) : ""));

    	UIOutput.make(tofill, "header.status", status);
    	UIOutput.make(tofill, "header.grade_scale", "Grade Scale from Gradebook");  //HERE
    	if (assignment.getModifiedTime() != null) {
    		UIOutput.make(tofill, "modified_by_header_row");
    		UIOutput.make(tofill, "header.modified_by", df.format(assignment.getModifiedTime()));
    	}
    	UIVerbatim.make(tofill, "instructions", assignment.getInstructions());
    	
    	attachmentListRenderer.makeAttachmentFromAssignmentAttachmentSet(tofill, "attachment_list:", params.viewID, 
	        	assignment.getAttachmentSet());
		
		
		//Begin Looping for previous submissions
    	List<AssignmentSubmissionVersion> history = submissionLogic.getVersionHistoryForSubmission(assignmentSubmission);
                
    	for (AssignmentSubmissionVersion asv : history){
        	if (asv.isDraft()) {
        		continue;
        	}
        	UIBranchContainer loop = UIBranchContainer.make(tofill, "previous_submissions:");
        	
        	UIMessage.make(loop, "loop_submission", "assignment2.student-submit-summary.loop_submission", 
        			new Object[] { (asv.getSubmittedTime() != null ? df.format(asv.getSubmittedTime()) : "") });
        	UIVerbatim.make(loop, "loop_submitted_text", asv.getSubmittedText());
        	UIVerbatim.make(loop, "loop_feedback_text", asv.getAnnotatedTextFormatted());
        	UIVerbatim.make(loop, "loop_feedback_notes", asv.getFeedbackNotes());
        	attachmentListRenderer.makeAttachmentFromSubmissionAttachmentSet(loop, "loop_submitted_attachment_list:", 
        			GradeProducer.VIEW_ID, asv.getSubmissionAttachSet());
        	attachmentListRenderer.makeAttachmentFromFeedbackAttachmentSet(loop, "loop_returned_attachment_list:", 
        			GradeProducer.VIEW_ID, asv.getFeedbackAttachSet());
        	if (asv.getLastFeedbackSubmittedBy() != null) {
	        	UIMessage.make(loop, "feedback_updated", "assignment2.student-submit-summary.feedback_updated",
	        			new Object[]{ 
	        				(asv.getLastFeedbackTime() != null ? df.format(asv.getLastFeedbackTime()) : ""), 
	        				externalLogic.getUserDisplayName(asv.getLastFeedbackSubmittedBy()) });
        	} else {
        		UIMessage.make(loop, "feedback_updated", "assignment2.student-submit-summary.feedback_not_updated");
        	}
        }
        if (history == null || history.size() == 0) {
        	//no history, add dialog
        	UIMessage.make(tofill, "no_history", "assignment2.student-submit-summary.no_history");
        }
		
		
	}

	public ViewParameters getViewParameters() {
		return new SimpleAssignmentViewParams();
	}

	public void setSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
		this.submissionLogic = submissionLogic;
	}

	public void setExternalLogic(ExternalLogic externalLogic) {
		this.externalLogic = externalLogic;
	}


	public void setLocale(Locale locale) {
		this.locale = locale;
	}


	public void setAttachmentListRenderer(
			AttachmentListRenderer attachmentListRenderer) {
		this.attachmentListRenderer = attachmentListRenderer;
	}


	public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
		this.assignmentLogic = assignmentLogic;
	}


	public void setMessageLocator(MessageLocator messageLocator) {
		this.messageLocator = messageLocator;
	}
}