package org.sakaiproject.assignment2.tool.producers.fragments;

import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.tool.params.FragmentGradebookDetailsViewParams;
import org.sakaiproject.assignment2.tool.producers.renderers.GradebookDetailsRenderer;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.content.ContentTypeReporter;
import uk.org.ponder.rsf.content.ContentTypeInfoRegistry;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class FragmentGradebookDetailsProducer implements ViewComponentProducer, ContentTypeReporter, ViewParamsReporter{
	
    public static final String VIEW_ID = "fragment-gradebook-details";
    public String getViewID() {
        return VIEW_ID;
    }
    	
	private GradebookDetailsRenderer gradebookDetailsRenderer;
	public void setGradebookDetailsRenderer(GradebookDetailsRenderer gradebookDetailsRenderer){
		this.gradebookDetailsRenderer = gradebookDetailsRenderer;
	}
	
	private AssignmentSubmissionLogic assignmentSubmissionLogic;
	public void setAssignmentSubmissionLogic(AssignmentSubmissionLogic assignmentSubmissionLogic) {
		this.assignmentSubmissionLogic = assignmentSubmissionLogic;
	}
	
    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	FragmentGradebookDetailsViewParams params = (FragmentGradebookDetailsViewParams) viewparams;
    	
    	
    	AssignmentSubmission as = assignmentSubmissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(params.assignmentId, params.userId);
    	gradebookDetailsRenderer.makeGradebookDetails(tofill, "gradebook_details", as, params.assignmentId, params.userId);
    	
    }
    	
	public String getContentType() {
		return ContentTypeInfoRegistry.HTML_FRAGMENT;
	}

	public ViewParameters getViewParameters() {
		return new FragmentGradebookDetailsViewParams();
	}
}