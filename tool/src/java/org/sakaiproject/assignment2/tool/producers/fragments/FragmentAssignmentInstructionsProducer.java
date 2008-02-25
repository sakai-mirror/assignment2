package org.sakaiproject.assignment2.tool.producers.fragments;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.assignment2.tool.producers.renderers.AttachmentListRenderer;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.content.ContentTypeReporter;
import uk.org.ponder.rsf.content.ContentTypeInfoRegistry;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class FragmentAssignmentInstructionsProducer implements ViewComponentProducer, ViewParamsReporter, ContentTypeReporter {

    public static final String VIEW_ID = "fragment-assignment-instructions";
    public String getViewID() {
        return VIEW_ID;
    }
    
    private AssignmentLogic assignmentLogic;
    private AttachmentListRenderer attachmentListRenderer;

    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	AssignmentViewParams params = (AssignmentViewParams) viewparams;

    	Assignment2 assignment = assignmentLogic.getAssignmentByIdWithAssociatedData(params.assignmentId);
    	
    	UIVerbatim.make(tofill, "assignment_instructions", assignment.getInstructions());
        attachmentListRenderer.makeAttachmentFromAssignmentAttachmentSet(tofill, "assignment_attachment_list:", params.viewID, 
        		assignment.getAttachmentSet(), Boolean.FALSE);


    }
    
    public ViewParameters getViewParameters() {
        return new AssignmentViewParams();
    }
	
	public String getContentType() {
		return ContentTypeInfoRegistry.HTML_FRAGMENT;
	}

	public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
		this.assignmentLogic = assignmentLogic;
	}

	public void setAttachmentListRenderer(
			AttachmentListRenderer attachmentListRenderer) {
		this.attachmentListRenderer = attachmentListRenderer;
	}
}