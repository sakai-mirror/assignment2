package org.sakaiproject.assignment2.tool.producers.fragments;

import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.tool.beans.PreviewAssignmentBean;
import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.assignment2.tool.producers.renderers.StudentViewAssignmentRenderer;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.content.ContentTypeInfoRegistry;
import uk.org.ponder.rsf.content.ContentTypeReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class FragmentAssignmentPreviewProducer implements ViewComponentProducer, ViewParamsReporter, ContentTypeReporter {

    public static final String VIEW_ID = "fragment-assignment_preview";
    public String getViewID() {
        return VIEW_ID;
    }

	private PreviewAssignmentBean previewAssignmentBean;
	private StudentViewAssignmentRenderer studentViewAssignmentRenderer;

    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	AssignmentViewParams params = (AssignmentViewParams) viewparams;

    	//we are coming from the add/edit assignment page
        Assignment2 assignment = previewAssignmentBean.getAssignment();
        
        AssignmentSubmission assignmentSubmission = new AssignmentSubmission();
    	
        String ASOTPKey = EntityBeanLocator.NEW_PREFIX + "1";
        studentViewAssignmentRenderer.makeStudentView(tofill, "portletBody:", assignmentSubmission, assignment, params, ASOTPKey, Boolean.TRUE);

    }
    
    public ViewParameters getViewParameters() {
        return new AssignmentViewParams();
    }
	
	public String getContentType() {
		return ContentTypeInfoRegistry.HTML_FRAGMENT;
	}

    public void setPreviewAssignmentBean(PreviewAssignmentBean previewAssignmentBean) {
    	this.previewAssignmentBean = previewAssignmentBean;
    }

	public void setStudentViewAssignmentRenderer(
			StudentViewAssignmentRenderer studentViewAssignmentRenderer) {
		this.studentViewAssignmentRenderer = studentViewAssignmentRenderer;
	}

}