package org.sakaiproject.assignment2.tool.producers.dev;

import java.util.ArrayList;
import java.util.List;

import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.assignment2.tool.producers.AssignmentListSortViewProducer;
import org.sakaiproject.assignment2.tool.producers.AssignmentProducer;
import org.sakaiproject.assignment2.tool.producers.fragments.FragmentAssignmentPreviewProducer;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCase;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCaseReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class AssignmentDevProducer implements ViewComponentProducer, NavigationCaseReporter, ViewParamsReporter {
	
	private AssignmentProducer assignmentProducer;
	public void setAssignmentProducer(AssignmentProducer assignmentProducer) {
		this.assignmentProducer = assignmentProducer;
	}

	public static final String VIEW_ID = "assignment_dev";
	public String getViewID() {
		return VIEW_ID;
	}

	public void fillComponents(UIContainer tofill, ViewParameters viewparams,
			ComponentChecker checker) {
		assignmentProducer.fillComponents(tofill, viewparams, checker);
		
	}

	public List reportNavigationCases() {
    	List<NavigationCase> nav= new ArrayList<NavigationCase>();
        nav.add(new NavigationCase("post", new SimpleViewParameters(
            AssignmentListSortViewProducer.VIEW_ID)));
        nav.add(new NavigationCase("preview", new AssignmentViewParams(
        	FragmentAssignmentPreviewProducer.VIEW_ID, null)));
        nav.add(new NavigationCase("refresh", new AssignmentViewParams(
        	AssignmentProducer.VIEW_ID, null)));
        nav.add(new NavigationCase("save_draft", new SimpleViewParameters(
        	AssignmentListSortViewProducer.VIEW_ID)));
        nav.add(new NavigationCase("cancel", new SimpleViewParameters(
        	AssignmentListSortViewProducer.VIEW_ID)));
        return nav;
    }

    public ViewParameters getViewParameters() {
        return new AssignmentViewParams();
    }
	
	
}