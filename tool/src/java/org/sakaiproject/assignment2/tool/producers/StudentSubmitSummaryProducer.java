package org.sakaiproject.assignment2.tool.producers;

import java.util.List;

import org.sakaiproject.assignment2.tool.params.SimpleAssignmentViewParams;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCaseReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class StudentSubmitSummaryProducer implements ViewComponentProducer, ViewParamsReporter {
	
	public static final String VIEW_ID = "student-submit-summary";
	public String getViewID() {
		return this.VIEW_ID;
	}
	public void fillComponents(UIContainer tofill, ViewParameters viewparams,
			ComponentChecker checker) {
		
		
		
	}

	public ViewParameters getViewParameters() {
		return new SimpleAssignmentViewParams();
	}
}