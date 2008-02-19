package org.sakaiproject.assignment2.tool.producers.dev;

import java.util.ArrayList;
import java.util.List;

import org.sakaiproject.assignment2.tool.params.GradeViewParams;
import org.sakaiproject.assignment2.tool.params.ViewSubmissionsViewParams;
import org.sakaiproject.assignment2.tool.producers.GradeProducer;
import org.sakaiproject.assignment2.tool.producers.ViewSubmissionsProducer;
import org.sakaiproject.assignment2.tool.producers.fragments.FragmentSubmissionGradePreviewProducer;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.flow.ARIResult;
import uk.org.ponder.rsf.flow.ActionResultInterceptor;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCase;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCaseReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class GradeDevProducer implements ViewComponentProducer, NavigationCaseReporter, ViewParamsReporter, ActionResultInterceptor {

	private GradeProducer gradeProducer;
	public void setGradeProducer(GradeProducer gradeProducer) {
		this.gradeProducer = gradeProducer;
	}
	
    public static final String VIEW_ID = "grade_dev";
    public String getViewID() {
        return VIEW_ID;
    }
	public void fillComponents(UIContainer tofill, ViewParameters viewparams,
			ComponentChecker checker) {
		gradeProducer.fillComponents(tofill, viewparams, checker);
		
	}
	public List reportNavigationCases() {
    	List<NavigationCase> nav= new ArrayList<NavigationCase>();
    	nav.add(new NavigationCase("release_all", new GradeViewParams(
    			GradeProducer.VIEW_ID, null, null)));
    	nav.add(new NavigationCase("submit", new ViewSubmissionsViewParams(
               ViewSubmissionsProducer.VIEW_ID)));
        nav.add(new NavigationCase("preview", new SimpleViewParameters(
              FragmentSubmissionGradePreviewProducer.VIEW_ID)));
        nav.add(new NavigationCase("cancel", new ViewSubmissionsViewParams(
                ViewSubmissionsProducer.VIEW_ID)));
        return nav;
	}
	public void interceptActionResult(ARIResult result, ViewParameters incoming, Object actionReturn) {
	    if (result.resultingView instanceof ViewSubmissionsViewParams) {
	    	ViewSubmissionsViewParams outgoing = (ViewSubmissionsViewParams) result.resultingView;
	    	GradeViewParams in = (GradeViewParams) incoming;
	    	outgoing.assignmentId = in.assignmentId;
	    } else if (result.resultingView instanceof GradeViewParams) {
	    	GradeViewParams outgoing = (GradeViewParams) result.resultingView;
	    	GradeViewParams in = (GradeViewParams) incoming;
	    	outgoing.assignmentId = in.assignmentId;
	    	outgoing.userId = in.userId;
	    	outgoing.submissionId = in.submissionId;
	    }
	}
	
	public ViewParameters getViewParameters() {
	    return new GradeViewParams();
	}
	
}