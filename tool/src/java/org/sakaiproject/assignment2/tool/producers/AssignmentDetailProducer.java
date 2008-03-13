package org.sakaiproject.assignment2.tool.producers;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.model.Assignment2;

public class AssignmentDetailProducer implements ViewComponentProducer, ViewParamsReporter {

	public static final String VIEW_ID = "assignment_detail";
	public String getViewID(){
		return this.VIEW_ID;
	}
	
	private AssignmentLogic assignmentLogic;
	

	public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker){
		AssignmentViewParams params = (AssignmentViewParams) viewparams;
		
		Assignment2 assignment = assignmentLogic.getAssignmentById(params.assignmentId);
		
		UIOutput.make(tofill, "title", assignment.getTitle());
		UIVerbatim.make(tofill, "instructions", assignment.getInstructions());
		
	}

	public ViewParameters getViewParameters()
	{
		return new AssignmentViewParams();
	}

	public void setAssignmentLogic(AssignmentLogic assignmentLogic)
	{
		this.assignmentLogic = assignmentLogic;
	}
	
	
}