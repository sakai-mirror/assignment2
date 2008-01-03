package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class AssignmentGradeViewParams extends SimpleViewParameters {
	
	public Long assignmentId;
	public String userId;
	
	public AssignmentGradeViewParams(){}
	
	public AssignmentGradeViewParams(String viewId, Long assignmentId, String userId){
		super(viewId);
		this.assignmentId = assignmentId;
		this.userId = userId;
	}
	
}