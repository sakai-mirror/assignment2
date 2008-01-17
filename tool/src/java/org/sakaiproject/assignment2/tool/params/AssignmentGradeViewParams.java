package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class AssignmentGradeViewParams extends SimpleViewParameters {
	
	public Long assignmentId;
	public String userId;
	public Long submissionId;
	
	public AssignmentGradeViewParams(){}
	
	public AssignmentGradeViewParams(String viewId, Long assignmentId, String userId){
		super(viewId);
		this.assignmentId = assignmentId;
		this.userId = userId;
	}
	
	public AssignmentGradeViewParams(String viewId, Long assignmentId, String userId, Long submissionId) {
		super(viewId);
		this.assignmentId = assignmentId;
		this.userId = userId;
		this.submissionId = submissionId;
	}
	
	public String getParseSpec(){
		return super.getParseSpec() + ",@1:assignmentId,@2:userId,?submissionId";
	}
	
}