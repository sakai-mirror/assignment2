package org.sakaiproject.assignment2.tool.params;

public class ZipViewParams extends AssignmentViewParams {

	public Long assignmentId;
	
	public ZipViewParams(String viewID, Long assignmentId) {
		super(viewID, assignmentId);
	}
	
	public ZipViewParams() {}

	public String getParseSpec() {
    	return super.getParseSpec() + ",@1:assignmentId"; 
    }
}
