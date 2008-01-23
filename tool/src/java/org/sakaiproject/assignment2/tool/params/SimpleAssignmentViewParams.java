
package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class SimpleAssignmentViewParams extends SimpleViewParameters {

	public Long assignmentId;
	
	public SimpleAssignmentViewParams() {}

    public SimpleAssignmentViewParams(String viewId, Long assignmentId){
    		super(viewId);
	        this.assignmentId = assignmentId;
    }
    
    public String getParseSpec() {
    	return super.getParseSpec() + ",@1:assignmentId";
    }
}