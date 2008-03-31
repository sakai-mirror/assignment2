
package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class SimpleAssignmentViewParams extends SimpleViewParameters {

	public Long assignmentId;
	public boolean clearSession = true;
	
	public SimpleAssignmentViewParams() {}

    public SimpleAssignmentViewParams(String viewId, Long assignmentId){
    		super(viewId);
	        this.assignmentId = assignmentId;
    }
    
    public SimpleAssignmentViewParams(String viewId, Long assignmentId, boolean clearSession) {
    	super(viewId);
    	this.assignmentId = assignmentId;
    	this.clearSession = clearSession;
    }
    
    public String getParseSpec() {
    	return super.getParseSpec() + ",@1:assignmentId,clearSession";
    }
}