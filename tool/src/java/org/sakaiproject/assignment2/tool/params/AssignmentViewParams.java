
package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;

public class AssignmentViewParams extends SimpleViewParameters {

	public Long assignmentId;
	
	public AssignmentViewParams() {}
	
	public AssignmentViewParams(String viewId) {
		super(viewId);
	}

    public AssignmentViewParams(String viewId, Long assignmentId) {
    		super(viewId);
    		this.assignmentId = assignmentId;
    }
    
    public String getParseSpec() {
    	return super.getParseSpec() + ",@1:assignmentId"; 
    }
}