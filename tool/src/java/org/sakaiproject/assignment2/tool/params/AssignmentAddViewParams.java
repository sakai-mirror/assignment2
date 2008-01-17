
package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;

public class AssignmentAddViewParams extends SimpleViewParameters {

	public Long assignmentId;
	
	public AssignmentAddViewParams() {}
	
	public AssignmentAddViewParams(String viewId) {
		super(viewId);
	}

    public AssignmentAddViewParams(String viewId, Long assignmentId) {
    		super(viewId);
    		this.assignmentId = assignmentId;
    }
    
    public String getParseSpec() {
    	return super.getParseSpec() + ",@1:assignmentId"; 
    }
}