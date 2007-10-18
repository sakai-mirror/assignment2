
package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class SimpleAssignmentViewParams extends SimpleViewParameters {

	public String assignmentId;
	
	public SimpleAssignmentViewParams() {}

    public SimpleAssignmentViewParams(String viewId, String assignmentId){
    		super(viewId);
	        this.assignmentId = assignmentId;
    }
}