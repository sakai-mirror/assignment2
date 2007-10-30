
package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class AssignmentAddViewParams extends SimpleViewParameters {

	public Long assignmentId;
	public Boolean fromPreview = false;
	
	public AssignmentAddViewParams() {}
	
	public AssignmentAddViewParams(String viewId) {
		super(viewId);
	}

    public AssignmentAddViewParams(String viewId, Long assignmentId) {
    		super(viewId);
    		this.assignmentId = assignmentId;
    }
    
    public AssignmentAddViewParams(String viewId, Long assignmentId, Boolean fromPreview) {
		super(viewId);
		this.assignmentId = assignmentId;
		this.fromPreview = fromPreview;
    }
}