
package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class AssignmentListReorderAjaxParams extends SimpleViewParameters {

	public String[] assignmentId;
	
	public AssignmentListReorderAjaxParams() {}

    public AssignmentListReorderAjaxParams(String viewId, String[] assignmentId){
    		super(viewId);
	        this.assignmentId = assignmentId;
    }
}