

package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import org.sakaiproject.assignment2.tool.params.PagerViewParams;

public class AssignmentGradeAssignmentViewParams extends PagerViewParams {

	public String assignment_id;
	
	public AssignmentGradeAssignmentViewParams() {}

	public AssignmentGradeAssignmentViewParams(String viewId) {
		super(viewId);
	}
	
    public AssignmentGradeAssignmentViewParams(String viewId, String assignment_id) {
    		super(viewId);
	        this.assignment_id = assignment_id;
    }
    
    public AssignmentGradeAssignmentViewParams(String viewId, String assignment_id, String currentStart, String currentCount) {
		super(viewId, currentStart, currentCount);
		this.assignment_id = assignment_id;
    }
}