

package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import org.sakaiproject.assignment2.tool.params.PagerViewParams;

public class AssignmentGradeAssignmentViewParams extends SortPagerViewParams {

	public Long assignmentId;
	
	public AssignmentGradeAssignmentViewParams() {}

	public AssignmentGradeAssignmentViewParams(String viewId) {
		super(viewId);
	}
	
    public AssignmentGradeAssignmentViewParams(String viewId, String sort_by, String sort_dir) {
    		super(viewId, sort_by, sort_dir);
    }
    
    public AssignmentGradeAssignmentViewParams(String viewId, String sort_by, String sort_dir, String currentStart, String currentCount) {
		super(viewId, currentStart, currentCount, sort_by, sort_dir);
    }
    
    public AssignmentGradeAssignmentViewParams(String viewId, String sort_by, String sort_dir, String currentStart, String currentCount, Long assignmentId) {
		super(viewId, currentStart, currentCount, sort_by, sort_dir);
        this.assignmentId = assignmentId;
    }
    
	public String getParseSpec() {
		// include a comma delimited list of the public properties in this class
		return super.getParseSpec() + ",assignmentId";
	}
}