

package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import org.sakaiproject.assignment2.tool.params.PagerViewParams;

public class AssignmentListSortViewParams extends SortPagerViewParams {

	public Long assignmentIdToDuplicate;
	
	public AssignmentListSortViewParams() {}

	public AssignmentListSortViewParams(String viewId) {
		super(viewId);
	}
	
    public AssignmentListSortViewParams(String viewId, String sort_by, String sort_dir) {
    		super(viewId, sort_by, sort_dir);
    }
    
    public AssignmentListSortViewParams(String viewId, String sort_by, String sort_dir, String currentStart, String currentCount) {
		super(viewId, currentStart, currentCount, sort_by, sort_dir);
    }
    
    public AssignmentListSortViewParams(String viewId, String sort_by, String sort_dir, String currentStart, String currentCount, Long assignmentIdToDuplicate) {
		super(viewId, currentStart, currentCount, sort_by, sort_dir);
        this.assignmentIdToDuplicate = assignmentIdToDuplicate;
    }
    
	public String getParseSpec() {
		// include a comma delimited list of the public properties in this class
		return super.getParseSpec() + ",assignmentIdToDuplicate";
	}
}