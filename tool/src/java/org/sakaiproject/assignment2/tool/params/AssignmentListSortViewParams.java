

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
    
    public AssignmentListSortViewParams(String viewId, String sort_by, String sort_dir, int currentStart, int currentCount) {
		super(viewId, sort_by, sort_dir, currentStart, currentCount);
    }
    
    public AssignmentListSortViewParams(String viewId, String sort_by, String sort_dir, int currentStart, int currentCount, Long assignmentIdToDuplicate) {
    	super(viewId, sort_by, sort_dir, currentStart, currentCount);
        this.assignmentIdToDuplicate = assignmentIdToDuplicate;
    }
    
	public String getParseSpec() {
		// include a comma delimited list of the public properties in this class
		return super.getParseSpec() + ",assignmentIdToDuplicate";
	}
}