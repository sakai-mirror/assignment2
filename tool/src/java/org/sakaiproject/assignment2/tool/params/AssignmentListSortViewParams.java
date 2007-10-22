

package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import org.sakaiproject.assignment2.tool.params.PagerViewParams;

public class AssignmentListSortViewParams extends PagerViewParams {

	public String sort_by;
	public String sort_dir;
	
	public AssignmentListSortViewParams() {}

	public AssignmentListSortViewParams(String viewId) {
		super(viewId);
	}
	
    public AssignmentListSortViewParams(String viewId, String sort_by, String sort_dir) {
    		super(viewId);
	        this.sort_by = sort_by;
	        this.sort_dir = sort_dir;
    }
    
    public AssignmentListSortViewParams(String viewId, String sort_by, String sort_dir, String currentStart, String currentCount) {
		super(viewId, currentStart, currentCount);
        this.sort_by = sort_by;
        this.sort_dir = sort_dir;
    }
    
	public String getParseSpec() {
		// include a comma delimited list of the public properties in this class
		return super.getParseSpec() + ",sort_by,sort_dir,currentStart,currentCount";
	}
}