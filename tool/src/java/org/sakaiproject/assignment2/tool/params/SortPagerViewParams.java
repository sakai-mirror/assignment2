package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import org.sakaiproject.assignment2.tool.params.PagerViewParams;

public class SortPagerViewParams extends PagerViewParams {

	public String sort_by;
	public String sort_dir;
	
	public SortPagerViewParams() {}

	public SortPagerViewParams(String viewId) {
		super(viewId);
	}
	
    public SortPagerViewParams(String viewId, String sort_by, String sort_dir) {
    		super(viewId);
	        this.sort_by = sort_by;
	        this.sort_dir = sort_dir;
    }
    
    public SortPagerViewParams(String viewId, String sort_by, String sort_dir, int currentStart, int currentCount) {
		super(viewId, currentStart, currentCount);
        this.sort_by = sort_by;
        this.sort_dir = sort_dir;
        //this.assignmentIdToDuplicate = null;
    }
    
    
	public String getParseSpec() {
		// include a comma delimited list of the public properties in this class
		return super.getParseSpec() + ",sort_by,sort_dir,currentStart,currentCount";
	}
}