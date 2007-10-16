

package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class PagerViewParams extends SimpleViewParameters {

	public Integer pager_start;
	public Integer pager_count;
	
	public PagerViewParams() {}

	public PagerViewParams(String viewId){
		super(viewId);
	}
	
    public PagerViewParams(String viewId, int pager_start, int pager_count) {
    		super(viewId);
    		this.pager_start = pager_start;
    		this.pager_count = pager_count;
    }
}