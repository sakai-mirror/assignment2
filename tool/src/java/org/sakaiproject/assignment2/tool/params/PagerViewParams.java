
package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class PagerViewParams extends SimpleViewParameters {

	public String currentStart = "0";
	public String currentCount = "5";
	
	public PagerViewParams() {}
	
	public PagerViewParams(String viewId) {
		super(viewId);
	}

    public PagerViewParams(String viewId, String currentStart, String currentCount){
    		super(viewId);
	        this.currentStart = currentStart;
	        this.currentCount = currentCount;
    }
}