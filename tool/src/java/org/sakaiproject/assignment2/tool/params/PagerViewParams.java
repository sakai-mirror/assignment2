
package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class PagerViewParams extends SimpleViewParameters {

	public String current_start = "0";
	public String current_count = "5";
	
	public PagerViewParams() {}
	
	public PagerViewParams(String viewId) {
		super(viewId);
	}

    public PagerViewParams(String viewId, String currentStart, String currentCount){
    		super(viewId);
	        this.current_start = currentStart;
	        this.current_count = currentCount;
    }
    
	public String getParseSpec() {
		// include a comma delimited list of the public properties in this class
		return super.getParseSpec() + ",current_start,current_count";
	}
}