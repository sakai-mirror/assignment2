
package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class AjaxCallbackViewParams extends SimpleViewParameters {

	public String[] sortable;
	
	public AjaxCallbackViewParams() {}

    public AjaxCallbackViewParams(String viewId, String[] sortable){
    		super(viewId);
	        this.sortable = sortable;
    }
}