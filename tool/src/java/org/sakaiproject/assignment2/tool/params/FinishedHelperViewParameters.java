package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class FinishedHelperViewParameters extends SimpleViewParameters {

	public String value;
	
	public FinishedHelperViewParameters() {}

    public FinishedHelperViewParameters(String viewId, String value){
    		super(viewId);
	        this.value = value;
    }
}