
package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class AjaxCallbackViewParams extends SimpleViewParameters {

	public String[] sortable;
	public Boolean removeAttachment;
	public String refId;
	
	public AjaxCallbackViewParams() {}

	//For sorting
    public AjaxCallbackViewParams(String viewId, String[] sortable){
    		super(viewId);
	        this.sortable = sortable;
    }
    
    //for removing attachments
    public AjaxCallbackViewParams(String viewId, Boolean removeAttachment, String refId) {
    	super(viewId);
    	this.removeAttachment = removeAttachment;
    	this.refId = refId;
    }
}