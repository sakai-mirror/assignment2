package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class FragmentAttachmentsViewParams extends SimpleViewParameters {
	
	public String otpkey;
	public Boolean remove;
	
	public FragmentAttachmentsViewParams(){}
	
	 public FragmentAttachmentsViewParams(String viewId, String otpkey, Boolean remove){
 		super(viewId);
	    this.otpkey = otpkey;
	    this.remove = remove;
	 }
	
}