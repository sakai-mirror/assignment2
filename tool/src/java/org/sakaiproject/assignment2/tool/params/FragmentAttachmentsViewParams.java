package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class FragmentAttachmentsViewParams extends SimpleViewParameters {
	
	public String otpkey;
	
	public FragmentAttachmentsViewParams(){}
	
	 public FragmentAttachmentsViewParams(String viewId, String otpkey){
 		super(viewId);
	    this.otpkey = otpkey;
	 }
	
}