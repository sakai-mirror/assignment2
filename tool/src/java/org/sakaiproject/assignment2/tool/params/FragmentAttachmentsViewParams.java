package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class FragmentAttachmentsViewParams extends SimpleViewParameters {
	
	public String attachmentRef;
	public int idOffset;
	public Boolean remove;
	
	public FragmentAttachmentsViewParams(){}
	
	 public FragmentAttachmentsViewParams(String viewId, Boolean remove){
 		super(viewId);
		    this.remove = remove;
	 }
}