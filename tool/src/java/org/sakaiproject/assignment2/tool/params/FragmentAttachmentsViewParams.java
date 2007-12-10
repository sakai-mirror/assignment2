package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class FragmentAttachmentsViewParams extends SimpleViewParameters {
	
	public String otpkey;
	public int attachmentSetType;
	public Boolean remove;
	
	public static final int ASSIGNMENT_ATTACHMENT = 0;
	public static final int ASSIGNMENT_SUBMISSION_ATTACHMENT = 1;
	public static final int ASSIGNMENT_FEEDBACK_ATTACHMENT = 2;
	
	public FragmentAttachmentsViewParams(){}
	
	 public FragmentAttachmentsViewParams(String viewId, String otpkey, Boolean remove){
 		super(viewId);
	    this.otpkey = otpkey;
	    this.remove = remove;
	    this.attachmentSetType = ASSIGNMENT_ATTACHMENT;
	 }
	 
	 public FragmentAttachmentsViewParams(String viewId, String otpkey, int attachmentSetType, Boolean remove) {
		 super(viewId);
		 this.otpkey = otpkey;
		 this.remove = remove;
		 this.attachmentSetType = attachmentSetType;
	 }
	
}