package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class FilePickerHelperViewParams extends ThickboxHelperViewParams {

	public String otpkey;
	
	public FilePickerHelperViewParams() {}

    public FilePickerHelperViewParams(String viewId, String otpkey){
    		super(viewId);
	        this.otpkey = otpkey;
    }
    
    public FilePickerHelperViewParams(String viewId, Boolean KeepThis, Boolean TB_iframe, int height, int width, String otpkey){
    	super(viewId, KeepThis, TB_iframe, height, width);
    	this.otpkey = otpkey;
    }
    
    public String getParseSpec() {
		// include a comma delimited list of the public properties in this class
		return super.getParseSpec() + ",otpkey";
	}
}