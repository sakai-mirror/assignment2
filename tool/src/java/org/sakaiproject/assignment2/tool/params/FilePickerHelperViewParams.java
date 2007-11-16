package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class FilePickerHelperViewParams extends ThickboxHelperViewParams {

	public Long assignmentId;
	
	public FilePickerHelperViewParams() {}

    public FilePickerHelperViewParams(String viewId, Long assignmentId){
    		super(viewId);
	        this.assignmentId = assignmentId;
    }
    
    public FilePickerHelperViewParams(String viewId, Boolean KeepThis, Boolean TB_iframe, int height, int width, Long assignmentId){
    	super(viewId, KeepThis, TB_iframe, height, width);
    	this.assignmentId = assignmentId;
    }
    
    public String getParseSpec() {
		// include a comma delimited list of the public properties in this class
    	String temp = super.getParseSpec();
		return super.getParseSpec() + ",assignmentId";
	}
}