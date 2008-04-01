package org.sakaiproject.assignment2.tool.params;

import uk.ac.cam.caret.sakai.rsf.helper.HelperViewParameters;

public class ThickboxHelperViewParams extends HelperViewParameters  {

	public Boolean keepThis;
	public Boolean TB_iframe;
	public int height;
	public int width;
	
	public ThickboxHelperViewParams() {}
	
	public ThickboxHelperViewParams(String viewId) {
		super(viewId);
	}

    public ThickboxHelperViewParams(String viewId, Boolean KeepThis, Boolean TB_iframe, int height, int width){
    		super(viewId);
	        this.keepThis = KeepThis;
	        this.TB_iframe = TB_iframe;
	        this.height = height;
	        this.width = width;
    }
    
    public String getParseSpec() {
		// include a comma delimited list of the public properties in this class
		return super.getParseSpec() + ",KeepThis,TB_iframe,height,width";
	}
}

