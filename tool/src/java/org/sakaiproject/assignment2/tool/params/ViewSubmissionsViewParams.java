package org.sakaiproject.assignment2.tool.params;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.tool.beans.Assignment2Bean;
import org.sakaiproject.assignment2.tool.producers.ViewSubmissionsProducer;

public class ViewSubmissionsViewParams extends SortPagerViewParams implements VerifiableViewParams {

	private static final Log LOG = LogFactory.getLog(Assignment2Bean.class);
	
	public Long assignmentId;
	
	public ViewSubmissionsViewParams() {}

	public ViewSubmissionsViewParams(String viewId) {
		super(viewId);
	}
	
	public ViewSubmissionsViewParams(String viewId, Long assignmentId) {
		super(viewId);
		this.assignmentId = assignmentId;
	}
	
    public ViewSubmissionsViewParams(String viewId, String sort_by, String sort_dir) {
    		super(viewId, sort_by, sort_dir);
    }
    
    public ViewSubmissionsViewParams(String viewId, String sort_by, String sort_dir, int currentStart, int currentCount) {
    	super(viewId, sort_by, sort_dir, currentStart, currentCount);
    }
    
    public ViewSubmissionsViewParams(String viewId, String sort_by, String sort_dir, int currentStart, int currentCount, Long assignmentId) {
    	super(viewId, sort_by, sort_dir, currentStart, currentCount);
        this.assignmentId = assignmentId;
    }
    
	public String getParseSpec() {
		// include a comma delimited list of the public properties in this class
		return super.getParseSpec() + ",@1:assignmentId";
	}

	public Boolean verify()
	{
		if (ViewSubmissionsProducer.VIEW_ID.equals(this.viewID) && this.assignmentId == null) {
			LOG.error("Null assignmentId in viewparameters while attempting to load ViewSubmissionsProducer");
			return Boolean.FALSE;
		}
		return Boolean.TRUE;
	}
}