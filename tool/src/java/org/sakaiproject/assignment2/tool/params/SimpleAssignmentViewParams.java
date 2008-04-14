
package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.tool.beans.Assignment2Bean;
import org.sakaiproject.assignment2.tool.producers.StudentSubmitProducer;
import org.sakaiproject.assignment2.tool.producers.StudentSubmitSummaryProducer;

public class SimpleAssignmentViewParams extends SimpleViewParameters implements VerifiableViewParams{

	private static final Log LOG = LogFactory.getLog(Assignment2Bean.class);
	
	public Long assignmentId;
	
	public SimpleAssignmentViewParams() {}

    public SimpleAssignmentViewParams(String viewId, Long assignmentId){
    		super(viewId);
	        this.assignmentId = assignmentId;
    }
    
    public String getParseSpec() {
    	return super.getParseSpec() + ",@1:assignmentId";
    }

	public Boolean verify()
	{
		if (StudentSubmitProducer.VIEW_ID.equals(this.viewID) && this.assignmentId == null){
			LOG.error("Null assignmentId in viewparamters while attempting to load StudentSubmitProducer");
			return Boolean.FALSE;
		}
		
		if (StudentSubmitSummaryProducer.VIEW_ID.equals(this.viewID) && this.assignmentId == null) {
			LOG.error("Null assignmentId in viewparamters while attempting to load StudentSubmitSummaryProducer");
			return Boolean.FALSE;
		}
		
		return Boolean.TRUE;
	}
}