package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.tool.beans.Assignment2Bean;
import org.sakaiproject.assignment2.tool.producers.GradeProducer;

public class GradeViewParams extends SimpleViewParameters implements VerifiableViewParams {
	
	private static final Log LOG = LogFactory.getLog(Assignment2Bean.class);
	
	public Long assignmentId;
	public String userId;
	public Long submissionId;
	
	public GradeViewParams(){}
	
	public GradeViewParams(String viewId, Long assignmentId, String userId){
		super(viewId);
		this.assignmentId = assignmentId;
		this.userId = userId;
	}
	
	public GradeViewParams(String viewId, Long assignmentId, String userId, Long submissionId) {
		super(viewId);
		this.assignmentId = assignmentId;
		this.userId = userId;
		this.submissionId = submissionId;
	}
	
	public String getParseSpec(){
		return super.getParseSpec() + ",@1:assignmentId,@2:userId,submissionId";
	}

	public Boolean verify()
	{
		if (GradeProducer.VIEW_ID.equals(this.viewID) && (this.assignmentId == null || this.userId == null)) {
			LOG.error("Null assignmentId or userId in viewparameters while attempting to load GradeProducer");
			return Boolean.FALSE;
		}
		return Boolean.TRUE;
	}
	
}