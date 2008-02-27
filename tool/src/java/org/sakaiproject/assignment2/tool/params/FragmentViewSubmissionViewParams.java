package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class FragmentViewSubmissionViewParams extends SimpleViewParameters {
	
	public Long submissionVersionId;
	
	public FragmentViewSubmissionViewParams(){}
	
	public FragmentViewSubmissionViewParams(String viewId, Long submissionVersionId) {
		super(viewId);
		this.submissionVersionId = submissionVersionId;
	}
	
	public String getParseSpec() {
		return super.getParseSpec() + ",@1:submissionVersionId";
	}
}