package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class FragmentGradebookDetailsViewParams extends SimpleViewParameters {
	
	public Long submissionId;
	
	public FragmentGradebookDetailsViewParams(){}
	
	public FragmentGradebookDetailsViewParams(String viewId, Long submissionId, String userId) {
		super(viewId);
		this.submissionId = submissionId;
	}
}