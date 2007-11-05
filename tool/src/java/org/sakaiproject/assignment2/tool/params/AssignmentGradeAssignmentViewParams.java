

package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import org.sakaiproject.assignment2.tool.params.PagerViewParams;

public class AssignmentGradeAssignmentViewParams extends PagerViewParams {

	public Long assignmentId;
	public String sort_by;
	public String sort_dir;
	
	public AssignmentGradeAssignmentViewParams() {}

	public AssignmentGradeAssignmentViewParams(String viewId) {
		super(viewId);
	}
	
    public AssignmentGradeAssignmentViewParams(String viewId, Long assignment_id) {
    		super(viewId);
	        this.assignmentId = assignment_id;
    }
    
    public AssignmentGradeAssignmentViewParams(String viewId, Long assignment_id, String currentStart, String currentCount) {
		super(viewId, currentStart, currentCount);
		this.assignmentId = assignment_id;
    }
    
    public AssignmentGradeAssignmentViewParams(String viewId, Long assignment_id, String sortBy, String sortDir, String currentStart, String currentCount) {
		super(viewId, currentStart, currentCount);
		this.assignmentId = assignment_id;
		this.sort_by = sortBy;
		this.sort_dir = sortDir;
    }
    
    public String getParseSpec() {
		// include a comma delimited list of the public properties in this class
		return super.getParseSpec() + ",assignmentId,sort_by,sort_dir,currentStart,currentCount";
	}
}