package org.sakaiproject.assignment2.tool.params;

public class RemoveAssignmentParams extends ThickboxHelperViewParams {
    
    public Long assignmentId;
    
    public RemoveAssignmentParams() {}
    
    public RemoveAssignmentParams(String viewid) {
        this.viewID = viewid;
    }
    
    public RemoveAssignmentParams(String viewid, Long asnnId) {
        super(viewid, Boolean.TRUE, Boolean.TRUE, 500, 700);
        this.viewID = viewid;
        this.assignmentId = asnnId;
    }
    
    public String getParseSpec() {
        // include a comma delimited list of the public properties in this class
        return super.getParseSpec() + ",assignmentId";
    }

}
