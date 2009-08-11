package org.sakaiproject.assignment2.tool.params;

public class StudentSubmissionParams extends SimpleAssignmentViewParams {
    public boolean previewsubmission;
    public boolean previewAsnnAsStudent;

    public StudentSubmissionParams() {}

    public StudentSubmissionParams(String viewId, Long assignmentId) {
        this.viewID = viewId;
        this.assignmentId = assignmentId;
    }

    public StudentSubmissionParams(String viewid, Long assignmentId, boolean preview) {
        this.viewID = viewid;
        this.assignmentId = assignmentId;
        this.previewsubmission = preview;
    }

    public StudentSubmissionParams(String viewid, Long assignmentId, boolean preview, boolean previewAsnnAsStudent) {
        this.viewID = viewid;
        this.assignmentId = assignmentId;
        this.previewsubmission = preview;
        this.previewAsnnAsStudent = previewAsnnAsStudent;
    }

    public String getParseSpec() {
        return super.getParseSpec() + ",previewsubmission,previewAsnnAsStudent";
    }
}
