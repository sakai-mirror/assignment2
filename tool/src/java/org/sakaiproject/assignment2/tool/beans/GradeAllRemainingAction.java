package org.sakaiproject.assignment2.tool.beans;

import java.util.List;

import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.GradeInformation;
import org.sakaiproject.assignment2.model.AssignmentSubmission;

/**
 * A small action bean that can be used to assign a grade to all students for
 * an Assignment that do not have grades yet.
 * 
 * @author sgithens
 *
 */
public class GradeAllRemainingAction {

    // Dependency
    private AssignmentSubmissionLogic submissionLogic;
    public void setSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
        this.submissionLogic = submissionLogic;
    }
    
    // Dependency
    private ExternalGradebookLogic gradebookLogic;
    public void setGradebookLogic(ExternalGradebookLogic gradebookLogic) {
        this.gradebookLogic = gradebookLogic;
    }
    
    // Dependency / Property
    private String curContext;
    public void setCurContext(String curContext) {
        this.curContext = curContext;
    }
    public String getCurContext() {
        return curContext;
    }
    
    // Property 
    private Long assignmentId;
    public void setAssignmentId(Long assignmentId) {
        this.assignmentId = assignmentId;
    }
    public Long getAssignmentId() {
        return assignmentId;
    }
    
    // Property
    private int grade;
    public void setGrade(int grade) {
        this.grade = grade;
    }
    public int getGrade() {
        return grade;
    }
    
    public void execute() {
        System.out.println("Executing Grade: " + assignmentId + ", " +
                grade);
        
        List<AssignmentSubmission> submissions = 
            submissionLogic.getViewableSubmissionsForAssignmentId(assignmentId);
        
        for (AssignmentSubmission submission: submissions) {
            GradeInformation grade = gradebookLogic.getGradeInformationForSubmission(curContext, submission);
            if (grade.getGradebookGrade() == null) {
                gradebookLogic.saveGradeAndCommentForStudent(curContext, 
                        submission.getAssignment().getGradableObjectId(), 
                        submission.getUserId(), grade+"", "");
            }
        }
    }
}
