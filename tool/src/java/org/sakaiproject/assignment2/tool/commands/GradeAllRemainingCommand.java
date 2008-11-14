package org.sakaiproject.assignment2.tool.commands;

import java.util.List;

import org.sakaiproject.assignment2.exception.AssignmentNotFoundException;
import org.sakaiproject.assignment2.exception.InvalidGradeForAssignmentException;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;

import uk.org.ponder.messageutil.TargettedMessage;
import uk.org.ponder.messageutil.TargettedMessageList;

/**
 * A small action bean that can be used to assign a grade to all students for
 * an Assignment that do not have grades yet.
 * 
 * @author sgithens
 *
 */
public class GradeAllRemainingCommand {
    
    // Dependency
    private AssignmentPermissionLogic permissionLogic;
    public void setAssignmentPermissionLogic(AssignmentPermissionLogic permissionLogic) {
        this.permissionLogic = permissionLogic;
    }
    
    // Dependency
    private AssignmentLogic assignmentLogic;
    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
        this.assignmentLogic = assignmentLogic;
    }
    
    // Dependency
    private ExternalGradebookLogic gradebookLogic;
    public void setGradebookLogic(ExternalGradebookLogic gradebookLogic) {
        this.gradebookLogic = gradebookLogic;
    }
    
    // Dependency
    private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }
    
    private TargettedMessageList messages;
    public void setMessages(TargettedMessageList messages) {
        this.messages = messages;
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
    private String grade;
    public void setGrade(String grade) {
        this.grade = grade;
    }
    public String getGrade() {
        return grade;
    }
    
    public void execute() {
        System.out.println("Executing Grade: " + assignmentId + ", " +
                grade);
        
        if (assignmentId == null) {
            throw new IllegalArgumentException("Null assignmentId param passed to GradeAllRemainingAction");
        }
        
        Assignment2 assign = assignmentLogic.getAssignmentById(assignmentId);
        
        if (assign == null) {
            throw new AssignmentNotFoundException("No assignment found for id: " + assignmentId);
        }
        
        if (grade == null || grade.trim().length() == 0) {
            messages.addMessage(new TargettedMessage("assignment2.assignment_grade.assigntoall.invalid_grade",
                    new Object[] { }, TargettedMessage.SEVERITY_ERROR));
            return;
        }
        
        String currUserId = externalLogic.getCurrentUserId();
        List<String> gradableStudents = permissionLogic.getGradableStudentsForUserForItem(currUserId, assign);
        
        if (gradableStudents != null) {
            try {
                gradebookLogic.assignGradeToUngradedStudents(assign.getContextId(), assign.getGradableObjectId(), gradableStudents, grade);
            } catch (InvalidGradeForAssignmentException igfae) {
                messages.addMessage(new TargettedMessage("assignment2.assignment_grade.assigntoall.invalid_grade",
                        new Object[] { }, TargettedMessage.SEVERITY_ERROR));
                return;
            }
        }
    }
}
