package org.sakaiproject.assignment2.tool.beans;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.user.api.User;

public class MarkTodoBean {
    private static Log log = LogFactory.getLog(MarkTodoBean.class);

    private AssignmentSubmissionLogic assignmentSubmissionLogic;
    public void setAssignmentSubmissionLogic(
            AssignmentSubmissionLogic assignmentSubmissionLogic) {
        this.assignmentSubmissionLogic = assignmentSubmissionLogic;
    }
    
    private User currentUser;
    public void setCurrentUser(User currentUser) {
        this.currentUser = currentUser;
    }
    
    private Long assignmentId;
    public void setAssignmentId(Long assignmentId) {
        this.assignmentId = assignmentId;
    }
    public Long getAssignmentId() {
        return assignmentId;
    }
    
    private boolean checkTodo;
    public void setCheckTodo(boolean checkTodo) {
        this.checkTodo = checkTodo;
    }
    public boolean isCheckTodo() {
        return checkTodo;
    }
    
    public String markTodo() {
        log.warn("Marking Assignment Item: " + assignmentId + " , " + checkTodo);
        
        Map<Long, Boolean> completed = new HashMap<Long, Boolean>();
        completed.put(assignmentId, new Boolean(checkTodo));
        assignmentSubmissionLogic.markAssignmentsAsCompleted(currentUser.getId(), completed);
        
        return "marked";
    }
}
