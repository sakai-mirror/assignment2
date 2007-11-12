package org.sakaiproject.assignment2.logic;


/**
 * This is the implementation for logic which uses Sakai's event implementation
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public interface ExternalEventLogic {
	
    
    public static final String EVENT_ASSIGN_CREATE = "assignment2.assignment.create";
    public static final String EVENT_ASSIGN_UPDATE = "assignment2.assignment.update";
    public static final String EVENT_ASSIGN_DELETE = "assignment2.assignment.delete";
    public static final String EVENT_SUB_SUBMIT = "assignment2.submission.submit";
    public static final String EVENT_SUB_GRADE = "assignment2.submission.grade";
    public static final String EVENT_SUB_RETURN = "assignment2.submission.return";
	
    /**
    * Post a sakai event
    * @param message
    * @param referenceObject
    */
   public void postEvent(String message, String referenceObject); 
}
