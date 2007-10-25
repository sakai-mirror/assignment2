package org.sakaiproject.assignment2.tool.beans;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.tool.beans.Assignment2Validator;
import org.sakaiproject.assignment2.exception.ConflictingAssignmentNameException;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.TargettedMessage;
import uk.org.ponder.messageutil.TargettedMessageList;

import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class Assignment2Bean {
	
	private static final String REMOVE = "remove";
	private static final String POST = "post";
	private static final String FAILURE = "failure";
	
	public Map selectedIds = new HashMap();
	
    private TargettedMessageList messages;
    public void setMessages(TargettedMessageList messages) {
    	this.messages = messages;
    }
	
	private AssignmentLogic logic;
	public void setLogic(AssignmentLogic logic) {
		this.logic = logic;
	}
	
	private Map<String, Assignment2> OTPMap;
	public void setEntityBeanLocator(EntityBeanLocator entityBeanLocator) {
		this.OTPMap = entityBeanLocator.getDeliveredBeans();
	}
	
	private ExternalLogic externalLogic;
	public void setExternalLogic(ExternalLogic externalLogic) {
		this.externalLogic = externalLogic;
	}
	
	public String processActionPost() {
		String currentUserId = externalLogic.getCurrentUserId();
		for (String key : OTPMap.keySet()) {
			Assignment2 assignment = OTPMap.get(key);

			assignment.setDraft(Boolean.FALSE);
			assignment.setCreateTime(new Date());
			
			//REMOVE THESE
			assignment.setUngraded(Boolean.FALSE);
			assignment.setGroupSubmission(Boolean.FALSE);
			assignment.setRestrictedToGroups(Boolean.FALSE);
			assignment.setHonorPledge(Boolean.FALSE);
			assignment.setSubmissionType(0);
			assignment.setNotificationType(0);
			assignment.setAllowResubmitUntilDue(Boolean.FALSE);
			
			//start the validator
			Assignment2Validator validator = new Assignment2Validator();
			if (validator.validate(assignment, messages)){
				//Validation Passed!
				try {
					logic.saveAssignment(assignment);
				} catch( ConflictingAssignmentNameException e){
					messages.addMessage(new TargettedMessage("assignment2.assignment_post.conflicting_assignment_name",
							new Object[] { assignment.getTitle() }, "Assignment2." + key + ".title"));
					return FAILURE;
				}
				
				//set Messages
				if (key.startsWith(EntityBeanLocator.NEW_PREFIX)) {
					messages.addMessage(new TargettedMessage("assignment2.assignment_post",
							new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_INFO));
				}
				else {
					messages.addMessage(new TargettedMessage("assignment2.assignment_saved",
							new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_INFO));
				}
			} else {
				messages.addMessage(new TargettedMessage("assignment2.assignment_post_error"));
				return FAILURE;
			}
		}
		return POST;
	}
	
	public String processActionRemove() {
		String currentUserId = externalLogic.getCurrentUserId();
		List<Assignment2> entries = logic.getViewableAssignments(currentUserId);
		int assignmentsRemoved = 0;
		for (Assignment2 assignment : entries) {
			if (selectedIds.get(assignment.getAssignmentId().toString()) == Boolean.TRUE){
				logic.deleteAssignment(assignment);
				assignmentsRemoved++;
			}
		}
		messages.addMessage( new TargettedMessage("assignment2.assignments_remove",
				new Object[] { new Integer(assignmentsRemoved) },
		        TargettedMessage.SEVERITY_INFO));
		return REMOVE;
	}
}