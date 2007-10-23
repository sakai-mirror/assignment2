package org.sakaiproject.assignment2.tool.beans;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.TargettedMessage;
import uk.org.ponder.messageutil.TargettedMessageList;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class Assignment2Bean {
	
	private static final String DELETE_ITEMS = "delete-items";
	
	public Map selectedIds = new HashMap();
	
    private TargettedMessageList messages;
    public void setMessages(TargettedMessageList messages) {
    	this.messages = messages;
    }
	
	private AssignmentLogic logic;
	public void setLogic(AssignmentLogic logic) {
		this.logic = logic;
	}
	
	private ExternalLogic externalLogic;
	public void setExternalLogic(ExternalLogic externalLogic) {
		this.externalLogic = externalLogic;
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
		messages.addMessage( new TargettedMessage("assignments2.assignments_remove",
				new Object[] { new Integer(assignmentsRemoved) },
		        TargettedMessage.SEVERITY_INFO));
		return DELETE_ITEMS;
	}
}