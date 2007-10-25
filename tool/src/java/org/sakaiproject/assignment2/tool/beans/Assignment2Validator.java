package org.sakaiproject.assignment2.tool.beans;

import org.sakaiproject.assignment2.model.Assignment2;

import uk.org.ponder.messageutil.TargettedMessage;
import uk.org.ponder.messageutil.TargettedMessageList;

public class Assignment2Validator  {
	
	public boolean validate(Assignment2 assignment, TargettedMessageList messages) {
	  
		boolean valid = true;
		String key = "";
		if (assignment.getAssignmentId() == null){
			key = "new 1";
		} else {
			key = assignment.getAssignmentId().toString();
		}
		
		if (assignment.getTitle() == null || assignment.getTitle().equals("")) {
			messages.addMessage(new TargettedMessage("assignment2.assignment_title_empty",
					new Object[] { assignment.getTitle() }, "Assignment2." + key + ".title"));
			valid = false;
		}
		
		return valid;
	}
  
}