package org.sakaiproject.assignment2.tool.beans;

import org.sakaiproject.assignment2.model.Assignment2;

public class Assignment2Validator  {
	
  public void setAssignment(Assignment2 newAssignment) {
  if (newAssignment.getTitle() == null || newAssignment.getTitle().equals("")) {
      throw new IllegalArgumentException("title_required");
    }
  }
  
}