package org.sakaiproject.assignment2.tool.beans;

import org.sakaiproject.assignment2.model.Assignment2;

public class PreviewAssignmentBean {
	
	private Assignment2 assignment;
	
	public void setAssignment(Assignment2 assignment){
		this.assignment = assignment;
	}
	
	public Assignment2 getAssignment(){
		return this.assignment;
	}
}