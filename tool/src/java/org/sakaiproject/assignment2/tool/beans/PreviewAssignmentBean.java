package org.sakaiproject.assignment2.tool.beans;

import org.sakaiproject.assignment2.model.Assignment2;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;

public class PreviewAssignmentBean {
	
	private Assignment2 assignment;
	private String OTPKey = EntityBeanLocator.NEW_PREFIX + "1";
	
	public void setAssignment(Assignment2 assignment){
		this.assignment = assignment;
	}
	
	public Assignment2 getAssignment(){
		return this.assignment;
	}
	
	public void setOTPKey(String key) {
		this.OTPKey = key;
	}
	
	public String getOTPKey() {
		return this.OTPKey;
	}
}