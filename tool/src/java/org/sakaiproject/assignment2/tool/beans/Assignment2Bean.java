package org.sakaiproject.assignment2.tool.beans;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.tool.beans.Assignment2Validator;
import org.sakaiproject.assignment2.exception.ConflictingAssignmentNameException;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.messageutil.TargettedMessage;
import uk.org.ponder.messageutil.TargettedMessageList;

import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class Assignment2Bean {
	
	public Assignment2 assignment = new Assignment2();
	public Date openDate;
	
	private static final String REMOVE = "remove";
	private static final String BACK_TO_LIST = "back_to_list";
	private static final String POST = "post";
	private static final String PREVIEW = "preview";
	private static final String SAVE_DRAFT = "save_draft";
	private static final String EDIT = "edit";
	private static final String CANCEL = "cancel";
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
	@SuppressWarnings("unchecked")
	public void setEntityBeanLocator(EntityBeanLocator entityBeanLocator) {
		this.OTPMap = entityBeanLocator.getDeliveredBeans();
	}
	
	private ExternalLogic externalLogic;
	public void setExternalLogic(ExternalLogic externalLogic) {
		this.externalLogic = externalLogic;
	}
	
	private PreviewAssignmentBean previewAssignmentBean;
	public void setPreviewAssignmentBean (PreviewAssignmentBean previewAssignmentBean) {
		this.previewAssignmentBean = previewAssignmentBean;
	}
	
	private MessageLocator messageLocator;
	public void setMessageLocator (MessageLocator messageLocator) {
		this.messageLocator = messageLocator;
	}
	
	public String processActionBackToList() {
		return BACK_TO_LIST;
	}
	
	public String processActionPost() {
		for (String key : OTPMap.keySet()) {
			Assignment2 assignment = OTPMap.get(key);
			 return internalProcessPost(assignment, key);
		}
		return POST;
	}
	
	public String processActionPreviewPost(){
		Assignment2 assignment = previewAssignmentBean.getAssignment();
		String key = "";
		if (assignment.getAssignmentId() != null){
			key += assignment.getAssignmentId().toString();
		} else {
			key += EntityBeanLocator.NEW_PREFIX + "1";
		}
		
		String result = internalProcessPost(assignment, key);
		//clear session scoped assignment
		if (result.equals(POST)){
			previewAssignmentBean.setAssignment(null);
		}
		return result;
	}
	
	private String internalProcessPost(Assignment2 assignment, String key){
		assignment.setDraft(Boolean.FALSE);
		assignment.setCreateTime(new Date());
		assignment.setModifiedTime(new Date());
		assignment.setModifiedBy(externalLogic.getCurrentUserId());
		
		//Since in the UI, the select box bound to the gradableObjectId is always present
		// we need to manually remove this value if the assignment is ungraded
		if (assignment.isUngraded()) {
			assignment.setGradableObjectId(null);
		}
		
		//REMOVE THESE
		assignment.setGroupSubmission(Boolean.FALSE);
		assignment.setRestrictedToGroups(Boolean.FALSE);
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
				messages.addMessage(new TargettedMessage("assignment2.assignment_save",
						new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_INFO));
			}
		} else {
			messages.addMessage(new TargettedMessage("assignment2.assignment_post_error"));
			return FAILURE;
		}
		return POST;
	}
	
	
	public String processActionPreview() {
		for (String key : OTPMap.keySet()) {
			Assignment2 assignment = OTPMap.get(key);
			previewAssignmentBean.setAssignment(assignment);
		}
		return PREVIEW;
	}
	
	public String processActionEdit() {
		return EDIT;
	}
	
	public String processActionSaveDraft() {
		String currentUserId = externalLogic.getCurrentUserId();
		for (String key : OTPMap.keySet()) {
			Assignment2 assignment = OTPMap.get(key);

			assignment.setDraft(Boolean.TRUE);
			assignment.setCreateTime(new Date());
			assignment.setModifiedTime(new Date());
			assignment.setModifiedBy(externalLogic.getCurrentUserId());
			
			//REMOVE THESE
			assignment.setUngraded(Boolean.FALSE);
			assignment.setGroupSubmission(Boolean.FALSE);
			assignment.setRestrictedToGroups(Boolean.FALSE);
			assignment.setNotificationType(0);
			assignment.setAllowResubmitUntilDue(Boolean.FALSE);
			
			//start the validator
			Assignment2Validator validator = new Assignment2Validator();
			if (validator.validate(assignment, messages)){
				//Validation Passed!
				try {
					logic.saveAssignment(assignment);
				} catch( ConflictingAssignmentNameException e){
					messages.addMessage(new TargettedMessage("assignment2.assignment_save_draft.conflicting_assignment_name",
							new Object[] { assignment.getTitle() }, "Assignment2." + key + ".title"));
					return FAILURE;
				}
				
				//set Messages
				messages.addMessage(new TargettedMessage("assignment2.assignment_save_draft",
					new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_INFO));
				
			} else {
				messages.addMessage(new TargettedMessage("assignment2.assignment_save_draft_error"));
				return FAILURE;
			}
		}
		return SAVE_DRAFT;
	}
	
	public String processActionCancel() {
		return CANCEL;
	}
	
	public String processActionRemove() {
		String currentUserId = externalLogic.getCurrentUserId();
		List<Assignment2> entries = logic.getViewableAssignments();
		int assignmentsRemoved = 0;
		for (Assignment2 assignment : entries) {
			if (selectedIds.get(assignment.getAssignmentId().toString()) == Boolean.TRUE){
				assignment.setModifiedTime(new Date());
				assignment.setModifiedBy(externalLogic.getCurrentUserId());
				logic.deleteAssignment(assignment);
				assignmentsRemoved++;
			}
		}
		messages.addMessage( new TargettedMessage("assignment2.assignments_remove",
				new Object[] { new Integer(assignmentsRemoved) },
		        TargettedMessage.SEVERITY_INFO));
		return REMOVE;
	}
	
	public void createDuplicate(Long assignmentId) {
		Assignment2Creator creator = new Assignment2Creator();
		creator.setExternalLogic(externalLogic);
		creator.setMessageLocator(messageLocator);
		Assignment2 duplicate = creator.createDuplicate(logic.getAssignmentById(assignmentId));
		try {
			logic.saveAssignment(duplicate);
		} catch(ConflictingAssignmentNameException e){
			messages.addMessage(new TargettedMessage("assignment2.assignment_post.duplicate_conflicting_assignment_name",
					new Object[]{ duplicate.getTitle() }));
			return;
		} catch (SecurityException e) {
			messages.addMessage(new TargettedMessage("assignment2.assignment_post.security_exception"));
			return;
		}
		messages.addMessage(new TargettedMessage("assignment2.assignment_post.duplicate",
			new Object[] {duplicate.getTitle() }));
	}
}