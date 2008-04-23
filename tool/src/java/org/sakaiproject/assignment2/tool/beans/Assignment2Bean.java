/**********************************************************************************
 * $URL$
 * $Id$
 ***********************************************************************************
 *
 * Copyright (c) 2007, 2008 The Sakai Foundation.
 *
 * Licensed under the Educational Community License, Version 1.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.opensource.org/licenses/ecl1.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 **********************************************************************************/

package org.sakaiproject.assignment2.tool.beans;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.tool.beans.Assignment2Validator;
import org.sakaiproject.user.api.UserNotDefinedException;
import org.sakaiproject.assignment2.exception.AnnouncementPermissionException;
import org.sakaiproject.assignment2.exception.StaleObjectModificationException;
import org.sakaiproject.exception.IdUnusedException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.messageutil.TargettedMessage;
import uk.org.ponder.messageutil.TargettedMessageList;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;

public class Assignment2Bean {
	
	public Assignment2 assignment = new Assignment2();
	public Date openDate;
	
	private static final Log LOG = LogFactory.getLog(Assignment2Bean.class);
	
	private static final String REMOVE = "remove";
	private static final String BACK_TO_LIST = "back_to_list";
	private static final String POST = "post";
	private static final String PREVIEW = "preview";
	private static final String SAVE_DRAFT = "save_draft";
	private static final String EDIT = "edit";
	private static final String CANCEL = "cancel";
	private static final String FAILURE = "failure";
	
	
	private Boolean requireAcceptUntil;
	public void setRequireAcceptUntil(Boolean requireAcceptUntil) {
		this.requireAcceptUntil = requireAcceptUntil;
	}
	
	public Boolean requireDueDate;
	public void setRequireDueDate(Boolean requireDueDate) {
		this.requireDueDate = requireDueDate;
	}
	
	public Map<String, Boolean> selectedIds = new HashMap<String, Boolean>();
	public String restrictedToGroups;
	
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
	public void setAssignment2EntityBeanLocator(EntityBeanLocator entityBeanLocator) {
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
	
	private NotificationBean notificationBean;
	public void setNotificationBean (NotificationBean notificationBean) {
		this.notificationBean = notificationBean;
	}
	
	private AttachmentBean attachmentBean;
	public void setAttachmentBean(AttachmentBean attachmentBean) {
		this.attachmentBean = attachmentBean;
	}
	
	public String processActionBackToList() {
		return BACK_TO_LIST;
	}
		
	public String processActionPost() {
		String result = POST;
		for (String key : OTPMap.keySet()) {
			Assignment2 assignment = OTPMap.get(key);
			 result = internalProcessPost(assignment, key, Boolean.FALSE);
		
			 // Notify students
			 if (result.equals(POST))
			 {
				 try
				 {
					 notificationBean.notifyStudentsOfNewAssignment(assignment);
				 }catch (IdUnusedException e)
				 {
					 messages.addMessage(new TargettedMessage("assignment2.student-submit.error.unexpected",
						new Object[] {e.getLocalizedMessage()}, TargettedMessage.SEVERITY_ERROR));
				 }catch (UserNotDefinedException e)
				 {
					 messages.addMessage(new TargettedMessage("assignment2.student-submit.error.unexpected",
						new Object[] {e.getLocalizedMessage()}, TargettedMessage.SEVERITY_ERROR));
				 }
			 }
		}
		return result;
	}
	
	private String internalProcessPost(Assignment2 assignment, String key, Boolean draft){
		Boolean errorFound = false;
		
		assignment.setDraft(draft);
		
		if (this.requireAcceptUntil == null || this.requireAcceptUntil == Boolean.FALSE) {
			assignment.setAcceptUntilTime(null);
		}
		
		if (this.requireDueDate == null || this.requireDueDate == Boolean.FALSE) {
			assignment.setDueDate(null);
		}

		Set<AssignmentAttachment> set = new HashSet<AssignmentAttachment>();
		for (String ref : attachmentBean.attachmentRefs) {
			if (ref != null) {
				AssignmentAttachment aa = new AssignmentAttachment();
				aa.setAttachmentReference(ref);
				set.add(aa);
			}
		}
		assignment.setAttachmentSet(set);

    	//Since in the UI, the select box bound to the gradableObjectId is always present
		// we need to manually remove this value if the assignment is ungraded
		if (assignment.isUngraded()) {
			assignment.setGradableObjectId(null);
		}
		
		//REMOVE THESE - TODO
		assignment.setNotificationType(0);
		//END REMOVE THESE 
		
		//do groups
		Set<AssignmentGroup> newGroups = new HashSet<AssignmentGroup>();
		if (this.restrictedToGroups != null && restrictedToGroups.equals(Boolean.TRUE.toString())){
			//now add any new groups
			if (assignment.getAssignmentGroupSet() != null) {
				newGroups.addAll(assignment.getAssignmentGroupSet());
			} 
			
			Set<AssignmentGroup> remGroups = new HashSet<AssignmentGroup>();
			for (String selectedId : selectedIds.keySet()) {
				if (selectedIds.get(selectedId) == Boolean.TRUE) {
					//Then add the group
					AssignmentGroup newGroup = new AssignmentGroup();
					newGroup.setAssignment(assignment);
					newGroup.setGroupId(selectedId);
					newGroups.add(newGroup);
				} else if (selectedIds.get(selectedId) == Boolean.FALSE) {
					//then remove the group
					for (AssignmentGroup ag : newGroups) {
						if (ag.getGroupId().equals(selectedId)) {
							remGroups.add(ag);
						}
					}
				}
			}
			newGroups.removeAll(remGroups);
		}

		if (this.restrictedToGroups != null && restrictedToGroups.equals(Boolean.TRUE.toString()) && newGroups.size() < 1){
			messages.addMessage(new TargettedMessage("assignment2.assignment_post.no_groups"));
			errorFound = true;
		}
		assignment.setAssignmentGroupSet(newGroups);
		
		
		//start the validator
		Assignment2Validator validator = new Assignment2Validator();
		if (validator.validate(assignment, messages) && !errorFound){
			//Validation Passed!
			try {
				
				logic.saveAssignment(assignment);
				
			} catch (AnnouncementPermissionException ape) {
				if (LOG.isDebugEnabled()) LOG.debug("Announcement could not " +
				"be updated b/c user does not have perm in annc tool");
				//TODO display to user?
			}
			
			//set Messages
			if (draft) {
				messages.addMessage(new TargettedMessage("assignment2.assignment_save_draft",
						new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_INFO));
			} else 	if (key.startsWith(EntityBeanLocator.NEW_PREFIX)) {
				messages.addMessage(new TargettedMessage("assignment2.assignment_post",
						new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_INFO));
			}
			else {
				messages.addMessage(new TargettedMessage("assignment2.assignment_save",
						new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_INFO));
			}
		} else {
			if (draft) {
				//messages.addMessage(new TargettedMessage("assignment2.assignment_save_draft_error"));
			} else {
				//messages.addMessage(new TargettedMessage("assignment2.assignment_post_error"));
			}
			return FAILURE;
		}
		attachmentBean.attachmentRefs = new String[100];
		return POST;
	}
	
	
	public String processActionPreview() {
		for (String key : OTPMap.keySet()) {
			Assignment2 assignment = OTPMap.get(key);
			if (this.requireAcceptUntil == null || Boolean.FALSE.equals(requireAcceptUntil)) {
				assignment.setAcceptUntilTime(null);
			}
			if (this.requireDueDate == null || this.requireDueDate == Boolean.FALSE) {
				assignment.setDueDate(null);
			}
			previewAssignmentBean.setAssignment(assignment);
			previewAssignmentBean.setOTPKey(key);
		}
		return PREVIEW;
	}
	
	public String processActionEdit() {
		return EDIT;
	}
	
	public String processActionSaveDraft() {
		String result = SAVE_DRAFT;
		for (String key : OTPMap.keySet()) {
			Assignment2 assignment = OTPMap.get(key);
			result = internalProcessPost(assignment, key, Boolean.TRUE);
		}
		return result;
	}
	
	public String processActionCancel() {
		return CANCEL;
	}
	
	public String processActionRemove() {
		List<Assignment2> entries = logic.getViewableAssignments();
		int assignmentsRemoved = 0;
		for (Assignment2 assignment : entries) {
			if (selectedIds.get(assignment.getId().toString()) == Boolean.TRUE){
				try {
					logic.deleteAssignment(assignment);
					assignmentsRemoved++;
				} catch (AnnouncementPermissionException ape) {
					LOG.error(ape.getMessage(), ape);
					// TODO the assign was deleted, but announcement was not
					// b/c user did not have delete perm in annc tool
				} catch (StaleObjectModificationException some) {
					LOG.error(some.getMessage(), some);
					// TODO provide a message to user that someone else was editing
					// this object at the same time
				}
			}
		}
		messages.addMessage( new TargettedMessage("assignment2.assignments_remove",
				new Object[] { Integer.valueOf(assignmentsRemoved) },
		        TargettedMessage.SEVERITY_INFO));
		return REMOVE;
	}
	
	public void createDuplicate(Long assignmentId) {
		Assignment2Creator creator = new Assignment2Creator();
		creator.setExternalLogic(externalLogic);
		creator.setMessageLocator(messageLocator);
		Assignment2 duplicate = creator.createDuplicate(logic.getAssignmentByIdWithGroupsAndAttachments(assignmentId));
		try {
			logic.saveAssignment(duplicate);
			
		} catch (SecurityException e) {
			LOG.error(e.getMessage(), e);
			messages.addMessage(new TargettedMessage("assignment2.assignment_post.security_exception"));
			return;
		} catch (AnnouncementPermissionException ape) {
			if (LOG.isDebugEnabled()) LOG.debug("Announcement could not " +
			"be updated b/c user does not have perm in annc tool");
			//TODO display to user?
		}
		messages.addMessage(new TargettedMessage("assignment2.assignment_post.duplicate",
			new Object[] {duplicate.getTitle() }, TargettedMessage.SEVERITY_INFO));
	}
	

}
