package org.sakaiproject.assignment2.tool.beans;

import java.util.HashMap;
import java.util.Map;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalAnnouncementLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.tool.api.SessionManager;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.messageutil.TargettedMessageList;

public class AssignmentSubmissionBean {
	
	private static final String SUBMIT = "submit";
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
	
	
	private EntityBeanLocator assignment2EntityBeanLocator;
	public void setAssignment2EntityBeanLocator(EntityBeanLocator entityBeanLocator) {
		this.assignment2EntityBeanLocator = entityBeanLocator;
	}
	
	private Map<String, Assignment2> OTPMap;
	@SuppressWarnings("unchecked")
	public void setAssignmentSubmissionEntityBeanLocator(EntityBeanLocator entityBeanLocator) {
		this.OTPMap = entityBeanLocator.getDeliveredBeans();
	}
		
	private ExternalLogic externalLogic;
	public void setExternalLogic(ExternalLogic externalLogic) {
		this.externalLogic = externalLogic;
	}
	
	private ExternalAnnouncementLogic announcementLogic;
	public void setExternalAnnouncementLogic(ExternalAnnouncementLogic announcementLogic) {
		this.announcementLogic = announcementLogic;
	}
	
	private PreviewAssignmentSubmissionBean previewAssignmentSubmissionBean;
	public void setPreviewAssignmentSubmissionBean (PreviewAssignmentSubmissionBean previewAssignmentSubmissionBean) {
		this.previewAssignmentSubmissionBean = previewAssignmentSubmissionBean;
	}
	
	private MessageLocator messageLocator;
	public void setMessageLocator (MessageLocator messageLocator) {
		this.messageLocator = messageLocator;
	}
	
	private SessionManager sessionManager;
	public void setSessionManager(SessionManager sessionManager) {
		this.sessionManager = sessionManager;
	}
	
	
	public String processActionSubmit(){
		return SUBMIT;
	}
	
	public String processActionPreview(){
		return PREVIEW;
	}
	
	public String processActionSaveDraft() {
		return SAVE_DRAFT;
	}
	
	public String processActionCancel() {
		return CANCEL;
	}
}