package org.sakaiproject.assignment2.tool.beans;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalAnnouncementLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmissionAttachment;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.tool.api.SessionManager;
import org.sakaiproject.tool.api.ToolSession;

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
	public Long assignmentId;
	public String userId;
	
    private TargettedMessageList messages;
    public void setMessages(TargettedMessageList messages) {
    	this.messages = messages;
    }
	
	private AssignmentLogic assignmentLogic;
	public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
		this.assignmentLogic = assignmentLogic;
	}
	
	private AssignmentSubmissionLogic submissionLogic;
	public void setSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
		this.submissionLogic = submissionLogic;
	}
	
	
	private EntityBeanLocator assignment2EntityBeanLocator;
	public void setAssignment2EntityBeanLocator(EntityBeanLocator entityBeanLocator) {
		this.assignment2EntityBeanLocator = entityBeanLocator;
	}
	
	private Map<String, AssignmentSubmission> OTPMap;
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
	
	
	/*
	 * STUDENT FUNCTIONS
	 */
	public String processActionSubmit(){
		for (String key : OTPMap.keySet()) {
			AssignmentSubmission assignmentSubmission = OTPMap.get(key);
			if (assignmentId == null){
				return FAILURE;
			}
			Assignment2 assignment = assignmentLogic.getAssignmentById(assignmentId);
			assignmentSubmission.setAssignment(assignment);
			assignmentSubmission.getCurrentSubmissionVersion().setDraft(Boolean.FALSE);
			assignmentSubmission.getCurrentSubmissionVersion().setSubmittedTime(new Date());
			assignmentSubmission.getCurrentSubmissionVersion().setCreatedTime(new Date());
			
			
			//Start attachment stuff
			Set<AssignmentSubmissionAttachment> set = new HashSet();
			if (assignmentSubmission.getCurrentSubmissionVersion().getSubmissionAttachSet() != null) {
				set.addAll(assignmentSubmission.getCurrentSubmissionVersion().getSubmissionAttachSet());
			}
			
	    	//get New attachments from session set
	    	ToolSession session = sessionManager.getCurrentToolSession();
	    	if (session.getAttribute("attachmentRefs") != null) {
	    		for (String ref : (Set<String>)session.getAttribute("attachmentRefs")) {
	    			AssignmentSubmissionAttachment asa = new AssignmentSubmissionAttachment();
	    			asa.setAttachmentReference(ref);
	    			set.add(asa);
	    		}
	    	}
	    	Set<AssignmentSubmissionAttachment> final_set = new HashSet();
	    	//Now check for attachments that have been removed
	    	if (session.getAttribute("removedAttachmentRefs") != null) {
		    	for (AssignmentSubmissionAttachment asa : set) {
		    		//If this item in the set does not have a reference id that is 
		    		// located in the removed attachment reference ids set
		    		if (!((Set<String>) session.getAttribute("removedAttachmentRefs")).contains(asa.getAttachmentReference())){
		    			final_set.add(asa);
		    		}
		    	}
	    	} else {
	    		final_set.addAll(set);
	    	}
	    	assignmentSubmission.getCurrentSubmissionVersion().setSubmissionAttachSet(final_set);
			//End Attachment stuff
			
			submissionLogic.saveStudentSubmission(assignmentSubmission);
		}

		return SUBMIT;
	}
	
	public String processActionPreview(){
		for (String key : OTPMap.keySet()) {
			AssignmentSubmission assignmentSubmission = OTPMap.get(key);
			previewAssignmentSubmissionBean.setAssignmentSubmission(assignmentSubmission);
			
		}
		return PREVIEW;
	}
	
	public String processActionSaveDraft() {
		for (String key : OTPMap.keySet()) {
			AssignmentSubmission assignmentSubmission = OTPMap.get(key);
			if (assignmentId == null){
				return FAILURE;
			}
			Assignment2 assignment = assignmentLogic.getAssignmentById(assignmentId);
			assignmentSubmission.setAssignment(assignment);
			assignmentSubmission.getCurrentSubmissionVersion().setDraft(Boolean.TRUE);
			assignmentSubmission.getCurrentSubmissionVersion().setSubmittedTime(new Date());

			//Start attachment stuff
			Set<AssignmentSubmissionAttachment> set = new HashSet();
			if (assignmentSubmission.getCurrentSubmissionVersion().getSubmissionAttachSet() != null) {
				set.addAll(assignmentSubmission.getCurrentSubmissionVersion().getSubmissionAttachSet());
			}
			
			//get New attachments from session set
	    	ToolSession session = sessionManager.getCurrentToolSession();
	    	if (session.getAttribute("attachmentRefs") != null) {
	    		for (String ref : (Set<String>)session.getAttribute("attachmentRefs")) {
	    			AssignmentSubmissionAttachment asa = new AssignmentSubmissionAttachment();
	    			asa.setAttachmentReference(ref);
	    			set.add(asa);
	    		}
	    	}
	    	Set<AssignmentSubmissionAttachment> final_set = new HashSet();
	    	//Now check for attachments that have been removed
	    	if (session.getAttribute("removedAttachmentRefs") != null) {
		    	for (AssignmentSubmissionAttachment asa : set) {
		    		//If this item in the set does not have a reference id that is 
		    		// located in the removed attachment reference ids set
		    		if (!((Set<String>) session.getAttribute("removedAttachmentRefs")).contains(asa.getAttachmentReference())){
		    			final_set.add(asa);
		    		}
		    	}
	    	} else {
	    		final_set.addAll(set);
	    	}
	    	assignmentSubmission.getCurrentSubmissionVersion().setSubmissionAttachSet(final_set);
			//End Attachment stuff
			
			submissionLogic.saveStudentSubmission(assignmentSubmission);
		}
		return SAVE_DRAFT;
	}
	
	/*
	 * INSTRUCTOR FUNCTIONS
	 */
	public String processActionGradeSubmit(){
		for (String key : OTPMap.keySet()){
			AssignmentSubmission assignmentSubmission = OTPMap.get(key);
			if (assignmentId == null || userId == null){
				return FAILURE;
			}
			Assignment2 assignment = assignmentLogic.getAssignmentById(assignmentId);
			assignmentSubmission.setAssignment(assignment);
			assignmentSubmission.setUserId(userId);
			
			
			submissionLogic.saveInstructorFeedback(assignmentSubmission);
		}
		return SUBMIT;
	}
	
	public String processActionGradePreview(){
		for (String key : OTPMap.keySet()){
			AssignmentSubmission assignmentSubmission = OTPMap.get(key);
			Assignment2 assignment = assignmentLogic.getAssignmentByIdWithAssociatedData(assignmentId);
			assignmentSubmission.setAssignment(assignment);
			previewAssignmentSubmissionBean.setAssignmentSubmission(assignmentSubmission);
		}
		return PREVIEW;
	}
	
	public String processActionCancel() {
		return CANCEL;
	}
	
	public void populateNonPersistedFieldsForSubmissions(List<AssignmentSubmission> submissionList) {
		if (submissionList == null || submissionList.isEmpty())
			return;
		
		// Now, iterate through the viewable assignments and set the not persisted fields 
		// that aren't related to the gradebook
		
		for (Iterator subIter = submissionList.iterator(); subIter.hasNext();) {
			AssignmentSubmission submission = (AssignmentSubmission) subIter.next();
			if (submission != null) {

				// set the status for this submission: "In Progress, Submitted, etc"
				int status = submissionLogic.getSubmissionStatus(submission);
				submission.setSubmissionStatus(messageLocator.getMessage("assignment2.assignment_grade-assignment.submission_status." + status));
			}
		}
	}
	
	public List filterListForPaging(List myList, int begIndex, int numItemsToDisplay) {
        if (myList == null || myList.isEmpty())
        	return myList;
        
        int endIndex = begIndex + numItemsToDisplay;
        if (endIndex > myList.size()) {
        	endIndex = myList.size();
        }

		return myList.subList(begIndex, endIndex);
	}
	
	/**
	 * Will apply paging and sorting to the given list and populate any non-persisted
	 * fields that need to be populated from the UI (ie fields that require access
	 * to the bundle)
	 * @param submissionList
	 * @param currentStart
	 * @param currentCount
	 * @param sortBy
	 * @param sortDir
	 */
	public void filterPopulateAndSortSubmissionList(List<AssignmentSubmission> submissionList, int currentStart, int currentCount, String sortBy, boolean sortDir) {
		submissionList = filterListForPaging(submissionList, currentStart, currentCount);
        populateNonPersistedFieldsForSubmissions(submissionList);
        submissionLogic.sortSubmissions(submissionList, sortBy, sortDir);
	}
}