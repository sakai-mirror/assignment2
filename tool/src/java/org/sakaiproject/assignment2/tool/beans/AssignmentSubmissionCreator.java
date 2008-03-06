package org.sakaiproject.assignment2.tool.beans;

import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;

import uk.org.ponder.messageutil.MessageLocator;

public class AssignmentSubmissionCreator {
	
	private ExternalLogic externalLogic;
	public void setExternalLogic(ExternalLogic externalLogic) {
		this.externalLogic = externalLogic;
	}

	private MessageLocator messageLocator;
	public void setMessageLocator(MessageLocator messageLocator) {
		this.messageLocator = messageLocator;
	}
	
	public AssignmentSubmission create(){
		AssignmentSubmission togo = new AssignmentSubmission();
		
		//create the AssignmentSubmissionVersion object
		AssignmentSubmissionVersion currentSubmissionVersion = new AssignmentSubmissionVersion();
		currentSubmissionVersion.setCreatedBy(externalLogic.getCurrentUserId());
		
		togo.setCurrentSubmissionVersion(currentSubmissionVersion);
		togo.setUserId(externalLogic.getCurrentUserId());
		
		return togo;
	}
}