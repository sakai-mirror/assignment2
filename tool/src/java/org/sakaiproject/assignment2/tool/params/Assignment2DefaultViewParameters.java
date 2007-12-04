package org.sakaiproject.assignment2.tool.params;

import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.tool.producers.AssignmentListSortViewProducer;
import org.sakaiproject.assignment2.tool.producers.StudentAssignmentListProducer;
import org.sakaiproject.assignment2.tool.params.AssignmentListSortViewParams;

import org.sakaiproject.user.api.User;
import org.sakaiproject.authz.api.SecurityService;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class Assignment2DefaultViewParameters {
	//getters setters
	private User user;
	public void setUser(User user) {
		this.user = user;
	}
	
	private SecurityService securityService;
	public void setSecurityService(SecurityService securityService) {
		this.securityService = securityService;
	}
	
	private ExternalLogic externalLogic;
	public void setExternalLogic(ExternalLogic externalLogic) {
		this.externalLogic = externalLogic;
	}

	public ViewParameters getViewParameters() {
		if (externalLogic.getCurrentUserHasPermission("assignment2.submit") && 
				!externalLogic.isUserAdmin(externalLogic.getCurrentUserId())){
			return new AssignmentListSortViewParams(StudentAssignmentListProducer.VIEW_ID);
		}
		else{
			return new AssignmentListSortViewParams(AssignmentListSortViewProducer.VIEW_ID);
		}
	}
}