package org.sakaiproject.assignment2.tool.beans.locallogic;

import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.tool.producers.*;
import org.sakaiproject.assignment2.tool.producers.fragments.*;

public class LocalPermissionLogic {
	
	private AssignmentPermissionLogic permissionLogic;
	public void setPermissionLogic(AssignmentPermissionLogic permissionLogic) {
		this.permissionLogic = permissionLogic;
	}
	
	private ExternalLogic externalLogic;
	public void setExternalLogic(ExternalLogic externalLogic) {
		this.externalLogic = externalLogic;
	}
	
	public Boolean checkCurrentUserHasViewPermission(String viewId) {
		String contextId = externalLogic.getCurrentContextId();
		
		if (AddAttachmentHelperProducer.VIEWID.equals(viewId)) {
			return Boolean.TRUE;
			
		} else if (AssignmentListSortViewProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isUserAbleToAccessInstructorView(contextId);
			
		} else if (AssignmentProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isCurrentUserAbleToEditAssignments(contextId);
			
		} else if (GradeProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isUserAbleToAccessInstructorView(contextId);
			
		} else if (StudentAssignmentListProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isCurrentUserAbleToSubmit(contextId);
			
		} else if (StudentSubmitProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isCurrentUserAbleToSubmit(contextId);
			
		} else if (ViewSubmissionsProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isUserAbleToAccessInstructorView(contextId);
			
		} else if (AjaxCallbackProducer.VIEW_ID.equals(viewId)) {
			return Boolean.TRUE;
			
		} else if (FragmentAssignment2SelectProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isUserAbleToAccessInstructorView(contextId);
			
		} else if (FragmentAssignmentPreviewProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isUserAbleToAccessInstructorView(contextId);
			
		} else if (FragmentAttachmentsProducer.VIEW_ID.equals(viewId)) {
			return Boolean.TRUE;
		} else if (FragmentGradebookDetailsProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isUserAbleToAccessInstructorView(contextId);
			
		} else if (FragmentSubmissionGradePreviewProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isUserAbleToAccessInstructorView(contextId);
			
		} else if (FragmentSubmissionPreviewProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isCurrentUserAbleToSubmit(contextId);
		}
		//else just say No
		return Boolean.FALSE;
	}
	
}