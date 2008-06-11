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

package org.sakaiproject.assignment2.tool.beans.locallogic;

import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.tool.producers.*;
import org.sakaiproject.assignment2.tool.producers.fragments.*;
import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.assignment2.tool.params.FragmentGradebookDetailsViewParams;
import org.sakaiproject.assignment2.tool.params.GradeViewParams;
import org.sakaiproject.assignment2.tool.params.SimpleAssignmentViewParams;
import org.sakaiproject.assignment2.tool.params.ViewSubmissionsViewParams;
import org.sakaiproject.assignment2.tool.params.ZipViewParams;

import uk.org.ponder.rsf.builtin.UVBProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;

public class LocalPermissionLogic {
	
	private AssignmentPermissionLogic permissionLogic;
	public void setPermissionLogic(AssignmentPermissionLogic permissionLogic) {
		this.permissionLogic = permissionLogic;
	}
	
	private ExternalLogic externalLogic;
	public void setExternalLogic(ExternalLogic externalLogic) {
		this.externalLogic = externalLogic;
	}
	
	private AssignmentSubmissionLogic submissionLogic;
	public void setSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
		this.submissionLogic = submissionLogic;
	}
	
	public Boolean checkCurrentUserHasViewPermission(ViewParameters viewParams) {
		String contextId = externalLogic.getCurrentContextId();
		String viewId = viewParams.viewID;
		
		if (AddAttachmentHelperProducer.VIEWID.equals(viewId)) {
			return Boolean.TRUE;
			
		} else if (AjaxResultsProducer.VIEW_ID.equals(viewId)) {
			return Boolean.TRUE;
			
		}else if (AssignmentDetailProducer.VIEW_ID.equals(viewId)) {
			// used by entity broker
			return Boolean.TRUE;
			
		} else if (ListProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isUserAbleToAccessInstructorView(contextId);
			
      } else if (ListReorderProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isUserAbleToAccessInstructorView(contextId);

		} else if (AssignmentProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isCurrentUserAbleToEditAssignments(contextId);
			
		} else if (FinishedHelperProducer.VIEWID.equals(viewId)) {
			return Boolean.TRUE;
			
		} else if (GradeProducer.VIEW_ID.equals(viewId)) {
			if (viewParams instanceof GradeViewParams)
			{
				GradeViewParams params = (GradeViewParams) viewParams;
				return permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(params.userId, params.assignmentId);
			} 
			
			return Boolean.FALSE;

      } else if (SettingsProducer.VIEW_ID.equals(viewId)) {
         return Boolean.TRUE;         

		} else if (StudentAssignmentListProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isCurrentUserAbleToSubmit(contextId);
			
		} else if (StudentSubmitProducer.VIEW_ID.equals(viewId)) {
			if (viewParams instanceof SimpleAssignmentViewParams) {
				SimpleAssignmentViewParams params = (SimpleAssignmentViewParams) viewParams;
				
				return permissionLogic.isCurrentUserAbleToSubmit(contextId) && 
					permissionLogic.isUserAbleToViewAssignment(contextId, params.assignmentId);
			}
			
			return Boolean.FALSE;
			
		} else if (ViewSubmissionsProducer.VIEW_ID.equals(viewId)) {
			if (viewParams instanceof ViewSubmissionsViewParams) {
				ViewSubmissionsViewParams params = (ViewSubmissionsViewParams) viewParams;
				
				return permissionLogic.isUserAbleToAccessInstructorView(contextId) && 
					permissionLogic.isUserAbleToViewAssignment(contextId, params.assignmentId);
			}

			return Boolean.FALSE;
			
		} else if (AjaxCallbackProducer.VIEW_ID.equals(viewId)) {
			return Boolean.TRUE;
			
		} else if (FragmentAssignment2SelectProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isCurrentUserAbleToEditAssignments(contextId);
			
		} else if (FragmentAssignmentPreviewProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isCurrentUserAbleToEditAssignments(contextId);
			
		} else if (FragmentGradebookDetailsProducer.VIEW_ID.equals(viewId)) {
			if (viewParams instanceof FragmentGradebookDetailsViewParams) {
				FragmentGradebookDetailsViewParams params = (FragmentGradebookDetailsViewParams) viewParams;
				return permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(params.userId, params.assignmentId);
			}
			
			return Boolean.FALSE;
			
		} else if (FragmentSubmissionGradePreviewProducer.VIEW_ID.equals(viewId)) {
			//TODO - RYAN!  Remove this producer!
			return Boolean.FALSE;
			
		} else if (FragmentSubmissionPreviewProducer.VIEW_ID.equals(viewId)) {
			return permissionLogic.isCurrentUserAbleToSubmit(contextId);
		
		} else if (FragmentViewSubmissionProducer.VIEW_ID.equals(viewId)) {
			return Boolean.TRUE;
		
		} else if (UploadAllProducer.VIEW_ID.equals(viewId)) {
			if (viewParams instanceof AssignmentViewParams) {
				AssignmentViewParams params = (AssignmentViewParams) viewParams;
				
				return permissionLogic.isUserAbleToAccessInstructorView(contextId) && 
					permissionLogic.isUserAbleToViewAssignment(contextId, params.assignmentId);
			}
			
			return Boolean.FALSE;
	
		} else if (FragmentAssignmentInstructionsProducer.VIEW_ID.equals(viewId)) {
			if (viewParams instanceof AssignmentViewParams) {
				AssignmentViewParams params = (AssignmentViewParams) viewParams;
				
				return permissionLogic.isUserAbleToViewAssignment(contextId, params.assignmentId);
			}
			
			return Boolean.FALSE;
			
		} else if ("zipSubmissions".equals(viewId)) {
			if (viewParams instanceof ZipViewParams) {
				ZipViewParams params = (ZipViewParams) viewParams;
				
				return permissionLogic.isUserAbleToAccessInstructorView(contextId) &&
					permissionLogic.isUserAbleToViewAssignment(contextId, params.assignmentId);
			}
			
			return Boolean.FALSE;
			
		} else if (TaggableHelperProducer.VIEWID.equals(viewId)) {
			return permissionLogic.isUserAbleToAccessInstructorView(contextId);
		}
		
		//Here are some RSF Generic always true viewIds
		
		else if (UVBProducer.VIEW_ID.equals(viewId)) {
			return Boolean.TRUE;
		}
		
		//else just say No
		return Boolean.FALSE;
	}
	
	public String filterViewIdForStudentSubmission(SimpleAssignmentViewParams incoming) {
		String userId = externalLogic.getCurrentUserId();
		if(submissionLogic.submissionIsOpenForStudentForAssignment(userId, incoming.assignmentId)){
			return StudentSubmitProducer.VIEW_ID;
		} else {
			return StudentSubmitSummaryProducer.VIEW_ID;
		}
	}
	
}
