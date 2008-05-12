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

package org.sakaiproject.assignment2.tool.params;

import org.sakaiproject.assignment2.tool.beans.locallogic.LocalPermissionLogic;
import org.sakaiproject.assignment2.tool.producers.AuthorizationFailedProducer;
import org.sakaiproject.assignment2.tool.producers.AssignmentListSortViewProducer;
import org.sakaiproject.assignment2.tool.producers.AssignmentDetailProducer;
import org.sakaiproject.assignment2.tool.producers.StudentAssignmentListProducer;
import org.sakaiproject.assignment2.tool.producers.StudentSubmitProducer;
import org.sakaiproject.assignment2.tool.producers.StudentSubmitSummaryProducer;
import org.sakaiproject.assignment2.tool.params.SimpleAssignmentViewParams;
import org.sakaiproject.assignment2.tool.params.VerifiableViewParams;
import org.sakaiproject.tool.api.ToolManager;

import uk.org.ponder.rsf.viewstate.AnyViewParameters;
import uk.org.ponder.rsf.viewstate.RawViewParameters;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsInterceptor;

public class Assignment2ViewParamsInterceptor implements ViewParamsInterceptor {

	private LocalPermissionLogic localPermissionLogic;
	public void setLocalPermissionLogic(LocalPermissionLogic localPermissionLogic){
		this.localPermissionLogic = localPermissionLogic;
	}

	private ToolManager toolMgr;
	public void setToolManager(ToolManager toolMgr)
	{
		this.toolMgr = toolMgr;
	}
	
	public AnyViewParameters adjustViewParameters(ViewParameters incoming) {
		if (AuthorizationFailedProducer.VIEWID.equals(incoming.viewID)) {
			//Always return incoming if we are going to the Authorization Failed Page
			return incoming;
		}

		if (true) {
			String context = toolMgr.getCurrentPlacement().getContext();
			RawViewParameters rawParams = new RawViewParameters(
					"/sakai-assignment2-tool/sdata/assnList?context=" + context);
			return rawParams;
		}

		if (AssignmentDetailProducer.VIEW_ID.equals(incoming.viewID)) {
			//This is a entitybroker "helper" that is always visible
			//TODO make sure that this is always visible
			return incoming;
		}
		
		//Verify View Params for completeness
		if (incoming instanceof VerifiableViewParams) {
			if(!((VerifiableViewParams)incoming).verify()){
				return new AssignmentListSortViewParams(AssignmentListSortViewProducer.VIEW_ID);
			}
		}
		
		//If current user has permission access to requested view
		if (localPermissionLogic.checkCurrentUserHasViewPermission(incoming.viewID)){
			
			//now do specific interceptions for student submission pages
			//Student always has same link, redirect here based on it being open or not
			if (StudentSubmitProducer.VIEW_ID.equals(incoming.viewID) 
					|| StudentSubmitSummaryProducer.VIEW_ID.equals(incoming.viewID)) {
				
				//Returning newly generated view params causes issues (ASNN-53)
				String trueDestinationViewID = localPermissionLogic.filterViewIdForStudentSubmission((SimpleAssignmentViewParams)incoming);
				if (incoming.viewID.equals(trueDestinationViewID)) {
					return incoming;
				} else {
					return new SimpleAssignmentViewParams(trueDestinationViewID, ((SimpleAssignmentViewParams)incoming).assignmentId);
				}
			}
			
			
			return incoming;
		} else if (localPermissionLogic.checkCurrentUserHasViewPermission(StudentAssignmentListProducer.VIEW_ID)) {
			return new AssignmentListSortViewParams(StudentAssignmentListProducer.VIEW_ID);
			
		}
		
		return new SimpleViewParameters(AuthorizationFailedProducer.VIEWID);
	}
	
}