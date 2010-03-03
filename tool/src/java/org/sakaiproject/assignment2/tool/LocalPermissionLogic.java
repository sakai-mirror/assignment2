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

package org.sakaiproject.assignment2.tool;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
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

/**
 * Given a view params, this class allows you determine whether a User has
 * permission to actually view that URL.
 * 
 * Also included is the logic that determines whether to go to the 
 * StudentSubmitSummary or just StudentSubmit page.  I'm thinking about 
 * combining those.
 * 
 * @author rjlowe
 * @author sgithens
 *
 */
public class LocalPermissionLogic {

    private AssignmentPermissionLogic permissionLogic;
    public void setPermissionLogic(AssignmentPermissionLogic permissionLogic) {
        this.permissionLogic = permissionLogic;
    }

    private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }
    
    private AssignmentLogic assignmentLogic;
    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
        this.assignmentLogic = assignmentLogic;
    }

    /**
     * Determines whether or not a User can actually view the page defined
     * by these view parameters.
     * 
     * @param viewParams
     * @return Whether not the user can view this page.  False if not, True if
     * they are.
     */
    public Boolean checkCurrentUserHasViewPermission(ViewParameters viewParams) {
        String contextId = externalLogic.getCurrentContextId();
        String viewId = viewParams.viewID;

        /* This first check is special and for our new hybrid pages that need to
         * go to the content directory for various Fluid interfaces.
         */
        // ASNN-466 if (viewId.equals("content")) {
        //    return Boolean.TRUE;
        // }
        if (AddAttachmentHelperProducer.VIEWID.equals(viewId)) {
            return Boolean.TRUE;
        }
        else if (PreviewAsStudentProducer.VIEW_ID.equals(viewId)) {
            // the instructor may preview new assignments if they have the add
            // perm but they can only preview existing assignments if they may
            // edit that assignment
            if (viewParams instanceof SimpleAssignmentViewParams) {
                SimpleAssignmentViewParams params = (SimpleAssignmentViewParams) viewParams;
                return isUserAllowedToAddOrEditAssignment(params.assignmentId, contextId);
            }
            
            return Boolean.FALSE;
        }
        else if (ListProducer.VIEW_ID.equals(viewId)) {
            return permissionLogic.isUserAbleToAccessInstructorView(contextId);
        }
        else if (AssignmentInfoDataProducer.VIEW_ID.equals(viewId)) {
            // Currently we are only allowing instructors to view the assignment
            // info since it contains the number of submissions. Will plan on 
            // doing better checking of what information users can see in the 
            // future from the JSON feed, so folks can make mashups.
            return permissionLogic.isUserAbleToAccessInstructorView(contextId);
        }
        else if (AssignmentProducer.VIEW_ID.equals(viewId)) {
            // permission to view this screen depends upon whether this is an add
            // or edit scenario
            if (viewParams instanceof AssignmentViewParams) {
                AssignmentViewParams params = (AssignmentViewParams) viewParams;
                return isUserAllowedToAddOrEditAssignment(params.assignmentId, contextId);
            }
            
            return Boolean.FALSE;

        } else if (FinishedHelperProducer.VIEWID.equals(viewId)) {
            return Boolean.TRUE;

        } else if (GradeProducer.VIEW_ID.equals(viewId)) {
            if (viewParams instanceof GradeViewParams)
            {
                GradeViewParams params = (GradeViewParams) viewParams;
                return permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(params.userId, params.assignmentId);
            } 

            return Boolean.FALSE;

        } 
        else if (StudentAssignmentListProducer.VIEW_ID.equals(viewId)) {
            return permissionLogic.isCurrentUserAbleToSubmit(contextId);

        } else if (StudentSubmitProducer.VIEW_ID.equals(viewId)) {
            if (viewParams instanceof SimpleAssignmentViewParams) {
                SimpleAssignmentViewParams params = (SimpleAssignmentViewParams) viewParams;

                return permissionLogic.isCurrentUserAbleToSubmit(contextId) && 
                permissionLogic.isUserAbleToViewAssignment(params.assignmentId);
            }

            return Boolean.FALSE;

        } else if (ViewSubmissionsProducer.VIEW_ID.equals(viewId)) {
            if (viewParams instanceof ViewSubmissionsViewParams) {
                ViewSubmissionsViewParams params = (ViewSubmissionsViewParams) viewParams;

                return permissionLogic.isUserAbleToAccessInstructorView(contextId) && 
                permissionLogic.isUserAbleToViewAssignment(params.assignmentId);
            }

            return Boolean.FALSE;

        } else if (FragmentAssignment2SelectProducer.VIEW_ID.equals(viewId)) {
            // TODO: it isn't clear what permission you should have for this one,
            // so defaulting to add perm
            return permissionLogic.isUserAllowedToAddAssignments(contextId);
        }
        else if (FragmentGradebookDetailsProducer.VIEW_ID.equals(viewId)) {
            if (viewParams instanceof FragmentGradebookDetailsViewParams) {
                FragmentGradebookDetailsViewParams params = (FragmentGradebookDetailsViewParams) viewParams;
                return permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(params.userId, params.assignmentId);
            }

            return Boolean.FALSE;
        } else if (FragmentSubmissionGradePreviewProducer.VIEW_ID.equals(viewId)) {
            //TODO - RYAN!  Remove this producer!
            return Boolean.FALSE;
        } else if (UploadAllProducer.VIEW_ID.equals(viewId) || UploadAllConfirmProducer.VIEW_ID.equals(viewId)) {
            if (viewParams instanceof AssignmentViewParams) {
                AssignmentViewParams params = (AssignmentViewParams) viewParams;

                return permissionLogic.isUserAbleToAccessInstructorView(contextId) && 
                permissionLogic.isUserAbleToViewAssignment(params.assignmentId);
            }

            return Boolean.FALSE;

        } else if ("zipSubmissions".equals(viewId)) {
            if (viewParams instanceof ZipViewParams) {
                ZipViewParams params = (ZipViewParams) viewParams;

                return permissionLogic.isUserAbleToAccessInstructorView(contextId) &&
                permissionLogic.isUserAbleToViewAssignment(params.assignmentId);
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
    
    /**
     * 
     * @param assignId if null, assumes add scenario
     * @param contextId
     * @return depending upon whether or not the assignId is null, will return true
     * if the current user has permission to add a new assignment (if assignId is null) or edit
     * an existing assignment (if assignId is not null)
     */
    private boolean isUserAllowedToAddOrEditAssignment(Long assignId, String contextId) {
        if (assignId == null) {
            // add assignment scenario
            return permissionLogic.isUserAllowedToAddAssignments(contextId);
        } else {
            // we are editing
            if (permissionLogic.isUserAllowedToEditAllAssignments(contextId)) {
                return true;
            } else {
                Assignment2 assign = assignmentLogic.getAssignmentById(assignId);
                return permissionLogic.isUserAllowedToEditAssignment(assign);
            }
        }
    }

}
