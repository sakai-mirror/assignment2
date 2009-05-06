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

    private AssignmentSubmissionLogic submissionLogic;
    public void setSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
        this.submissionLogic = submissionLogic;
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
        } else if (RemoveAssignmentConfirmProducer.VIEW_ID.equals(viewId)) {
            return permissionLogic.isCurrentUserAbleToEditAssignments(contextId);
        }
        else if (PreviewAsStudentProducer.VIEW_ID.equals(viewId)) {
            return permissionLogic.isCurrentUserAbleToEditAssignments(contextId);
        }
        else if (AjaxResultsProducer.VIEW_ID.equals(viewId)) {
            return Boolean.TRUE;

        } else if (AssignmentDetailProducer.VIEW_ID.equals(viewId)) {
            // used by entity broker
            return Boolean.TRUE;

        } else if (ListProducer.VIEW_ID.equals(viewId)) {
            return permissionLogic.isUserAbleToAccessInstructorView(contextId);
        }
        else if (AssignmentInfoDataProducer.VIEW_ID.equals(viewId)) {
            // Currently we are only allowing instructors to view the assignment
            // info since it contains the number of submissions. Will plan on 
            // doing better checking of what information users can see in the 
            // future from the JSON feed, so folks can make mashups.
            return permissionLogic.isCurrentUserAbleToEditAssignments(contextId);
        } else if (ListReorderProducer.VIEW_ID.equals(viewId)) {
            return permissionLogic.isCurrentUserAbleToEditAssignments(contextId);

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
            return permissionLogic.isCurrentUserAbleToEditAssignments(contextId);
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
        } else if (FragmentViewSubmissionProducer.VIEW_ID.equals(viewId)) {
            return Boolean.TRUE;
        } else if (UploadAllProducer.VIEW_ID.equals(viewId) || UploadAllConfirmProducer.VIEW_ID.equals(viewId)) {
            if (viewParams instanceof AssignmentViewParams) {
                AssignmentViewParams params = (AssignmentViewParams) viewParams;

                return permissionLogic.isUserAbleToAccessInstructorView(contextId) && 
                permissionLogic.isUserAbleToViewAssignment(params.assignmentId);
            }

            return Boolean.FALSE;

        } else if (FragmentAssignmentInstructionsProducer.VIEW_ID.equals(viewId)) {
            if (viewParams instanceof AssignmentViewParams) {
                AssignmentViewParams params = (AssignmentViewParams) viewParams;

                return permissionLogic.isUserAbleToViewAssignment(params.assignmentId);
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

}
