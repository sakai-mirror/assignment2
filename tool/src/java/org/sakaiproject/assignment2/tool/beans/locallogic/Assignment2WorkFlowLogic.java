/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/tool/src/java/org/sakaiproject/assignment2/tool/params/Assignment2ViewParamsInterceptor.java $
 * $Id: Assignment2ViewParamsInterceptor.java 53899 2008-10-13 15:07:58Z swgithen@mtu.edu $
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

import org.sakaiproject.assignment2.tool.beans.AssignmentSubmissionBean;
import org.sakaiproject.assignment2.tool.params.AssignmentListSortViewParams;
import org.sakaiproject.assignment2.tool.params.SimpleAssignmentViewParams;
import org.sakaiproject.assignment2.tool.params.StudentSubmissionParams;
import org.sakaiproject.assignment2.tool.params.VerifiableViewParams;
import org.sakaiproject.assignment2.tool.params.ViewSubmissionsViewParams;
import org.sakaiproject.assignment2.tool.producers.AssignmentDetailProducer;
import org.sakaiproject.assignment2.tool.producers.ListProducer;
import org.sakaiproject.assignment2.tool.producers.AuthorizationFailedProducer;
import org.sakaiproject.assignment2.tool.producers.RedirectToAssignmentProducer;
import org.sakaiproject.assignment2.tool.producers.StudentAssignmentListProducer;
import org.sakaiproject.assignment2.tool.producers.StudentSubmitProducer;
import org.sakaiproject.assignment2.tool.producers.ViewSubmissionsProducer;

import uk.org.ponder.rsf.flow.ARIResult;
import uk.org.ponder.rsf.flow.ActionResultInterceptor;
import uk.org.ponder.rsf.viewstate.AnyViewParameters;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsInterceptor;

/**
 * The primary HTTP GET interceptor for Assignments2, that checks URL's for 
 * permissions and other logic, and then changes where they should be going or
 * their parameters if necessary.
 * 
 * @author rjlowe
 * @author sgithens
 *
 */
public class Assignment2WorkFlowLogic implements ViewParamsInterceptor, ActionResultInterceptor {

    private LocalPermissionLogic localPermissionLogic;
    public void setLocalPermissionLogic(LocalPermissionLogic localPermissionLogic){
        this.localPermissionLogic = localPermissionLogic;
    }


    public AnyViewParameters adjustViewParameters(ViewParameters incoming) {
        if (AuthorizationFailedProducer.VIEWID.equals(incoming.viewID)) {
            //Always return incoming if we are going to the Authorization Failed Page
            return incoming;
        }

        if (AssignmentDetailProducer.VIEW_ID.equals(incoming.viewID)) {
            //This is a entitybroker "helper" that is always visible
            //TODO make sure that this is always visible
            return incoming;
        }

        //Verify View Params for completeness
        if (incoming instanceof VerifiableViewParams) {
            if(!((VerifiableViewParams)incoming).verify()){
                return new AssignmentListSortViewParams(ListProducer.VIEW_ID);
            }
        }

        // depending on the user's perms in the site, will redirect them to either
        // the "View Submissions" page or Student Summary/Submit page from this generic link
        if (RedirectToAssignmentProducer.VIEWID.equals(incoming.viewID)) {
            if (incoming instanceof SimpleAssignmentViewParams) {
                SimpleAssignmentViewParams params = (SimpleAssignmentViewParams) incoming;
                if (localPermissionLogic.checkCurrentUserHasViewPermission(new ViewSubmissionsViewParams(ViewSubmissionsProducer.VIEW_ID, params.assignmentId))) {
                    return new ViewSubmissionsViewParams(ViewSubmissionsProducer.VIEW_ID, params.assignmentId);
                } else if (localPermissionLogic.checkCurrentUserHasViewPermission(new SimpleAssignmentViewParams(StudentSubmitProducer.VIEW_ID, params.assignmentId))) {
                    return new SimpleAssignmentViewParams(StudentSubmitProducer.VIEW_ID, params.assignmentId);
                }
            }
        }

        //If current user has permission access to requested view
        if (localPermissionLogic.checkCurrentUserHasViewPermission(incoming)){

            //now do specific interceptions for student submission pages
            //Student always has same link, redirect here based on it being open or not
            // TODO FIXME This is being merged into one view. Come back
            // and remove this after it works.
            //if (StudentSubmitProducer.VIEW_ID.equals(incoming.viewID) 
            //        || StudentSubmitSummaryProducer.VIEW_ID.equals(incoming.viewID)) {

                /*
                 * This is responsible for going to different views depending on
                 * if the Student can submit still.
                 */
            //    String trueDestinationViewID = localPermissionLogic.filterViewIdForStudentSubmission((SimpleAssignmentViewParams)incoming);
             //   if (incoming.viewID.equals(trueDestinationViewID)) {
             //       return incoming;
             //   } else {
             //       return new SimpleAssignmentViewParams(trueDestinationViewID, ((SimpleAssignmentViewParams)incoming).assignmentId);
             //   }
            //    */
           // }

            return incoming;
        } else if (localPermissionLogic.checkCurrentUserHasViewPermission(new AssignmentListSortViewParams(StudentAssignmentListProducer.VIEW_ID))) {
            return new AssignmentListSortViewParams(StudentAssignmentListProducer.VIEW_ID);
        }

        return new SimpleViewParameters(AuthorizationFailedProducer.VIEWID);
    }


    public void interceptActionResult(ARIResult result, ViewParameters incoming, Object actionReturn) {
        if (incoming instanceof StudentSubmissionParams) {
            StudentSubmissionParams params = (StudentSubmissionParams) incoming;
            
            if (AssignmentSubmissionBean.SUBMIT.equals(actionReturn)) {
                result.resultingView = new SimpleViewParameters(StudentAssignmentListProducer.VIEW_ID);
            }
            else if (AssignmentSubmissionBean.BACK_TO_EDIT.equals(actionReturn)) {
                result.resultingView = new StudentSubmissionParams(StudentSubmitProducer.VIEW_ID, params.assignmentId, false);
                result.propagateBeans = ARIResult.FLOW_ONESTEP;
            }
            else if (AssignmentSubmissionBean.PREVIEW.equals(actionReturn)) {
                System.out.println("PREVIEW action");
                result.resultingView = new StudentSubmissionParams(StudentSubmitProducer.VIEW_ID, params.assignmentId, true);
                result.propagateBeans = ARIResult.FLOW_ONESTEP;
            } else if (AssignmentSubmissionBean.SAVE_DRAFT.equals(actionReturn)) {
                result.resultingView = new SimpleViewParameters(StudentAssignmentListProducer.VIEW_ID);
                System.out.println("SAVE_DRAFT action");
            } else if (AssignmentSubmissionBean.CANCEL.equals(actionReturn)) {
                System.out.println("CANCEL action");
            } else {
                System.out.println("OTHER ACTION!!! " + actionReturn);
            }
        }
    }

}