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

import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.tool.producers.ListProducer;
import org.sakaiproject.assignment2.tool.producers.StudentAssignmentListProducer;

import uk.org.ponder.rsf.viewstate.ViewParameters;

public class Assignment2DefaultViewParameters {
    //getters setters
    private AssignmentPermissionLogic permissionLogic;
    public void setPermissionLogic(AssignmentPermissionLogic permissionLogic) {
        this.permissionLogic = permissionLogic;
    }

    private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }

    public ViewParameters getViewParameters() {
        AssignmentListSortViewParams viewParams = new AssignmentListSortViewParams(StudentAssignmentListProducer.VIEW_ID);
        String contextId = externalLogic.getCurrentContextId();

        if (permissionLogic.isUserAllowedToAccessInstructorView(contextId)) {
            viewParams = new AssignmentListSortViewParams(ListProducer.VIEW_ID);
        }

        return viewParams;
    }
}