/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/tool/src/java/org/sakaiproject/assignment2/tool/params/AjaxCallbackViewParams.java $
 * $Id: AjaxCallbackViewParams.java 53787 2008-10-08 19:41:11Z swgithen@mtu.edu $
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

package org.sakaiproject.assignment2.tool.commands;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;

/**
 * This is an action bean that reorders the
 * {@link Assignment2} objects in a site given
 * a list of assignmentIds
 * 
 * @author wagnermr
 *
 */
public class ReorderAssignmentsCommand {

    private static Log log = LogFactory.getLog(ReorderAssignmentsCommand.class);
    
    // Dependency
    private AssignmentLogic assignmentLogic;
    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
        this.assignmentLogic = assignmentLogic;
    }
    
    private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }
    
    // Property
    private String orderedAssignIds;
    /**
     * a comma-delimited list of the assignmentIds (numbers) in the order
     * you wish them to be saved
     * @param orderedAssignIds
     */
    public void setOrderedAssignIds(String orderedAssignIds) {
        this.orderedAssignIds = orderedAssignIds;
    }
    
    /**
     * The action method.
     */
    public void execute() {
        if (orderedAssignIds != null) {
            String[] stringAssignIds = orderedAssignIds.split(",");
            try {
                // get the contextId
                String contextId = externalLogic.getCurrentContextId();
                // convert the strings to longs
                List<Long> longAssignmentIds = new ArrayList<Long>();
                for (int i=0; i < stringAssignIds.length; i++){
                    String idAsString = stringAssignIds[i];
                    if (idAsString != null && idAsString.trim().length() > 0) { 
                        longAssignmentIds.add(Long.valueOf(stringAssignIds[i]));
                    }
                }
                assignmentLogic.reorderAssignments(longAssignmentIds, contextId);

                if (log.isDebugEnabled()) log.debug("Assignments reordered via ReorderAssignmentsCommand");
            } catch (NumberFormatException nfe) {
                log.error("Non-numeric value passed to ReorderAssignmentsCommand. No reordering was saved.");
            }
        }
    }
}
