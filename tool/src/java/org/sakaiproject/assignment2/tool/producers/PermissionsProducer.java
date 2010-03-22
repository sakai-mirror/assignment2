/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/branches/ASNN-617/tool/src/java/org/sakaiproject/assignment2/tool/producers/AssignmentProducer.java $
 * $Id: AssignmentProducer.java 66488 2010-03-10 19:44:33Z wagnermr@iupui.edu $
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

package org.sakaiproject.assignment2.tool.producers;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.authz.api.Role;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIBoundBoolean;
import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIForm;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

/**
 * Paints the Assignment2 Page used to define site-level permissions for
 * Assignment2. We could use the sakai permissions helper except that we
 * need to link to the gradebook to allow the user to edit TA grader permissions.
 *
 */
public class PermissionsProducer implements ViewComponentProducer, ViewParamsReporter {
    private static Log log = LogFactory.getLog(PermissionsProducer.class);

    public static final String VIEW_ID = "permissions";
    public String getViewID() {
        return VIEW_ID;
    }
    
    // Dependencies
    private ExternalLogic externalLogic;
    private AssignmentPermissionLogic permissionLogic;
    private MessageLocator messageLocator;

    @SuppressWarnings("unchecked")
    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
        
        String currContextId = externalLogic.getCurrentContextId();
        
        // This information is displayed in the instructions
        String toolTitle = externalLogic.getToolTitle();
        String siteTitle = externalLogic.getSiteTitle(currContextId);
        String siteId = currContextId;
        UIMessage.make(tofill, "instructions", "assignment2.permissions.instructions", new Object[] {toolTitle, siteTitle, siteId});
        
        /**
         * Begin the Form
         */
        UIForm form = UIForm.make(tofill, "form");
        
        
        
        // get the role/permission information
        Map<Role, Map<String, Boolean>> roleFunctionMap = permissionLogic.getRoleFunctionMap(currContextId, true);
        List<Role> orderedRoles = new ArrayList<Role>(roleFunctionMap.keySet());
        Collections.sort(orderedRoles);
        
        
        UIOutput.make(form, "roles:", messageLocator.getMessage("assignment2.permissions.perm.heading"));
        for (Role role : orderedRoles) {
            UIOutput.make(form, "roles:", role.getId());
        }

        List<String> orderedPermissions = permissionLogic.getAssignment2PermissionFunctions();
        for (String perm : orderedPermissions) {
            UIBranchContainer permRow = UIBranchContainer.make(form, "perms:");
            UIMessage.make(permRow, "perm_name", "desc-" + perm);
            // now iterate through the roles and retrieve whether or not that role
            // has the perm
            for (Role role : orderedRoles) {
                UIBranchContainer perm_checkboxes = UIBranchContainer.make(permRow, "has_perm:");
                Map<String, Boolean> functionMap = roleFunctionMap.get(role);
                boolean hasPerm = false;
                if (functionMap != null && functionMap.containsKey(perm)) {
                    hasPerm = functionMap.get(perm);
                }
                
                UIBoundBoolean.make(perm_checkboxes, "perm_checkbox", hasPerm);
            }
        }
        
        // now we may need to include a final row that contains the gradebook grading permission
        // information
        if (externalLogic.siteHasTool(currContextId, ExternalLogic.TOOL_ID_GRADEBOOK)) {
            UIOutput.make(form, "gradebook_perm");
            
            boolean displayTAFootnote = false;
            
            for (Role role : orderedRoles) {
                UIBranchContainer grade_perms = UIBranchContainer.make(form, "gb_has_perm:");
                Map<String, Boolean> functionMap = roleFunctionMap.get(role);

                // default to none
                String perm_level = messageLocator.getMessage("assignment2.permissions.grade.none");
                if (functionMap != null) {
                    if (functionMap.containsKey(AssignmentConstants.GB_PERMISSION_GRADE_ALL) &&
                            functionMap.get(AssignmentConstants.GB_PERMISSION_GRADE_ALL)) {
                        perm_level = messageLocator.getMessage("assignment2.permissions.grade.all");
                    } else if (functionMap.containsKey(AssignmentConstants.GB_PERMISSION_GRADE_SECTION) &&
                            functionMap.get(AssignmentConstants.GB_PERMISSION_GRADE_SECTION)) {
                        perm_level = messageLocator.getMessage("assignment2.permissions.grade.groups");
                        displayTAFootnote = true;
                    }
                }
                
                UIOutput.make(grade_perms, "gb_perm_desc", perm_level);
            }
            
            if (displayTAFootnote) {
                UIMessage.make(form, "grading_footnote", "assignment2.permissions.ta_footnote");
            }
        }
    }


    public ViewParameters getViewParameters() {
        return new AssignmentViewParams();
    }


    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }
    
    public void setAssignmentPermissionLogic(AssignmentPermissionLogic permissionLogic) {
        this.permissionLogic = permissionLogic;
    }
    
    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }

}