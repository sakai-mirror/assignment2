/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/branches/ASNN-617/impl/src/java/org/sakaiproject/assignment2/logic/impl/AssignmentBundleLogicImpl.java $
 * $Id: AssignmentBundleLogicImpl.java 61481 2009-06-29 18:47:43Z swgithen@mtu.edu $
 ***********************************************************************************
 *
 * Copyright (c) 2007 The Sakai Foundation.
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

package org.sakaiproject.assignment2.logic.impl;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.AssignmentAuthzLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.authz.api.FunctionManager;
import org.sakaiproject.authz.api.SecurityService;
import org.sakaiproject.site.api.SiteService;

/**
 * Used for Assignment2-specific authorization based upon
 * the fine-grained site-scoped Sakai permissions. This is solely used for
 * answering questions based upon these Sakai permissions and is not meant
 * for use outside the logic layer. For more situation-specific
 * permission answers, use the {@link AssignmentPermissionLogic}
 */
public class AssignmentAuthzLogicImpl implements AssignmentAuthzLogic
{
    private static Log log = LogFactory.getLog(AssignmentAuthzLogicImpl.class);
    
    private FunctionManager functionManager;
    private SecurityService securityService;
    private SiteService siteService;

    public void init() {
        if (log.isDebugEnabled()) log.debug("init");
        
        registerPermissions();
    }
    
    /**
     * Register the assignment2 permissions
     */
    protected void registerPermissions() {
        // register Sakai permissions for this tool
        functionManager.registerFunction(AssignmentConstants.PERMISSION_ADD_ASSIGNMENTS);
        functionManager.registerFunction(AssignmentConstants.PERMISSION_EDIT_ASSIGNMENTS);
        functionManager.registerFunction(AssignmentConstants.PERMISSION_SUBMIT);
        functionManager.registerFunction(AssignmentConstants.PERMISSION_ALL_GROUPS);
        functionManager.registerFunction(AssignmentConstants.PERMISSION_MANAGE_SUBMISSIONS);
        functionManager.registerFunction(AssignmentConstants.PERMISSION_VIEW_ASSIGNMENTS);
        functionManager.registerFunction(AssignmentConstants.PERMISSION_REMOVE_ASSIGNMENTS);
    }
    
    /**
     * 
     * @param contextId
     * @return true if the current user's role has the "add assignments" permission
     */
    public boolean userHasAddPermission(String contextId) {
        return hasPermission(contextId, AssignmentConstants.PERMISSION_ADD_ASSIGNMENTS);
    }
    
    /**
     * 
     * @param contextId
     * @return true if the current user's role has the "edit assignments" permission
     */
    public boolean userHasEditPermission(String contextId) {
        return hasPermission(contextId, AssignmentConstants.PERMISSION_EDIT_ASSIGNMENTS);
    }
    
    /**
     * 
     * @param contextId
     * @return true if the current user's role has the "delete assignments" permission
     */
    public boolean userHasDeletePermission(String contextId) {
        return hasPermission(contextId, AssignmentConstants.PERMISSION_REMOVE_ASSIGNMENTS);
    }
    
    /**
     * 
     * @param contextId
     * @return true if the current user's role permissions are for all groups
     */
    public boolean userHasAllGroupsPermission(String contextId) {
        return hasPermission(contextId, AssignmentConstants.PERMISSION_ALL_GROUPS);
    }
    
    /**
     * 
     * @param contextId
     * @return true if the current user's role has permission to submit assignments
     */
    public boolean userHasSubmitPermission(String contextId) {
        return hasPermission(contextId, AssignmentConstants.PERMISSION_SUBMIT);
    }
    
    private boolean hasPermission(String contextId, String permission) {
        return securityService.unlock(permission, siteService.siteReference(contextId));
    }
    
    private boolean hasPermission(String userId, String contextId, String permission) {
        return securityService.unlock(userId, permission, siteService.siteReference(contextId));
    }
    
    /* Dependencies */

    public void setFunctionManager(FunctionManager functionManager) {
        this.functionManager = functionManager;
    }
    
    public void setSecurityService(SecurityService securityService) {
        this.securityService = securityService;
    }
    
    public void setSiteService(SiteService siteService) {
        this.siteService = siteService;
    }
}