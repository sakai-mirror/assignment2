/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/branches/ASNN-617/api/src/java/org/sakaiproject/assignment2/logic/AssignmentBundleLogic.java $
 * $Id: AssignmentBundleLogic.java 61480 2009-06-29 18:39:09Z swgithen@mtu.edu $
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

package org.sakaiproject.assignment2.logic;


/**
 * This interface is used for Assignment2-specific authorization based upon
 * the fine-grained site-scoped Sakai permissions. This is solely used for
 * answering questions based upon these Sakai permissions and is not meant
 * for use outside the logic layer. For more situation-specific
 * permission answers, use the {@link AssignmentPermissionLogic}
 *
 */
public interface AssignmentAuthzLogic {
    
    /**
     * 
     * @param contextId
     * @return true if the current user's role has the "add assignments" permission
     */
    public boolean userHasAddPermission(String contextId);
    
    /**
     * 
     * @param contextId
     * @return true if the current user's role has the "edit assignments" permission
     */
    public boolean userHasEditPermission(String contextId);
    
    /**
     * 
     * @param contextId
     * @return true if the current user's role has the "delete assignments" permission
     */
    public boolean userHasDeletePermission(String contextId);
    
    /**
     * 
     * @param contextId
     * @return true if the current user's role permissions are for all groups
     */
    public boolean userHasAllGroupsPermission(String contextId);
    
    /**
     * 
     * @param contextId
     * @return true if the current user's role has permission to submit assignments
     */
    public boolean userHasSubmitPermission(String contextId);

}