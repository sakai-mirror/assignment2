/**********************************************************************************
 * $URL:https://source.sakaiproject.org/contrib/assignment2/trunk/tool/src/java/org/sakaiproject/assignment2/tool/beans/locallogic/LocalAssignmentLogic.java $
 * $Id:LocalAssignmentLogic.java 48274 2008-04-23 20:07:00Z wagnermr@iupui.edu $
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

package org.sakaiproject.assignment2.tool;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.component.api.ServerConfigurationService;


/**
 * Contains methods that are used by the UI. These methods will be used if your
 * implemented content review service is Turnitin.
 */
public class LocalTurnitinLogic {

    private static final Log LOG = LogFactory.getLog(LocalTurnitinLogic.class);
    
    private ServerConfigurationService serverConfigurationService;

    /**
     * if you want to restrict how your content is submitted to a repository, 
     * include this property in sakai.properties. Possible values are {@link #VALUE_NO_REPO}, 
     * {@link #VALUE_INSTITUTION_REPO}, {@link #VALUE_STANDARD_REPO}.
     * If you do not include this property, all three options will be available
     * to your users.
     */
    public static final String PROP_SUBMIT_TO_REPO = "turnitin.repository.setting";
    
    /**
     * use this option if you do not want submissions saved to a repository.
     */
    public static final String VALUE_NO_REPO = "0";
    /**
     * use this option if you want submissions saved only to your institutional repository.
     */
    public static final String VALUE_INSTITUTION_REPO = "1";
    /**
     * use this option if you want all submissions saved to the content review service's
     * standard repository. This generally means that submissions from your institutions
     * can be used to check plagiarism at other institutions.
     */
    public static final String VALUE_STANDARD_REPO = "2";
    
    /**
     * Optional property to set the name of your institutional repository most likely
     * for use in the UI
     */
    public static final String PROP_INSTITUTION_REPO_NAME = "turnitin.repository.institutional.name";

    /**
     * 
     * @return the submission repository restriction set via sakai.properties. possible values
     * are {@link #VALUE_NO_REPO}, {@link #VALUE_INSTITUTION_REPO}, {@link #VALUE_STANDARD_REPO}.
     * Returns null if there is no restriction on the submission repository.
     */
    public String getSubmissionRepositoryRestriction() {
        // we return null if the property does not exist or is not valid
        String submissionRepoSetting = null;
        String propertyVal = serverConfigurationService.getString(PROP_SUBMIT_TO_REPO);
        if (propertyVal != null && 
                (propertyVal.equals(VALUE_NO_REPO) || 
                 propertyVal.equals(VALUE_INSTITUTION_REPO) ||
                 propertyVal.equals(VALUE_STANDARD_REPO))) {
            submissionRepoSetting = propertyVal;
        }
        
        return submissionRepoSetting;
    }
    
    /**
     * 
     * @return Returns the value for the institutional repository name set in sakai.properties.
     * Returns null if no value was set.
     */
    public String getInstitutionalRepositoryName() {
        String repoName = null;
        String prop = serverConfigurationService.getString(PROP_INSTITUTION_REPO_NAME);
        if (prop != null && !"".equals(prop.trim())) {
            repoName = prop;
        }
        
        return repoName;
    }
    
    public void setServerConfigurationService(ServerConfigurationService serverConfigurationService) {
        this.serverConfigurationService = serverConfigurationService;
    }

}