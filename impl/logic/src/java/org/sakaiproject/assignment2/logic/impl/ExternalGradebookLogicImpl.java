/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/api/logic/src/java/org/sakaiproject/assignment2/logic/ExternalGradebookLogicImpl.java $
 * $Id: ExternalGradebookLogic.java 12544 2006-05-03 15:06:26Z wagnermr@iupui.edu $
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

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.component.cover.ComponentManager;
import org.sakaiproject.service.gradebook.shared.GradebookService;


/**
 * This is the implementation for logic which is interacting with the Sakai
 * Gradebook tool
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class ExternalGradebookLogicImpl implements ExternalGradebookLogic {

    private static Log log = LogFactory.getLog(ExternalGradebookLogicImpl.class);
    
    GradebookService gradebookService = (org.sakaiproject.service.gradebook.shared.GradebookService) 
    ComponentManager.get("org.sakaiproject.service.gradebook.GradebookService"); 

    public void init() {
    	log.debug("init");
    }

    // Gradebook interaction
    public List<org.sakaiproject.service.gradebook.shared.Assignment> getViewableGradebookAssignments(String siteContext) {
    	if (siteContext == null) {
    		throw new IllegalArgumentException("Null site context passed to getViewableGradebookAssignments");
    	}
    	
    	//return gradebookService.getViewableAssignmentsForCurrentUser(siteContext);
    	return new ArrayList();
    }

}
