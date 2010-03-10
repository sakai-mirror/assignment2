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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIMessage;
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

    @SuppressWarnings("unchecked")
    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
        
        String currContextId = externalLogic.getCurrentContextId();
        
        // This information is displayed in the instructions
        String toolTitle = externalLogic.getToolTitle();
        String siteTitle = externalLogic.getSiteTitle(currContextId);
        String siteId = currContextId;
        UIMessage.make(tofill, "instructions", "assignment2.permissions.instructions", new Object[] {toolTitle, siteTitle, siteId});
        
    }


    public ViewParameters getViewParameters() {
        return new AssignmentViewParams();
    }


    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }

}