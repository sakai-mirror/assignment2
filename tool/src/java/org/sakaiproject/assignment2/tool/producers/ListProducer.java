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

package org.sakaiproject.assignment2.tool.producers;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.tool.api.Placement;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCase;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCaseReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.DefaultView;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;

/**
 * This renders the Instructor Landing Page that shows the list of assignments
 * in the course. Along with the ability to drag'n'drop reorder, delete, edit,
 * and go to the submissions.
 * 
 * @author rjlowe
 * @author sgithens
 *
 */
public class ListProducer implements ViewComponentProducer, NavigationCaseReporter, DefaultView {

    public static final String VIEW_ID = "list";

    public String getViewID() {
        return VIEW_ID;
    }

    private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }

    private Placement placement;
    public void setPlacement(Placement placement) {
        this.placement = placement;
    }

    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
        UIVerbatim.make(tofill, "asnnlist-decl-js", "var sakai = sakai || {};"
                + "sakai.curPlacement = '"+placement.getId()+"';"
                + "sakai.curContext = '"+externalLogic.getCurrentContextId()+"';");
    }

    @SuppressWarnings("unchecked")
    public List reportNavigationCases() {
        List<NavigationCase> nav= new ArrayList<NavigationCase>();
        nav.add(new NavigationCase("remove", new SimpleViewParameters(AjaxResultsProducer.VIEW_ID)));
        return nav;
    }

}
