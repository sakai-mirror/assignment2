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

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.tool.DecoratedTaggingProvider;
import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.assignment2.tool.params.RemoveAssignmentParams;
import org.sakaiproject.assignment2.tool.params.ViewSubmissionsViewParams;
import org.sakaiproject.component.cover.ComponentManager;
import org.sakaiproject.taggable.api.TaggingManager;
import org.sakaiproject.taggable.api.TaggingProvider;
import org.sakaiproject.tool.api.Placement;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.components.decorators.DecoratorList;
import uk.org.ponder.rsf.components.decorators.UIFreeAttributeDecorator;
import uk.org.ponder.rsf.components.decorators.UIStyleDecorator;
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

    private MessageLocator messageLocator;
    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }

    private AssignmentLogic assignmentLogic;
    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
        this.assignmentLogic = assignmentLogic;
    }

    private AssignmentSubmissionLogic submissionLogic;
    public void setSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
        this.submissionLogic = submissionLogic;
    }

    private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }

    private AssignmentPermissionLogic permissionLogic;
    public void setPermissionLogic(AssignmentPermissionLogic permissionLogic) {
        this.permissionLogic = permissionLogic;
    }

    private Locale locale;
    public void setLocale(Locale locale) {
        this.locale = locale;
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

    public List reportNavigationCases() {
        List<NavigationCase> nav= new ArrayList<NavigationCase>();
        nav.add(new NavigationCase("remove", new SimpleViewParameters(AjaxResultsProducer.VIEW_ID)));
        return nav;
    }

    /*
    private List<DecoratedTaggingProvider> initDecoratedProviders() {
        TaggingManager taggingManager = (TaggingManager) ComponentManager
        .get("org.sakaiproject.taggable.api.TaggingManager");
        List<DecoratedTaggingProvider> providers = new ArrayList<DecoratedTaggingProvider>();
        for (TaggingProvider provider : taggingManager.getProviders())
        {
            providers.add(new DecoratedTaggingProvider(provider));
        }
        return providers;
    }
     */

    /**
     * Functionality for matrix tagging. Currently on Hold. ASNN-113
     */
    private void renderMatrixTagging() {
        /*** Removing support for Assignments2 and matrix linking for now
        TaggingManager taggingManager = (TaggingManager) ComponentManager.get("org.sakaiproject.taggable.api.TaggingManager");
        if (taggingManager.isTaggable() && assignment != null){
                //TODO: optimize?
                List<DecoratedTaggingProvider> providers = initDecoratedProviders();

                AssignmentActivityProducer assignmentActivityProducer = (AssignmentActivityProducer) ComponentManager
                .get("org.sakaiproject.assignment2.taggable.api.AssignmentActivityProducer");

                for (DecoratedTaggingProvider provider : providers){
                        UIBranchContainer tagLinks = UIBranchContainer.make(row, "tag_provider_links:");
                        String ref = assignmentActivityProducer.getActivity(
                                                assignment).getReference();
                        TaggingHelperInfo helper = provider.getProvider().getActivityHelperInfo(ref);
                        if (helper != null){
                                //String url = ServerConfigurationService.getToolUrl() + "/" + 
                                //      helper.getPlacement() + "/" + helper.getHelperId() + 
                                //      ".helper?1=1";
                                String url = "/?1=1";
                                for (String key : helper.getParameterMap().keySet()) {
                                        url = url + "&" + key + "=" + helper.getParameterMap().get(key);
                                }

                                //UILink.make(tagLinks, "assignment_view_links", helper.getName(), url);                                        

                                 //This is commented out until RSF has some better helper support
                                UIInternalLink.make(tagLinks, "assignment_view_links", helper.getName(),
                                        new TaggableHelperViewParams(TaggableHelperProducer.VIEWID, 
                                                        helper.getHelperId(), 
                                                        helper.getParameterMap().keySet().toArray(new String[0]), 
                                                        helper.getParameterMap().values().toArray(new String[0])));
                        }
                }
        }
         */
    }

}
