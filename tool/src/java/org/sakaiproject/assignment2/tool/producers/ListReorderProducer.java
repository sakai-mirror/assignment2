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
import java.util.List;
import java.util.Locale;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UICommand;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIForm;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UILink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.decorators.DecoratorList;
import uk.org.ponder.rsf.components.decorators.UIRowSpanDecorator;
import uk.org.ponder.rsf.components.decorators.UIStyleDecorator;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCase;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCaseReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;

public class ListReorderProducer implements ViewComponentProducer, NavigationCaseReporter{

    public static final String VIEW_ID = "list-reorder";
    public String getViewID() {
        return VIEW_ID;
    }

    private AssignmentLogic assignmentLogic;
    private Locale locale;
    private MessageLocator messageLocator;
    private ExternalLogic externalLogic;

    @SuppressWarnings("unchecked")
	public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {

    	//use a date which is related to the current users locale
    	DateFormat df = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT, locale);
    	
    	// getViewableAssignments won't return a null object; empty list if nothing found
        List<Assignment2> entries = assignmentLogic.getViewableAssignments();
        
        UIMessage.make(tofill, "page-title", "assignment2.assignment_list-reorder.title");
        UIMessage.make(tofill, "heading", "assignment2.list.heading");
        
      //Breadcrumbs
        UIInternalLink.make(tofill, "breadcrumb", 
        		messageLocator.getMessage("assignment2.list.heading"),
        		new SimpleViewParameters(ListProducer.VIEW_ID));
        UIMessage.make(tofill, "last_breadcrumb", "assignment2.list-reorder.reorder");

        if (entries.size() == 0) {
            UIMessage.make(tofill, "assignment_empty", "assignment2.list-reorder.assignment_empty");
            return;
        }
        int i=0;
        UIOutput holder = null;
        for (Assignment2 assignment : entries){
        	UIBranchContainer row = UIBranchContainer.make(tofill, "row:");
        	
        	if (i > 0) {
        		UILink.make(row, "arrow_up", "/sakai-assignment2-tool/content/images/bullet_arrow_up.png");
        	} 
        	if (i < entries.size() -1 ) {
        		UILink.make(row, "arrow_down", "/sakai-assignment2-tool/content/images/bullet_arrow_down.png");
        	}
        	if (i == 0 || holder == null) {
        		holder = UIOutput.make(row, "holder");
        		holder.decorators = new DecoratorList(new UIRowSpanDecorator(entries.size()));
        	}
        	
        	UIBranchContainer assignment_row = UIBranchContainer.make(tofill, "assignments:");
        	assignment_row.decorators = new DecoratorList(new UIStyleDecorator("sortable_" + assignment.getId().toString()));
        	
        	UIOutput.make(assignment_row, "row_title", assignment.getTitle());
        	if (assignment.getOpenDate() != null) {
        		UIOutput.make(assignment_row, "row_open", df.format(assignment.getOpenDate()));
        	} else {
        		UIMessage.make(assignment_row, "row_open", "assignment2.list-reorder.no_open_date");
        	}
        	
        	if (assignment.getDueDate() != null) {
        		UIOutput.make(assignment_row, "row_due", df.format(assignment.getDueDate()));
        	} else {
        		UIMessage.make(assignment_row, "row_due", "assignment2.list-reorder.no_due_date");
        	}
        	i++;
        }
        UIForm form = UIForm.make(tofill, "form");
        UICommand.make(form, "save", messageLocator.getMessage("assignment2.list-reorder.save"), "Assignment2Bean.processSaveReorder");
        UICommand.make(form, "cancel", messageLocator.getMessage("assignment2.list-reorder.cancel"), "Assignment2Bean.processCancelReorder");
    }
    
    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
    	this.assignmentLogic = assignmentLogic;
    }
    
    public void setLocale(Locale locale) {
    	this.locale = locale;
    }
	public void setMessageLocator(MessageLocator messageLocator)
	{
		this.messageLocator = messageLocator;
	}

	public List reportNavigationCases()
	{
		List<NavigationCase> nav = new ArrayList<NavigationCase>();
		nav.add(new NavigationCase("save", new SimpleViewParameters(ListProducer.VIEW_ID)));
		nav.add(new NavigationCase("cancel", new SimpleViewParameters(ListProducer.VIEW_ID)));
		return nav;
	}

	public void setExternalLogic(ExternalLogic externalLogic)
	{
		this.externalLogic = externalLogic;
	}
}