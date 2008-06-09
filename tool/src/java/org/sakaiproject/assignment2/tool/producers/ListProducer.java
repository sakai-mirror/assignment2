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
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.taggable.api.AssignmentActivityProducer;
import org.sakaiproject.assignment2.tool.beans.Assignment2Bean;
import org.sakaiproject.assignment2.tool.beans.locallogic.DecoratedTaggingProvider;
import org.sakaiproject.assignment2.tool.beans.locallogic.LocalAssignmentLogic;
import org.sakaiproject.assignment2.tool.params.AssignmentListSortViewParams;
import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.assignment2.tool.params.TaggableHelperViewParams;
import org.sakaiproject.assignment2.tool.params.ViewSubmissionsViewParams;
import org.sakaiproject.assignment2.tool.producers.renderers.PagerRenderer;
import org.sakaiproject.assignment2.tool.producers.renderers.SortHeaderRenderer;
import org.sakaiproject.component.cover.ComponentManager;
import org.sakaiproject.component.cover.ServerConfigurationService;
import org.sakaiproject.taggable.api.TaggingHelperInfo;
import org.sakaiproject.taggable.api.TaggingManager;
import org.sakaiproject.taggable.api.TaggingProvider;

import uk.org.ponder.htmlutil.HTMLUtil;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIBoundBoolean;
import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UICommand;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIForm;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UILink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.components.decorators.DecoratorList;
import uk.org.ponder.rsf.components.decorators.UIStyleDecorator;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.DefaultView;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class ListProducer implements ViewComponentProducer, ViewParamsReporter, DefaultView {

    public static final String VIEW_ID = "list";
   
    //sorting strings
    public static final String DEFAULT_SORT_DIR = AssignmentLogic.SORT_DIR_ASC;
    public static final String DEFAULT_OPPOSITE_SORT_DIR = AssignmentLogic.SORT_DIR_DESC;
    public static final String DEFAULT_SORT_BY = AssignmentLogic.SORT_BY_INDEX;
    
    private String current_sort_by = DEFAULT_SORT_BY;
    private String current_sort_dir = DEFAULT_SORT_DIR;
    private String opposite_sort_dir = DEFAULT_OPPOSITE_SORT_DIR;
    
    //images
    public static final String BULLET_UP_IMG_SRC = "/sakai-assignment2-tool/content/images/bullet_arrow_up.png";
    public static final String BULLET_DOWN_IMG_SRC = "/sakai-assignment2-tool/content/images/bullet_arrow_down.png";
    
    public String getViewID() {
        return VIEW_ID;
    }

    private PagerRenderer pagerRenderer;
    private MessageLocator messageLocator;
    private AssignmentLogic assignmentLogic;
    private AssignmentSubmissionLogic submissionLogic;
    private ExternalLogic externalLogic;
    private AssignmentPermissionLogic permissionLogic;
    private Locale locale;
    private Assignment2Bean assignment2Bean;
    private SortHeaderRenderer sortHeaderRenderer;
    private LocalAssignmentLogic localAssignmentLogic;
    
    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {

    	// use a date which is related to the current users locale
        DateFormat df = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT, locale);        
        
        //Edit Permission
        Boolean edit_perm = permissionLogic.isCurrentUserAbleToEditAssignments(externalLogic.getCurrentContextId());
        
    	//get parameters
    	AssignmentListSortViewParams params = (AssignmentListSortViewParams) viewparams;
    	if (params.sort_by == null) params.sort_by = DEFAULT_SORT_BY;
    	if (params.sort_dir == null) params.sort_dir = DEFAULT_SORT_DIR;
    	current_sort_by = params.sort_by;
    	current_sort_dir = params.sort_dir;
    	opposite_sort_dir = (AssignmentLogic.SORT_DIR_ASC.equals(current_sort_dir) ? AssignmentLogic.SORT_DIR_DESC : AssignmentLogic.SORT_DIR_ASC);
    	UIVerbatim.make(tofill, "defaultSortBy", HTMLUtil.emitJavascriptVar("defaultSortBy", DEFAULT_SORT_BY));
    	
    	//check if we need to duplicate an assignment, params.assignmentIdToDuplicate is not null
    	if (params.assignmentIdToDuplicate != null){
    		assignment2Bean.createDuplicate(params.assignmentIdToDuplicate);
    		params.assignmentIdToDuplicate = null;
    	}
    	
        /*List<Assignment2> entries = assignmentLogic.getViewableAssignments(currentUserId, current_sort_by, current_sort_dir.equals(SORT_DIR_ASC), 
        		params.current_start, params.current_count);*/
        
    	List<Assignment2> entries = assignmentLogic.getViewableAssignments();
    	
    	//Breadcrumbs
    	UIMessage.make(tofill, "last_breadcrumb", "assignment2.list.heading");
    	
    	//Links to settings and reorder
    	UIInternalLink.make(tofill, "settings_link", new SimpleViewParameters(ListProducer.VIEW_ID));
    	UIInternalLink.make(tofill, "reorder_link", new SimpleViewParameters(ListReorderProducer.VIEW_ID));
    	
    	
        UIMessage.make(tofill, "page-title", "assignment2.list.title");
        //navBarRenderer.makeNavBar(tofill, "navIntraTool:", VIEW_ID);
        pagerRenderer.makePager(tofill, "pagerDiv:", VIEW_ID, viewparams, entries.size());
        
        UIVerbatim.make(tofill, "debug_info", "Currently, you are sorting by: <strong>" + current_sort_by + " " + 
        			current_sort_dir + "</strong>,   starting from record: <strong>" + params.current_start + "</strong> and paging: <strong>" + params.current_count + "</strong> items.");
        
        //UIMessage.make(tofill, "heading", "assignment2.list.heading");
        //Links
        if (edit_perm){
        	UIInternalLink.make(tofill, "add_assignment", UIMessage.make("assignment2.list.add_assignment"),
        		new SimpleViewParameters(AssignmentProducer.VIEW_ID));
        }
                
        //table headers and sorting links
        if (edit_perm){
        	UIMessage.make(tofill, "tableheader.remove", "assignment2.list.tableheader.remove");
        }
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.assignment", viewparams, 
        		AssignmentLogic.SORT_BY_TITLE, "assignment2.list.tableheader.assignment");
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.for", viewparams, 
        		LocalAssignmentLogic.SORT_BY_FOR, "assignment2.list.tableheader.for");
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.status", viewparams, 
        		LocalAssignmentLogic.SORT_BY_STATUS, "assignment2.list.tableheader.status");
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.open", viewparams, 
        		AssignmentLogic.SORT_BY_OPEN, "assignment2.list.tableheader.open");
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.due", viewparams, 
        		AssignmentLogic.SORT_BY_DUE, "assignment2.list.tableheader.due");
        //sortHeaderRenderer.makeSortingLink(tofill, "tableheader.ungraded", viewparams, 
        //		AssignmentLogic.SORT_BY_NUM_UNGRADED, "assignment2.list.tableheader.ungraded");

              
        UIForm form = UIForm.make(tofill, "form");
        
        if (entries.size() <= 0) {
            UIMessage.make(tofill, "assignment_empty", "assignment2.list.assignment_empty");
            return;
        }
        
        // retrieve groups here for display of group restrictions
        Map<String, String> groupIdToNameMap = externalLogic.getGroupIdToNameMapForSite(externalLogic.getCurrentContextId());
        
        //Fill out Table
        for (Assignment2 assignment : entries){
        	UIBranchContainer row = UIBranchContainer.make(form, "assignment-row:");
        	if (edit_perm){
        		UIBoundBoolean.make(row, "assignment_row_remove", 
        			"Assignment2Bean.selectedIds." + assignment.getId(),
        			Boolean.FALSE);
        	}
        	UIOutput title = UIOutput.make(row, "assignment_title", (assignment != null) ? assignment.getTitle() : "");
        	
        	//If Current User has the ability to edit or duplicate the assignment
        	if (edit_perm) {
        		UICommand.make(row, "assignment_delete");
	        	UIInternalLink.make(row, "assignment_edit", 
	        			UIMessage.make("assignment2.list.edit"), 
	        			new AssignmentViewParams(AssignmentProducer.VIEW_ID, assignment.getId()));
        	}
        	
        	// Tag provider stuff
        	TaggingManager taggingManager = (TaggingManager) ComponentManager
        	.get("org.sakaiproject.taggable.api.TaggingManager");
        	if (taggingManager.isTaggable() && assignment != null)
        	{
        		//TODO: optimize?
        		List<DecoratedTaggingProvider> providers = initDecoratedProviders();
        		
        		AssignmentActivityProducer assignmentActivityProducer = (AssignmentActivityProducer) ComponentManager
        		.get("org.sakaiproject.assignment2.taggable.api.AssignmentActivityProducer");
        		
        		/* Removing support for Assignments2 and matrix linking for now
        		for (DecoratedTaggingProvider provider : providers)
        		{
        			UIBranchContainer tagLinks = UIBranchContainer.make(row, "tag_provider_links:");
        			String ref = assignmentActivityProducer.getActivity(
							assignment).getReference();
        			TaggingHelperInfo helper = provider.getProvider().getActivityHelperInfo(ref);
        			if (helper != null)
        			{
        				//String url = ServerConfigurationService.getToolUrl() + "/" + 
        				//	helper.getPlacement() + "/" + helper.getHelperId() + 
        				//	".helper?1=1";
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
        		*/
        	}

        	// Submitted/Total display
        	int total = 0;
        	int withSubmission = 0;
        	List<String> viewableStudents = permissionLogic.getViewableStudentsForUserForItem(assignment);
        	if (viewableStudents != null) {
        		total = viewableStudents.size();
        		if (total > 0) {
        			withSubmission = submissionLogic.getNumStudentsWithASubmission(assignment, viewableStudents);
        		}
        	}

        	UIInternalLink.make(row, "grade", 
        			messageLocator.getMessage("assignment2.list.submissions_link", new Object[]{ withSubmission, total}), 
        			new ViewSubmissionsViewParams(ViewSubmissionsProducer.VIEW_ID, assignment.getId()));
        	
        	
        	// group restrictions
        	if (assignment.getAssignmentGroupSet() != null && !assignment.getAssignmentGroupSet().isEmpty()) {
        		title.decorators = new DecoratorList(new UIStyleDecorator("group"));
        	}
        	
        	if (assignment.isDraft()){
        		UIMessage.make(row, "draft", "assignment2.list.draft");
        	}
        	
        	UIOutput divLeftContainer = UIOutput.make(row, "div-left-container");
        	//find active
        	if (assignment.isOpen())
        	{
        		//show active styleclass
        		divLeftContainer.decorators = new DecoratorList(new UIStyleDecorator("assignActive"));
        		
        	}else{
        		//show inactive styleclass
        		divLeftContainer.decorators = new DecoratorList(new UIStyleDecorator("assignInactive"));
        	}
        	UIOutput.make(row, "assignment_row_open", df.format(assignment.getOpenTime()));

        	if (assignment.getDueDate() != null) {
        		UIOutput.make(row, "assignment_row_due", df.format(assignment.getDueDate()));
        	} else {
        		UIMessage.make(row, "assignment_row_due", "assignment2.list.no_due_date");	
        	}

        }
        
    }
    
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
    
    public ViewParameters getViewParameters() {
    	return new AssignmentListSortViewParams();
    }
    
    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }
    
    public void setPagerRenderer(PagerRenderer pagerRenderer) {
    	this.pagerRenderer = pagerRenderer;
    }
      
    public void setAssignmentLogic (AssignmentLogic assignmentLogic) {
    	this.assignmentLogic = assignmentLogic;
    }
    
    public void setExternalLogic(ExternalLogic externalLogic) {
    	this.externalLogic = externalLogic;
    }
    
    public void setLocale(Locale locale) {
    	this.locale = locale;
    }
    
    public void setAssignment2Bean(Assignment2Bean assignment2Bean) {
    	this.assignment2Bean = assignment2Bean;
    }
    
    public void setSortHeaderRenderer(SortHeaderRenderer sortHeaderRenderer) {
    	this.sortHeaderRenderer = sortHeaderRenderer;
    }
	
	public void setPermissionLogic(AssignmentPermissionLogic permissionLogic) {
		this.permissionLogic = permissionLogic;
	}
	
	public void setLocalAssignmentLogic(LocalAssignmentLogic localAssignmentLogic) {
		this.localAssignmentLogic = localAssignmentLogic;
	}
	
	public void setAssignmentSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
		this.submissionLogic = submissionLogic;
	}
}