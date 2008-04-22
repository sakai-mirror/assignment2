package org.sakaiproject.assignment2.tool.producers;

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.taggable.api.AssignmentActivityProducer;
import org.sakaiproject.assignment2.tool.beans.Assignment2Bean;
import org.sakaiproject.assignment2.tool.beans.locallogic.DecoratedTaggingProvider;
import org.sakaiproject.assignment2.tool.beans.locallogic.LocalAssignmentLogic;
import org.sakaiproject.assignment2.tool.params.AssignmentListSortViewParams;
import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.assignment2.tool.params.ViewSubmissionsViewParams;
import org.sakaiproject.assignment2.tool.producers.AssignmentProducer;
import org.sakaiproject.assignment2.tool.producers.ViewSubmissionsProducer;
import org.sakaiproject.assignment2.tool.producers.renderers.PagerRenderer;
import org.sakaiproject.assignment2.tool.producers.renderers.SortHeaderRenderer;
import org.sakaiproject.component.cover.ComponentManager;
import org.sakaiproject.component.cover.ServerConfigurationService;
import org.sakaiproject.taggable.api.TaggingHelperInfo;
import org.sakaiproject.taggable.api.TaggingManager;
import org.sakaiproject.taggable.api.TaggingProvider;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.*;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;
import uk.org.ponder.rsf.view.DefaultView;

public class AssignmentListSortViewProducer implements ViewComponentProducer, ViewParamsReporter, DefaultView {

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

    	//check if we need to duplicate an assignment, params.assignmentIdToDuplicate is not null
    	if (params.assignmentIdToDuplicate != null){
    		assignment2Bean.createDuplicate(params.assignmentIdToDuplicate);
    		params.assignmentIdToDuplicate = null;
    	}
    	
        /*List<Assignment2> entries = assignmentLogic.getViewableAssignments(currentUserId, current_sort_by, current_sort_dir.equals(SORT_DIR_ASC), 
        		params.current_start, params.current_count);*/
        
    	List<Assignment2> entries = assignmentLogic.getViewableAssignments();
    	
    	//Breadcrumbs
    	UIMessage.make(tofill, "last_breadcrumb", "assignment2.assignment_list-sortview.heading");
    	
        UIMessage.make(tofill, "page-title", "assignment2.assignment_list-sortview.title");
        //navBarRenderer.makeNavBar(tofill, "navIntraTool:", VIEW_ID);
        pagerRenderer.makePager(tofill, "pagerDiv:", VIEW_ID, viewparams, entries.size());
        
        UIVerbatim.make(tofill, "debug_info", "Currently, you are sorting by: <strong>" + current_sort_by + " " + 
        			current_sort_dir + "</strong>,   starting from record: <strong>" + params.current_start + "</strong> and paging: <strong>" + params.current_count + "</strong> items.");
        
        //UIMessage.make(tofill, "heading", "assignment2.assignment_list-sortview.heading");
        //Links
        if (edit_perm){
        	UIOutput.make(tofill, "navIntraTool");
        	UIInternalLink.make(tofill, "assignment_list-add-assignment-link", UIMessage.make("assignment2.assignment_add.title"),
        		new SimpleViewParameters(AssignmentProducer.VIEW_ID));
        }
                
        //table headers and sorting links
        if (edit_perm){
        	UIMessage.make(tofill, "tableheader.remove", "assignment2.assignment_list-sortview.tableheader.remove");
        }
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.assignment", viewparams, 
        		AssignmentLogic.SORT_BY_TITLE, "assignment2.assignment_list-sortview.tableheader.assignment");
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.for", viewparams, 
        		AssignmentLogic.SORT_BY_FOR, "assignment2.assignment_list-sortview.tableheader.for");
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.status", viewparams, 
        		AssignmentLogic.SORT_BY_STATUS, "assignment2.assignment_list-sortview.tableheader.status");
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.open", viewparams, 
        		AssignmentLogic.SORT_BY_OPEN, "assignment2.assignment_list-sortview.tableheader.open");
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.due", viewparams, 
        		AssignmentLogic.SORT_BY_DUE, "assignment2.assignment_list-sortview.tableheader.due");
        //sortHeaderRenderer.makeSortingLink(tofill, "tableheader.ungraded", viewparams, 
        //		AssignmentLogic.SORT_BY_NUM_UNGRADED, "assignment2.assignment_list-sortview.tableheader.ungraded");

              
        UIForm form = UIForm.make(tofill, "form");
                
        entries = (List<Assignment2>) localAssignmentLogic.filterPopulateAndSortAssignmentList(entries, params.current_start, params.current_count, 
        		current_sort_by, current_sort_dir.equals(AssignmentLogic.SORT_DIR_ASC));
        
        if (entries.size() <= 0) {
            UIMessage.make(tofill, "assignment_empty", "assignment2.assignment_list-sortview.assignment_empty");
            return;
        }
        
        //Fill out Table
        for (Assignment2 assignment : entries){
        	UIBranchContainer row = UIBranchContainer.make(form, "assignment-row:");
        	if (edit_perm){
        		UIOutput.make(row, "assignment_row_remove_col");
        		UIBoundBoolean.make(row, "assignment_row_remove", 
        			"Assignment2Bean.selectedIds." + assignment.getId(),
        			Boolean.FALSE);
        	}
        	UIMessage.make(row, "assignment_row_remove_label", "assignment2.assignment_list-sortview.assignment_row_remove_label");
        	String title = (assignment != null) ? assignment.getTitle() : "";
        	UIOutput.make(row, "assignment_title", title);
        	
        	//If Current User has the ability to edit or duplicate the assignment
        	if (edit_perm) {
	        	UIInternalLink.make(row, "assignment_row_edit", 
	        			UIMessage.make("assignment2.assignment_list-sortview.assignment_row_edit"), 
	        			new AssignmentViewParams(AssignmentProducer.VIEW_ID, assignment.getId()));
	        	UIInternalLink.make(row, "assignment_row_duplicate", 
	        			UIMessage.make("assignment2.assignment_list-sortview.assignment_row_duplicate"), 
	        			new AssignmentListSortViewParams(AssignmentListSortViewProducer.VIEW_ID, current_sort_by, current_sort_dir, 
	        					params.current_start, params.current_count, assignment.getId()));
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
        		
        		for (DecoratedTaggingProvider provider : providers)
        		{
        			UIBranchContainer tagLinks = UIBranchContainer.make(row, "tag_provider_links:");
        			String ref = assignmentActivityProducer.getActivity(
							assignment).getReference();
        			TaggingHelperInfo helper = provider.getProvider().getActivityHelperInfo(ref);
        			if (helper != null)
        			{
        				String url = ServerConfigurationService.getToolUrl() + "/" + 
        					helper.getPlacement() + "/" + helper.getHelperId() + 
        					".helper?1=1";
        				
        				for (String key : helper.getParameterMap().keySet()) {
        					url = url + "&session." + key + "=" + helper.getParameterMap().get(key);
        				}
        				
        				UILink.make(tagLinks, "assignment_view_links", helper.getName(), url);
        				
        				/*
        				 //This is commented out until RSF has some better helper support
        				UIInternalLink.make(tagLinks, "assignment_view_links", helper.getName(),
        		        		new TaggableHelperViewParams(TaggableHelperProducer.VIEWID, 
        		        				helper.getHelperId(), 
        		        				helper.getParameterMap().keySet().toArray(new String[0]), 
        		        				helper.getParameterMap().values().toArray(new String[0])));
        		        */
        			}
        		}
        	}

        	//Current user should always be able to grade, otherwise getViewableAssignments wouldn't have returned it... or at least it shouldn't ;-)
        	UIInternalLink.make(row, "assignment_row_grade", 
        			UIMessage.make("assignment2.assignment_list-sortview.assignment_row_grade"), 
        			new ViewSubmissionsViewParams(ViewSubmissionsProducer.VIEW_ID, assignment.getId()));
        	
        	UIOutput.make(row, "assignment_row_for", assignment.getRestrictedToText());
        	if (assignment.isDraft()){
        		UIOutput.make(row, "assignment_row_draft_td");
        		UIOutput.make(row, "assignment_row_draft", assignment.getAssignmentStatus());
        	} else {
        	   	UIOutput.make(row, "assignment_row_open_text", assignment.getAssignmentStatus());
        	}
        	UIOutput.make(row, "assignment_row_open", df.format(assignment.getOpenTime()));

        	if (assignment.getDueDate() != null) {
        		UIOutput.make(row, "assignment_row_due", df.format(assignment.getDueDate()));
        	} else {
        		UIOutput.make(row, "assignment_row_due", messageLocator.getMessage("assignment2.assignment_list-sortview.no_due_date"));	
        	}

        	//For JS Sorting
        	UIOutput.make(row, "status", assignment.getAssignmentStatus());
        	UIOutput.make(row, "open_timestamp", assignment.getOpenTime() != null ? String.valueOf(assignment.getOpenTime().getTime()) : "");
        	UIOutput.make(row, "due_timestamp", assignment.getDueDate() != null ? String.valueOf(assignment.getDueDate().getTime()) : "");
        	
        	//UIInternalLink.make(row, "assignment_row_in_new", "2/4", new SimpleViewParameters(GradeAssignmentProducer.VIEW_ID));
        }
        
        if (edit_perm) {
	        UICommand.make(form, "submit_remove", UIMessage.make("assignment2.assignment_list-sortview.submit_remove"),
	        		"Assignment2Bean.processActionRemove");
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
}