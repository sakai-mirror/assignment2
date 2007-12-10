package org.sakaiproject.assignment2.tool.producers;

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.tool.beans.Assignment2Bean;
import org.sakaiproject.assignment2.tool.params.AssignmentListSortViewParams;
import org.sakaiproject.assignment2.tool.params.SimpleAssignmentViewParams;

import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UILink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class StudentAssignmentListProducer implements ViewComponentProducer, ViewParamsReporter {
	
	public static final String VIEW_ID = "student-assignment-list";
	public String getViewID(){
		return this.VIEW_ID;
	}
	
	private PagerRenderer pagerRenderer;
    private AssignmentLogic assignmentLogic;
    private ExternalLogic externalLogic;
    private Locale locale;
    private Assignment2Bean assignment2Bean;
    private SortHeaderRenderer sortHeaderRenderer;
    
    //sorting strings
    public static final String SORT_DIR_ASC = "asc";
    public static final String SORT_DIR_DESC = "desc";
    public static final String SORT_BY_SORT_INDEX = "sortIndex";
    public static final String SORT_BY_ASSIGNMENT = "title";
    public static final String SORT_BY_FOR = "restrictedToGroups";
    public static final String SORT_BY_STATUS = "draft";
    public static final String SORT_BY_OPEN = "openTime";
    public static final String SORT_BY_DUE = "dueDateForUngraded";		//change me to due date

    //public static final String SORT_BY_SCALE = "scale";			//fix me
    public static final String DEFAULT_SORT_DIR = SORT_DIR_ASC;
    public static final String DEFAULT_OPPOSITE_SORT_DIR = SORT_DIR_DESC;
    public static final String DEFAULT_SORT_BY = SORT_BY_SORT_INDEX;
    
    private String current_sort_by = DEFAULT_SORT_BY;
    private String current_sort_dir = DEFAULT_SORT_DIR;
    private String opposite_sort_dir = DEFAULT_OPPOSITE_SORT_DIR;
    
    //images
    public static final String BULLET_UP_IMG_SRC = "/sakai-assignment2-tool/content/images/bullet_arrow_up.png";
    public static final String BULLET_DOWN_IMG_SRC = "/sakai-assignment2-tool/content/images/bullet_arrow_down.png";
    public static final String ATTACH_IMG_SRC = "/sakai-assignment2-tool/content/images/attach.png";

	
	public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	String currentUserId = externalLogic.getCurrentUserId();
    	
    	// use a date which is related to the current users locale
        DateFormat df = DateFormat.getDateInstance(DateFormat.MEDIUM, locale);
    	
    	//get parameters
    	AssignmentListSortViewParams params = (AssignmentListSortViewParams) viewparams;
    	if (params.sort_by == null) params.sort_by = DEFAULT_SORT_BY;
    	if (params.sort_dir == null) params.sort_dir = DEFAULT_SORT_DIR;
    	current_sort_by = params.sort_by;
    	current_sort_dir = params.sort_dir;
    	opposite_sort_dir = (SORT_DIR_ASC.equals(current_sort_dir) ? SORT_DIR_DESC : SORT_DIR_ASC);

    	//check if we need to duplicate an assignment, params.assignmentIdToDuplicate is not null
    	if (params.assignmentIdToDuplicate != null){
    		assignment2Bean.createDuplicate(params.assignmentIdToDuplicate);
    		params.assignmentIdToDuplicate = null;
    	}
    	
    	//get paging data
    	int total_count = assignmentLogic.getTotalCountViewableAssignments(currentUserId);
    		
        UIMessage.make(tofill, "page-title", "assignment2.student-assignment-list.title");
        pagerRenderer.makePager(tofill, "pagerDiv:", VIEW_ID, viewparams, total_count);
        
        //table headers and sorting links
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.assignment", viewparams, 
        		SORT_BY_ASSIGNMENT, "assignment2.student-assignment-list.tableheader.assignment");
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.for", viewparams, 
        		SORT_BY_FOR, "assignment2.student-assignment-list.tableheader.for");
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.status", viewparams, 
        		SORT_BY_STATUS, "assignment2.student-assignment-list.tableheader.status");
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.open", viewparams, 
        		SORT_BY_OPEN, "assignment2.student-assignment-list.tableheader.open");
        sortHeaderRenderer.makeSortingLink(tofill, "tableheader.due", viewparams, 
        		SORT_BY_DUE, "assignment2.student-assignment-list.tableheader.due");

        
        //Table TIME!!!! WOOHOO
        List<Assignment2> entries = new ArrayList<Assignment2>();
        entries = assignmentLogic.getViewableAssignments();
        
        entries = assignment2Bean.filterListForPaging(entries, params.current_start, params.current_count);
        assignment2Bean.populateNonPersistedFieldsForAssignments(entries);
        
        if (entries.size() <= 0) {
            UIMessage.make(tofill, "assignment_empty", "assignment2.student-assignment-list.assignment_empty");
            return;
        }
        //TEMP REMOVE
        entries.add(assignmentLogic.getAssignmentById(((Integer)2).longValue()));
        
        //Fill out Table
        for (Assignment2 assignment : entries){
        	UIBranchContainer row = UIBranchContainer.make(tofill, "assignment-row:");
        	
        	UILink.make(row, "attachments", ATTACH_IMG_SRC);
        	UIInternalLink.make(row, "assignment_link", assignment.getTitle(), 
        			new SimpleAssignmentViewParams(StudentSubmitProducer.VIEW_ID, assignment.getAssignmentId()));
        	UIOutput.make(row, "assignment_row_for", assignment.getRestrictedToText());
        	UIOutput.make(row, "assignment_row_status", "Not Started");
        	UIOutput.make(row, "assignment_row_open", df.format(assignment.getOpenTime()));
        	if (assignment.isUngraded()) {
        		if (assignment.getDueDateForUngraded() != null) {
        			UIOutput.make(row, "assignment_row_due", df.format(assignment.getDueDateForUngraded()));
        		} else {
        			UIMessage.make(row, "assignment_row_due", "assignment2.student-assignment-list.no_due_date");
        		}
        	} else {
        		if (assignment.getDueDate() != null) {
        			UIOutput.make(row, "assignment_row_due", df.format(assignment.getDueDate()));
        		} else {
        			UIMessage.make(row, "assignment_row_due", "assignment2.student-assignment-list.no_due_date");	
        		}
        	}
        }
	}
	
	public ViewParameters getViewParameters() {
    	return new AssignmentListSortViewParams();
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
}