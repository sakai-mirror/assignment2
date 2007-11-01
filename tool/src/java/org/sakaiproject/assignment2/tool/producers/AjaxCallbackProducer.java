package org.sakaiproject.assignment2.tool.producers;

import java.util.ArrayList;
import java.util.List;

import org.sakaiproject.assignment2.tool.params.AjaxCallbackViewParams;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.content.ContentTypeInfoRegistry;
import uk.org.ponder.rsf.content.ContentTypeReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class AjaxCallbackProducer implements ViewComponentProducer, ViewParamsReporter, ContentTypeReporter {
	
	public static final String VIEW_ID = "ajax-callback";
    public String getViewID() {
        return VIEW_ID;
    }
    
    private String[] assignmentIdParam;
    private ExternalLogic externalLogic;
    private AssignmentLogic assignmentLogic;
    
    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	//UIOutput.make(tofill, "test", "TEST THIS");
    	
    	AjaxCallbackViewParams params = (AjaxCallbackViewParams) viewparams;
    	
    	if (params.sortable != null){
    		assignmentIdParam = (String[]) params.sortable;
    	    	
	    	for (int i=0; i < assignmentIdParam.length; i++){
	    		Assignment2 assignment = assignmentLogic.getAssignmentById(Long.valueOf(assignmentIdParam[i].substring(3)));
	    		if (assignment != null){
	    			assignment.setSortIndex(i);
	    			assignmentLogic.saveAssignment(assignment);
	    		}
	    	}
	    }
    }
    
    public ViewParameters getViewParameters(){
    	return new AjaxCallbackViewParams();
    }
    
    public String getContentType() {
        return ContentTypeInfoRegistry.AJAX;
    }
    
    public void setExternalLogic(ExternalLogic externalLogic){
    	this.externalLogic = externalLogic;
    }
    
    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
    	this.assignmentLogic = assignmentLogic;
    }

}