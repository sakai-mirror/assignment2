package org.sakaiproject.assignment2.tool.producers;

import java.util.ArrayList;
import java.util.List;

import org.sakaiproject.assignment2.tool.params.AjaxCallbackViewParams;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.content.ContentTypeInfoRegistry;
import uk.org.ponder.rsf.content.ContentTypeReporter;
import uk.org.ponder.rsf.evolvers.TextInputEvolver;
import uk.org.ponder.rsf.flow.ARIResult;
import uk.org.ponder.rsf.flow.ActionResultInterceptor;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCase;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCaseReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class AjaxCallbackProducer implements ViewComponentProducer, ViewParamsReporter, ContentTypeReporter {
	
	public static final String VIEW_ID = "ajax-callback";
    public String getViewID() {
        return VIEW_ID;
    }
    
    private String[] assignmentIdParam;
    
    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	//UIOutput.make(tofill, "test", "TEST THIS");
    	
    	AjaxCallbackViewParams params = (AjaxCallbackViewParams) viewparams;
    	
    	if (params.sortable != null){
    		assignmentIdParam = (String[]) params.sortable;
    	    	
	    	for (int i=0; i < assignmentIdParam.length; i++){
	    		UIOutput.make(tofill, ":" + i, "Homework Example " + assignmentIdParam[i].substring(3));    		
	    	}
	    }
    }
    
    public ViewParameters getViewParameters(){
    	return new AjaxCallbackViewParams();
    }
    
    public String getContentType() {
        return ContentTypeInfoRegistry.AJAX;
    }

}