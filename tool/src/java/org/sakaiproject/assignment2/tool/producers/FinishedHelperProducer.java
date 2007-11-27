package org.sakaiproject.assignment2.tool.producers;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCaseReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;

public class FinishedHelperProducer implements ViewComponentProducer
{
	  public static final String VIEWID = "FinishedHelper";
	  
	  public String getViewID() {
	    return VIEWID;
	  }
	
	  public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
		  
		  //Really do nothing, let the JS do it all, call thickbox close window and Ajax call
	  }
	
}