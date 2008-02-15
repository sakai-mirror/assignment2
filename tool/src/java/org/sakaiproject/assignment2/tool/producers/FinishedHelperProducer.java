package org.sakaiproject.assignment2.tool.producers;

import org.sakaiproject.assignment2.tool.params.FinishedHelperViewParameters;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;
import uk.org.ponder.htmlutil.HTMLUtil;

public class FinishedHelperProducer implements ViewComponentProducer, ViewParamsReporter
{
	  public static final String VIEWID = "FinishedHelper";
	  
	  public String getViewID() {
	    return VIEWID;
	  }
	
	  public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
		  
		  FinishedHelperViewParameters params = (FinishedHelperViewParameters) viewparams;
		  
		  //Really do nothing, let the JS do it all, call thickbox close window and Ajax call
		  // except call frame resize because the parent document window may have changed
		  
		  UIVerbatim.make(tofill, "sizeFrame",
				  HTMLUtil.emitJavascriptCall("parent.a2SetMainFrameHeight",
						  new String[] {org.sakaiproject.util.Web.escapeJavascript(
								  "Main" + org.sakaiproject.tool.cover.ToolManager.getCurrentPlacement().getId())
				  			}
				  )
		  );
		  
		  if (params.value != null && !params.value.equals("")) {
			  UIVerbatim.make(tofill, "useValue", 
					  HTMLUtil.emitJavascriptCall("parent.useValue", params.value));
		  }
	  }

	public ViewParameters getViewParameters() {
		return new FinishedHelperViewParameters();
	}
	
}