package org.sakaiproject.assignment2.tool.producers;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;

public class AuthorizationFailedProducer implements ViewComponentProducer
{
	  public static final String VIEWID = "authorizationFailed";
	  
	  public String getViewID() {
	    return VIEWID;
	  }

	public void fillComponents(UIContainer tofill, ViewParameters viewparams,
			ComponentChecker checker) {

		//Really do nothing here except echo
		UIMessage.make(tofill, "permissions_error", "assignment2.authorizationFailed.permissions_error");
		
	}
}