package org.sakaiproject.assignment2.tool.params;

import org.sakaiproject.assignment2.tool.beans.locallogic.LocalPermissionLogic;
import org.sakaiproject.assignment2.tool.producers.AuthorizationFailedProducer;

import uk.org.ponder.rsf.viewstate.AnyViewParameters;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsInterceptor;

public class Assignment2ViewParamsInterceptor implements ViewParamsInterceptor {

	private LocalPermissionLogic localPermissionLogic;
	public void setLocalPermissionLogic(LocalPermissionLogic localPermissionLogic){
		this.localPermissionLogic = localPermissionLogic;
	}
	
	
	public AnyViewParameters adjustViewParameters(ViewParameters incoming) {
		if (AuthorizationFailedProducer.VIEWID.equals(incoming.viewID)) {
			//Always return incoming if we are going to the Authorization Failed Page
			return incoming;
		}
		
		if (localPermissionLogic.checkCurrentUserHasViewPermission(incoming.viewID)){
			return incoming;
		}
		return new SimpleViewParameters(AuthorizationFailedProducer.VIEWID);
	}
	
}