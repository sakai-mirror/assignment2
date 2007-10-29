package org.sakaiproject.assignment2.logic.impl;

import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Date;
import java.util.Collection;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.authz.api.FunctionManager;
import org.sakaiproject.authz.api.SecurityService;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.component.cover.ServerConfigurationService;
import org.sakaiproject.entity.api.Entity;
import org.sakaiproject.exception.IdUnusedException;
import org.sakaiproject.site.api.Site;
import org.sakaiproject.site.api.SiteService;
import org.sakaiproject.tool.api.Session;
import org.sakaiproject.tool.api.SessionManager;
import org.sakaiproject.tool.api.ToolManager;
import org.sakaiproject.user.api.User;
import org.sakaiproject.user.api.UserDirectoryService;
import org.sakaiproject.user.api.UserNotDefinedException;
import org.sakaiproject.util.FormattedText;

/**
 * This is the implementation for logic which is external to our app logic
 * 
 * @author Sakai App Builder -AZ
 */
public class ExternalLogicImpl implements ExternalLogic {

    private static Log log = LogFactory.getLog(ExternalLogicImpl.class);

    private FunctionManager functionManager;
    public void setFunctionManager(FunctionManager functionManager) {
        this.functionManager = functionManager;
    }

    private ToolManager toolManager;
    public void setToolManager(ToolManager toolManager) {
        this.toolManager = toolManager;
    }

    private SecurityService securityService;
    public void setSecurityService(SecurityService securityService) {
        this.securityService = securityService;
    }

    private SessionManager sessionManager;
    public void setSessionManager(SessionManager sessionManager) {
        this.sessionManager = sessionManager;
    }

    private SiteService siteService;
    public void setSiteService(SiteService siteService) {
        this.siteService = siteService;
    }

    private UserDirectoryService userDirectoryService;
    public void setUserDirectoryService(UserDirectoryService userDirectoryService) {
        this.userDirectoryService = userDirectoryService;
    }

    private static final String ANON_USER_ATTRIBUTE = "AnonUserAttribute";

    /**
     * Place any code that should run when this class is initialized by spring here
     */
    public void init() {
        log.debug("init");
        // register Sakai permissions for this tool
        functionManager.registerFunction(ASSIGNMENT2_CREATE);
        functionManager.registerFunction(ASSIGNMENT2_DELETE);
        functionManager.registerFunction(ASSIGNMENT2_REVISE);
        functionManager.registerFunction(ASSIGNMENT2_SUBMIT);
        functionManager.registerFunction(ASSIGNMENT2_READ);
        functionManager.registerFunction(ASSIGNMENT2_RECEIVE_NOTIF);
        functionManager.registerFunction(ASSIGNMENT2_ALL_GROUPS_UNGRADED);
    }

    public String getCurrentLocationId() {
        try {
            Site s = siteService.getSite(toolManager.getCurrentPlacement().getContext());
            return s.getReference(); // get the entity reference to the site
        } catch (IdUnusedException e) {
            return NO_LOCATION;
        }
    }

    public String getCurrentUserId() {
        return sessionManager.getCurrentSessionUserId();
    }

    public String getUserDisplayName(String userId) {
        try {
            User user = userDirectoryService.getUser(userId);
            return user.getDisplayName();
        } catch (UserNotDefinedException ex) {
            log.error("Could not get user from userId: " + userId, ex);
        }

        return "----------";
    }

    public boolean isUserAdmin(String userId) {
        return securityService.isSuperUser(userId);
    }

    public boolean getCurrentUserHasPermission(String permission) {

        if (securityService.unlock(getCurrentUserId(), permission, getCurrentLocationId())) {
            return true;
        }
        return false;
    }

    public String cleanupUserStrings(String userSubmittedString) {
        // clean up the string
        return FormattedText.processFormattedText(userSubmittedString, new StringBuilder(), true, false);            
    }
    
    public String getAssignmentViewUrl(String viewId) {
    	return ServerConfigurationService.getToolUrl() + Entity.SEPARATOR
    	+ toolManager.getCurrentPlacement().getId() + Entity.SEPARATOR + viewId;
    }
    
    public Collection getSiteGroups() {
    	try {
	    	Site s = siteService.getSite(toolManager.getCurrentPlacement().getContext());
	    	return s.getGroups();
    	} catch (IdUnusedException e){
    		return new ArrayList();
    	}
    }

}
