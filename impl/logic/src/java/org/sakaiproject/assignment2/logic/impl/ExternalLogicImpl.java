package org.sakaiproject.assignment2.logic.impl;

import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Iterator;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.authz.api.FunctionManager;
import org.sakaiproject.authz.api.SecurityService;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.component.cover.ServerConfigurationService;
import org.sakaiproject.content.api.ContentResource;
import org.sakaiproject.content.api.ContentTypeImageService;
import org.sakaiproject.entity.api.Entity;
import org.sakaiproject.exception.IdUnusedException;
import org.sakaiproject.site.api.Group;
import org.sakaiproject.site.api.Site;
import org.sakaiproject.site.api.SiteService;
import org.sakaiproject.tool.api.SessionManager;
import org.sakaiproject.tool.api.ToolManager;
import org.sakaiproject.user.api.User;
import org.sakaiproject.user.api.UserDirectoryService;
import org.sakaiproject.user.api.UserNotDefinedException;
import org.sakaiproject.util.FormattedText;
import org.sakaiproject.section.api.SectionAwareness;
import org.sakaiproject.section.api.facade.Role;
import org.sakaiproject.section.api.coursemanagement.ParticipationRecord;

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
    
    private SectionAwareness sectionAwareness;
    public void setSectionAwareness(SectionAwareness sectionAwareness) {
    	this.sectionAwareness = sectionAwareness;
    }
    
    private static final String ANON_USER_ATTRIBUTE = "AnonUserAttribute";
    private static final String BASE_IMG_PATH= "/library/image/";

    /**
     * Place any code that should run when this class is initialized by spring here
     */
    public void init() {
    	if (log.isDebugEnabled()) log.debug("init");
    }

    public String getCurrentLocationId() {
        try {
            Site s = siteService.getSite(toolManager.getCurrentPlacement().getContext());
            return s.getReference(); // get the entity reference to the site
        } catch (IdUnusedException e) {
            return NO_LOCATION;
        }
    }
    
    public String getCurrentContextId() {
    	return toolManager.getCurrentPlacement().getContext();
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
    
    public Collection getUserMemberships(String userId) {
    	if (userId == null) {
    		throw new IllegalArgumentException("Null userId passed to getUserMemberships");
    	}
    	try {
	    	Site s = siteService.getSite(toolManager.getCurrentPlacement().getContext());
	    	return s.getGroupsWithMember(userId);
    	} catch (IdUnusedException e){
    		return new ArrayList();
    	}
    }
    
    public List<String> getUserMembershipGroupIdList(String userId) {
    	if (userId == null) {
    		throw new IllegalArgumentException("Null userId passed to getUserMembershipGroupIdList");
    	}
    	List memberships = new ArrayList(getUserMemberships(userId));
    	List groupIds = new ArrayList();
    	if (memberships != null) {
    		for (Iterator groupIter = memberships.iterator(); groupIter.hasNext();) {
    			Group group = (Group) groupIter.next();
    			if (group != null) {
    				groupIds.add(group.getId());
    			}
    		}
    	}
    	
    	return groupIds;
    }
    
    public Map<String, String> getGroupIdToNameMapForSite() {
    	Collection siteGroups = getSiteGroups();
    	
    	Map groupIdToNameMap = new HashMap();
    	if (siteGroups != null && !siteGroups.isEmpty()) {
			for (Iterator siteGroupIter = siteGroups.iterator(); siteGroupIter.hasNext();) {
				Group siteGroup = (Group) siteGroupIter.next();
				if (siteGroup != null) {
					groupIdToNameMap.put(siteGroup.getId(), siteGroup.getTitle());
				}
			}
			
		}
    	
    	return groupIdToNameMap;
    }
    
    public boolean siteHasTool(String contextId, String toolId) {
    	try {
    		Site currSite = siteService.getSite(contextId);
    		if (currSite.getToolForCommonId(toolId) != null) {
    			return true;
    		}
    	} catch (IdUnusedException ide) {
    		if (log.isDebugEnabled()) log.debug("IdUnusedException caught in siteHasTool with contextId: " + contextId + " and toolId: " + toolId);
    	}
		return false;
    }
    
    public String getContentTypeImagePath(ContentResource contentReference) {
    	String image_path = BASE_IMG_PATH;
    	ContentTypeImageService imageService = org.sakaiproject.content.cover.ContentTypeImageService.getInstance();
    	image_path += imageService.getContentTypeImage(
    			contentReference.getProperties().getProperty(
    					contentReference.getProperties().getNamePropContentType()));
    	return image_path;
    }
    
    public List<String> getStudentsInSite(String contextId) {
    	if (contextId == null) {
    		throw new IllegalArgumentException("Null contextId passed to getStudentsInSite");
    	}
    	List<String> studentsInSite = new ArrayList();
    	
    	List<ParticipationRecord> participants = sectionAwareness.getSiteMembersInRole(contextId, Role.STUDENT);
    	if (participants != null) {
    		for (Iterator pIter = participants.iterator(); pIter.hasNext();) {
    			ParticipationRecord part = (ParticipationRecord) pIter.next();
    			if (part != null) {
    				String studentId = part.getUser().getUserUid();
    				studentsInSite.add(studentId);
    			}
    		}
    	}
    	
    	return studentsInSite;
    }
    
    public List<String> getStudentsInSection(String sectionId) {
    	if (sectionId == null) {
    		throw new IllegalArgumentException("null sectionId passed to getStudentsInSection");
    		
    	}
    	
    	List<String> studentsInSection = new ArrayList();
    	
    	List<ParticipationRecord> participants = sectionAwareness.getSectionMembersInRole(sectionId, Role.STUDENT);
    	for (Iterator pIter = participants.iterator(); pIter.hasNext();) {
			ParticipationRecord part = (ParticipationRecord) pIter.next();
			if (part != null) {
				String studentId = part.getUser().getUserUid();
				studentsInSection.add(studentId);
			}
		}
    	
    	return studentsInSection;
    }
    
    public String getUrlForGradebookItemHelper(Long gradeableObjectId, String returnViewId) {
    	//TODO URL encode this so I can put it as a url parameter
    	 String url = "/direct/gradebook/_/gradebookItem/" + getCurrentContextId();
	     String finishedURL = getAssignmentViewUrl(returnViewId);
	     String getParams = "?TB_iframe=true&width=700&height=350&KeepThis=true&finishURL=" + finishedURL;
	      
	     return url + "/" + (gradeableObjectId != null ? gradeableObjectId : "") + getParams;
    }
    
    public String getUrlForGradeGradebookItemHelper(Long gradeableObjectId, String userId, String returnViewId) {
    	String url = "/direct/gradebook/_/gradeGradebookItem/" + getCurrentContextId() +
    	"/" + gradeableObjectId + "/" + userId; 
    	String finishedURL = getAssignmentViewUrl(returnViewId);
    	String getParams = "?TB_iframe=true&width=700&height=380&KeepThis=true&finishURL=" + finishedURL;
    
    	return url + getParams;
    }

	public String getUserFullName(String userId) {
        try {
            User user = userDirectoryService.getUser(userId);
            return user.getLastName() + ", " + user.getFirstName();
        } catch (UserNotDefinedException ex) {
            log.error("Could not get user from userId: " + userId, ex);
        }

        return ", ";
    }
}
