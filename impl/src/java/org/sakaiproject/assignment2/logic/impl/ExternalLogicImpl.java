/**********************************************************************************
 * $URL$
 * $Id$
 ***********************************************************************************
 *
 * Copyright (c) 2007 The Sakai Foundation.
 * 
 * Licensed under the Educational Community License, Version 1.0 (the "License"); 
 * you may not use this file except in compliance with the License. 
 * You may obtain a copy of the License at
 * 
 *      http://www.opensource.org/licenses/ecl1.php
 * 
 * Unless required by applicable law or agreed to in writing, software 
 * distributed under the License is distributed on an "AS IS" BASIS, 
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 *
 **********************************************************************************/
package org.sakaiproject.assignment2.logic.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.authz.api.SecurityService;
import org.sakaiproject.component.cover.ServerConfigurationService;
import org.sakaiproject.content.api.ContentResource;
import org.sakaiproject.content.api.ContentTypeImageService;
import org.sakaiproject.entity.api.Entity;
import org.sakaiproject.exception.IdUnusedException;
import org.sakaiproject.section.api.SectionAwareness;
import org.sakaiproject.section.api.coursemanagement.ParticipationRecord;
import org.sakaiproject.section.api.facade.Role;
import org.sakaiproject.site.api.Group;
import org.sakaiproject.site.api.Site;
import org.sakaiproject.site.api.SiteService;
import org.sakaiproject.tool.api.SessionManager;
import org.sakaiproject.tool.api.ToolManager;
import org.sakaiproject.user.api.User;
import org.sakaiproject.user.api.UserDirectoryService;
import org.sakaiproject.user.api.UserNotDefinedException;
import org.sakaiproject.util.FormattedText;

/**
 * This is the implementation for logic which is external to our app logic
 */
public class ExternalLogicImpl implements ExternalLogic {

    private static Log log = LogFactory.getLog(ExternalLogicImpl.class);

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
    
    private static final String BASE_IMG_PATH= "/library/image/";

    /**
     * Place any code that should run when this class is initialized by spring here
     */
    public void init() {
    	if (log.isDebugEnabled()) log.debug("init");
    }

    public String getCurrentLocationId() {
        try {
            Site s = siteService.getSite(getCurrentContextId());
            return s.getReference(); // get the entity reference to the site
        } catch (IdUnusedException e) {
            return NO_LOCATION;
        }
    }
    
    public String getCurrentContextId() {
    	if (toolManager != null && toolManager.getCurrentPlacement() != null && toolManager.getCurrentPlacement().getContext() != null){
    		return toolManager.getCurrentPlacement().getContext();
    		
    	} else {
    		return null;
    	}
    }
    
    public String getToolTitle() {
    	return toolManager.getTool(ExternalLogic.TOOL_ID_ASSIGNMENT2).getTitle();
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
    
    public Collection<Group> getSiteGroups(String contextId) {
    	try {
	    	Site s = siteService.getSite(contextId);
	    	return s.getGroups();
    	} catch (IdUnusedException e){
    		return new ArrayList<Group>();
    	}
    }
    
    public Collection<Group> getUserMemberships(String userId, String contextId) {
    	if (userId == null || contextId == null) {
    		throw new IllegalArgumentException("Null userId or contextId passed to getUserMemberships");
    	}
    	try {
	    	Site s = siteService.getSite(toolManager.getCurrentPlacement().getContext());
	    	return s.getGroupsWithMember(userId);
    	} catch (IdUnusedException e){
    		return new ArrayList<Group>();
    	}
    }
    
    public List<String> getUserMembershipGroupIdList(String userId, String contextId) {
    	if (userId == null || contextId == null) {
    		throw new IllegalArgumentException("Null userId or contextId passed to getUserMembershipGroupIdList");
    	}
    	List<Group> memberships = new ArrayList<Group>(getUserMemberships(userId, contextId));
    	List<String> groupIds = new ArrayList<String>();
    	if (memberships != null) {
    		for (Group group : memberships) {
    			if (group != null) {
    				groupIds.add(group.getId());
    			}
    		}
    	}
    	
    	return groupIds;
    }
    
    public Map<String, String> getGroupIdToNameMapForSite(String contextId) {
    	if (contextId == null) {
    		throw new IllegalArgumentException("Null contextId passed to getGroupIdToNameMapForSite");
    	}
    	
    	Collection<Group> siteGroups = getSiteGroups(contextId);
    	
    	Map<String, String> groupIdToNameMap = new HashMap<String, String>();
    	if (siteGroups != null && !siteGroups.isEmpty()) {
			for (Group siteGroup : siteGroups) {
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
    	List<String> studentsInSite = new ArrayList<String>();
    	
    	List<ParticipationRecord> participants = sectionAwareness.getSiteMembersInRole(contextId, Role.STUDENT);
    	if (participants != null) {
    		for (ParticipationRecord part : participants) {
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
    	
    	List<String> studentsInSection = new ArrayList<String>();
    	
    	List<ParticipationRecord> participants = sectionAwareness.getSectionMembersInRole(sectionId, Role.STUDENT);
    	for (ParticipationRecord part : participants) {
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

	public User getUser(String userId)
	{
		try
		{
			User user = userDirectoryService.getUser(userId);
			return user;
		} catch (UserNotDefinedException ex) {
			log.error("Could not get user from userId: " + userId, ex);
		}

		return userDirectoryService.getAnonymousUser();
	}
	
	public String getReadableFileSize(int sizeVal)
	{
		double retVal = sizeVal;
		String sizeSuffix = "bytes";
		int GB = 1024 * 1024 * 1024;
		int MB = 1024 * 1024;
		int KB = 1024;
		if (sizeVal > GB) {
		retVal = sizeVal / GB;
		sizeSuffix = "GB";
		}
		else if(sizeVal > MB) {
		retVal = sizeVal / MB;
		sizeSuffix = "MB";
		}
		else if (sizeVal > KB) {
		retVal = sizeVal / KB;
		sizeSuffix = "KB";
		}
		String finalVal = "(".concat(Double.toString(retVal).concat(" " + sizeSuffix.concat(")")));
		return finalVal;

	}
	
	public Map<String, User> getUserIdUserMap(List<String> userIds) {
		Map<String, User> userIdUserMap = new HashMap<String, User>();
		if (userIds != null) {
			List<User> userList = new ArrayList<User>();
			userList = userDirectoryService.getUsers(userIds);
			
			if (userList != null) {
				for (User user : userList) {
					userIdUserMap.put(user.getId(), user);
				}
			}
		}
		
		return userIdUserMap;
	}
	
    public Map<String, String> getUserDisplayIdUserIdMapForStudentsInSite(String contextId) {
    	if (contextId == null) {
    		throw new IllegalArgumentException("Null contextId passed to getUserDisplayIdUserIdMapForStudentsInSite");
    	}
    	
    	Map<String, String> userDisplayIdUserIdMap = new HashMap<String, String>();

    	List<String> allStudentsInSite = getStudentsInSite(contextId);
    	
    	if (allStudentsInSite != null) {
			List<User> userList = new ArrayList<User>();
			userList = userDirectoryService.getUsers(allStudentsInSite);
			
			if (userList != null) {
				for (User user : userList) {
					userDisplayIdUserIdMap.put(user.getDisplayId(), user.getId());
				}
			}
		}
    	
    	return userDisplayIdUserIdMap;
    }
}