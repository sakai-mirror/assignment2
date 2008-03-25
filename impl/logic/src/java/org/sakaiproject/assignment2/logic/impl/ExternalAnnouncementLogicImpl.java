/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/api/logic/src/java/org/sakaiproject/assignment2/logic/ExternalGradebookLogicImpl.java $
 * $Id: ExternalGradebookLogic.java 12544 2006-05-03 15:06:26Z wagnermr@iupui.edu $
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
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.ExternalAnnouncementLogic;
import org.sakaiproject.component.cover.ServerConfigurationService;
import org.sakaiproject.entity.cover.EntityManager;
import org.sakaiproject.event.cover.NotificationService;
import org.sakaiproject.exception.IdUnusedException;
import org.sakaiproject.exception.InUseException;
import org.sakaiproject.exception.PermissionException;
import org.sakaiproject.site.api.Site;
import org.sakaiproject.site.api.Group;
import org.sakaiproject.site.cover.SiteService;
import org.sakaiproject.announcement.api.AnnouncementChannel;
import org.sakaiproject.announcement.api.AnnouncementMessageEdit;
import org.sakaiproject.announcement.api.AnnouncementMessageHeaderEdit;
import org.sakaiproject.announcement.api.AnnouncementService;
import org.sakaiproject.assignment2.exception.AnnouncementPermissionException;

/**
 * This is the implementation for logic to interact with the Sakai
 * Announcements tool
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class ExternalAnnouncementLogicImpl implements ExternalAnnouncementLogic {

    private static Log log = LogFactory.getLog(ExternalAnnouncementLogic.class);
    
    private AnnouncementService aService;
    private AnnouncementChannel announcementChannel;

    public void init() {
    	if (log.isDebugEnabled()) log.debug("init");
    }
    
    public String addOpenDateAnnouncement(Collection<String> restrictedGroupIds, String contextId,
    		String announcementSubject, String announcementBody) throws AnnouncementPermissionException {
    	if (contextId == null) {
    		throw new IllegalArgumentException("null contextId passed to addOpenDateAnnouncement");
    	}
    	
    	initializeAnnouncementServiceData(contextId);
    	if (announcementChannel == null) {
    		log.warn("announcementChannel was null when trying to add announcement so no annc added");
    		return null;
    	}
    	
    	try
		{
			AnnouncementMessageEdit message = announcementChannel.addAnnouncementMessage();
			AnnouncementMessageHeaderEdit header = message.getAnnouncementHeaderEdit();
			header.setDraft(false);
			header.replaceAttachments(EntityManager.newReferenceList());

			header.setSubject(announcementSubject);
			message.setBody(announcementBody);
				
			if (restrictedGroupIds == null || restrictedGroupIds.isEmpty()) {
				//site announcement
				header.clearGroupAccess();
			} else {
				addGroupRestrictions(restrictedGroupIds, contextId, header);
			}

			announcementChannel.commitMessage(message, NotificationService.NOTI_NONE);
			if (log.isDebugEnabled()) log.debug("announcement added with id: " + message.getId());
			
			return message.getId();	
			
		} catch (PermissionException pe) {
			throw new AnnouncementPermissionException("The current user does not have permission to access add announcement");
		}
	}
    
	public String updateOpenDateAnnouncement(String announcementId, Collection<String> restrictedGroupIds, String contextId, 
			String announcementSubject, String announcementBody) throws AnnouncementPermissionException {
		if (contextId == null) {
    		throw new IllegalArgumentException("null contextId passed to updateOpenDateAnnouncement");
    	}
		
    	if (announcementId == null) {
    		log.warn("there was no announcementId passed to update");
    		return null;
    	}
    	
    	initializeAnnouncementServiceData(contextId);
    	if (announcementChannel == null) {
    		log.warn("announcementChannel was null when trying to add announcement so no annc added");
    		return null;
    	}

    	try
		{
			AnnouncementMessageEdit message = announcementChannel.editAnnouncementMessage(announcementId);
			AnnouncementMessageHeaderEdit header = message.getAnnouncementHeaderEdit();
			header.setDraft(false);
			header.replaceAttachments(EntityManager.newReferenceList());
			header.setSubject(announcementSubject);
			message.setBody(announcementBody);
				
			if (restrictedGroupIds == null || restrictedGroupIds.isEmpty()) {
				//site announcement
				header.clearGroupAccess();
			} else {
				addGroupRestrictions(restrictedGroupIds, contextId, header);
			}

			announcementChannel.commitMessage(message, NotificationService.NOTI_NONE);
			if (log.isDebugEnabled()) log.debug("Announcement updated with id: " + announcementId);
			
			return message.getId();	
			
		} catch (PermissionException pe) {
			throw new AnnouncementPermissionException("The current user does not have permission to access add announcement");
		} catch (IdUnusedException iue) {
			// the announcement id stored in the assignment is invalid, so add a new announcement
			if (log.isDebugEnabled()) log.debug("Bad announcementId associated with assignment, so adding new announcement");
			return addOpenDateAnnouncement(restrictedGroupIds, contextId, announcementSubject, announcementBody);
		} catch (InUseException iue) {
			log.error("Announcement " + announcementId + " is locked and cannot be" +
					"updated");
			return null;
		}
	}
	
	public void deleteOpenDateAnnouncement(String announcementId, String contextId) {
		if (contextId == null) {
    		throw new IllegalArgumentException("null announcementId or contextId passed to addOpenDateAnnouncement");
    	}
    	
    	if (announcementId == null) {
    		log.warn("there was no announcementId passed, so announcement was not deleted");
    		return;
    	}
    	
    	initializeAnnouncementServiceData(contextId);
    	if (announcementChannel == null) {
    		log.warn("announcementChannel was null when trying to delete announcement so no annc deleted");
    		return;
    	}

    	try
		{
			announcementChannel.removeMessage(announcementId);
			if (log.isDebugEnabled()) log.debug("Announcement removed with id: " + announcementId);
			
		} catch (PermissionException pe) {
			throw new AnnouncementPermissionException("The current user does not have permission to access add announcement");
		} catch (Exception e) {
			if (log.isDebugEnabled()) log.debug("Announcement no longer exists so cannot be deleted. It was probably deleted via annc tool.");
			// this is thrown by removeMessage if the annc doesn't exist
		}
	}
    
	private void initializeAnnouncementServiceData(String contextId) {
		aService = org.sakaiproject.announcement.cover.AnnouncementService.getInstance();
    	
    	String channelId = ServerConfigurationService.getString("channel", null);
    	if (channelId == null)
		{
			channelId = aService.channelReference(contextId, SiteService.MAIN_CONTAINER);
			try
			{
				announcementChannel = aService.getAnnouncementChannel(channelId);
			}
			catch (IdUnusedException e)
			{
				log.warn("No announcement channel found");
				announcementChannel = null;
			}
			catch (PermissionException e)
			{
				throw new AnnouncementPermissionException("The current user does not have permission to access the announcement channel");
			}
		}
	}
	
	private void addGroupRestrictions(Collection<String> restrictedGroupIds, String contextId, AnnouncementMessageHeaderEdit header) {
		try
		{
			if (restrictedGroupIds != null && !restrictedGroupIds.isEmpty()) {

				//make a collection of Group objects from the collection of group ref strings
				Site site = SiteService.getSite(contextId);
				List<Group> groupRestrictions = new ArrayList<Group>();
				for (String groupId : restrictedGroupIds)
				{
					if (groupId != null) {
						Group thisGroup = site.getGroup(groupId);
						if (thisGroup != null) {
							groupRestrictions.add(thisGroup);
						}
					}
				}

				// set access
				header.setGroupAccess(groupRestrictions);
			}
		}
		catch (Exception exception)
		{
			log.warn("There was an error adding the group restrictions to the announcement");
		}
	}
		
}