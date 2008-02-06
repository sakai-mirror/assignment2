/******************************************************************************
 * EvalExternalLogicStub.java - created by aaronz@vt.edu on Dec 25, 2006
 * 
 * Copyright (c) 2007 Virginia Polytechnic Institute and State University
 * Licensed under the Educational Community License version 1.0
 * 
 * A copy of the Educational Community License has been included in this 
 * distribution and is available at: http://www.opensource.org/licenses/ecl1.php
 * 
 * Contributors:
 * Aaron Zeckoski (aaronz@vt.edu) - primary
 * 
 *****************************************************************************/

package org.sakaiproject.assignment2.logic.test.stubs;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.sakaiproject.content.api.ContentResource;

import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.test.AssignmentTestDataLoad;

import org.sakaiproject.section.api.SectionAwareness;
import org.sakaiproject.section.api.coursemanagement.CourseSection;
import org.sakaiproject.section.api.coursemanagement.ParticipationRecord;
import org.sakaiproject.tool.gradebook.facades.Authn;
import org.sakaiproject.section.api.facade.Role;



/**
 * This is a stub class for testing purposes, it will allow us to test all the classes
 * that depend on it since it has way to many external dependencies to make it worth
 * it to mock them all up<br/>
 *
 * Methods only used by the ui are not implemented
 */
public class ExternalLogicStub implements ExternalLogic {

	private Authn authn;
	private SectionAwareness sectionAwareness;
	
	public void setAuthn(Authn authn) {
		this.authn = authn;
	}
	public void setSectionAwareness(SectionAwareness sectionAwareness) {
		this.sectionAwareness = sectionAwareness;
	}
	/**
     * @return the current sakai user id (not username)
     */
    public String getCurrentUserId() {
    	return authn.getUserUid();
    }

    /**
     * Get the display name for a user by their unique id
     * 
     * @param userId
     *            the current sakai user id (not username)
     * @return display name (probably firstname lastname) or "----------" (10 hyphens) if none found
     */
    public String getUserDisplayName(String userId) {
    	return "Artichoke, Arty";
    }

    /**
     * @return the current location id of the current user
     */
    public String getCurrentLocationId() {
    	return AssignmentTestDataLoad.CONTEXT_ID;
    }
    
    /**
     * 
     * @return the current context for the current user
     */
    public String getCurrentContextId() {
    	return AssignmentTestDataLoad.CONTEXT_ID;
    }

    /**
     * Check if this user has super admin access
     * 
     * @param userId
     *            the internal user id (not username)
     * @return true if the user has admin access, false otherwise
     */
    public boolean isUserAdmin(String userId) {
    	return false; 
    }

    /**
     * Cleans up the users submitted strings to protect us from XSS
     * 
     * @param userSubmittedString any string from the user which could be dangerous
     * @return a cleaned up string which is now safe
     */
    public String cleanupUserStrings(String userSubmittedString) {
    	return null; // used for ui
    }
    
    /**
     * Returns URL to viewId pass in
     * @param viewId of view to build path to
     * @return a url path to the vie
     */
    public String getAssignmentViewUrl(String viewId) {
    	return null; // used for ui
    }

    /**
     * Return a Collection of all Groups
     * @return a collection
     */
    public Collection getSiteGroups() {
    	return null; // used for ui
    }
    
    /**
     * 
     * @return a collection of the groups that the given user is a member of
     */
    public Collection getUserMemberships(String userId) {
    	return null; // used for ui
    }
    
    /**
     * 
     * @return list of the group ids of the groups that the given user is
     * a member of
     */
    public List<String> getUserMembershipGroupIdList(String userId) {
    	List groupIdList = new ArrayList();
    	List sectionList = sectionAwareness.getSections(AssignmentTestDataLoad.CONTEXT_ID);
    	if (sectionList != null && !sectionList.isEmpty()) {
    		for (Iterator sIter = sectionList.iterator(); sIter.hasNext();) {
    			CourseSection section = (CourseSection) sIter.next();
    			if (section != null) {
    				List sectionMembers = sectionAwareness.getSectionMembers(section.getUuid());
    				if (sectionAwareness.isSectionMemberInRole(section.getUuid(), userId, Role.STUDENT) || 
    						sectionAwareness.isSectionMemberInRole(section.getUuid(), userId, Role.TA)) {
    					groupIdList.add(section.getUuid());
    				}
    			}
    		}
    		
    	}
    	
    	return groupIdList;
    }
    
    /**
     * 
     * @return a map of group id to group name for all of the sections/groups
     * associated with the current site
     */
    public Map<String, String> getGroupIdToNameMapForSite() {
    	return null; //used for ui
    }
    
    /**
     * @param contextId
     * @param toolId
     * @return true if tool with the given toolId exists in the site with the given siteId
     */
    public boolean siteHasTool(String contextId, String toolId) {
    	return false; //used for ui
    }

    /**
     * 
     * @param contentReference
     * @return String of path for <img> tag for resource image type icon
     */
    public String getContentTypeImagePath(ContentResource contentReference) {
    	return null; //used for ui
    }
    
    /**
     * 
     * @param contextId
     * @return a list of the student Ids of all of the students in the given site
     */
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
    
    /**
     * 
     * @param sectionId
     * @return a list of the student ids of students in the given section  
     */
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
    
    /**
     * 
     * @param gradeableObjectId
     * @param returnViewId
     * @return url to helper
     */
    public String getUrlForGradebookItemHelper(Long gradeableObjectId, String returnViewId) {
    	return null; //used for ui
    }
    
    /**
     * 
     * @param gradeableObjectId
     * @param userId
     * @param returnViewId
     * @return url to helper
     */
    public String getUrlForGradeGradebookItemHelper(Long gradeableObjectId, String userId, String returnViewId) {
    	return null; //used for ui
    }

}
