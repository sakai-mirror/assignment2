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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.PermissionLogic;
import org.sakaiproject.assignment2.logic.GradebookItem;
import org.sakaiproject.component.cover.ComponentManager;
import org.sakaiproject.component.cover.ServerConfigurationService;
import org.sakaiproject.exception.IdUnusedException;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.site.api.Site;
import org.sakaiproject.site.api.ToolConfiguration;
import org.sakaiproject.site.cover.SiteService;


/**
 * This is the implementation for logic to answer common permissions questions
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class PermissionLogicImpl implements PermissionLogic {

    private static Log log = LogFactory.getLog(PermissionLogicImpl.class);

    public void init() {
    	log.debug("init");
    }
    
	private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }
    
    private ExternalGradebookLogic gradebookLogic;
    public void setExternalGradebookLogic(ExternalGradebookLogic gradebookLogic) {
        this.gradebookLogic = gradebookLogic;
    }
    
    public boolean isCurrentUserAbleToEditAssignments(String contextId) {

    	return gradebookLogic.isCurrentUserAbleToEdit(contextId);
    }
    
    public boolean isUserAbleToViewStudentSubmissionForAssignment(String studentId, Assignment2 assignment) {
    	if (studentId == null || assignment == null) {
    		throw new IllegalArgumentException("Null studentId or assignment passed to isUserAbleToViewStudentSubmissionForAssignment");
    	}
    	
    	boolean viewable = true;
    	
    	if (externalLogic.getCurrentUserId().equals(studentId)) {
    		viewable = true;
    	} else if (gradebookLogic.isCurrentUserAbleToGradeAll(assignment.getContextId())) {
    		viewable = true;
    	} else {
    		if (assignment.isUngraded()) {
    			viewable = isUserAbleToViewSubmissionForUngradedAssignment(studentId, assignment);
    		} else {
    			
    		}
    	}
    	
    	return viewable;
    	
    }
    
    public boolean isUserAbleToViewSubmissionForUngradedAssignment(String studentId, Assignment2 assignment) {
    	if (studentId == null || assignment == null) {
    		throw new IllegalArgumentException("null studentId or assignment passed to isUserAbleToViewSubmissionForUngradedAssignment");
    	}
    	
    	boolean viewable = false;
    	if (gradebookLogic.isCurrentUserAbleToGrade(assignment.getContextId())) {
    		List currentUserMemberships = externalLogic.getUserMembershipGroupIdList(externalLogic.getCurrentUserId());
    		List studentMemberships = externalLogic.getUserMembershipGroupIdList(studentId);
    		if (userMembershipsOverlap(currentUserMemberships, studentMemberships)) {
    			viewable = true;
    		}
    	}
    	
    	return viewable;
    }
    
    public boolean isUserAbleToViewUngradedAssignment(String userId, Assignment2 assignment) {
    	if (assignment == null || userId == null) {
    		throw new IllegalArgumentException("Null assignment or userId passed to isUserAbleToViewUngradedAssignment");
    	}
    	
    	boolean viewable = false;
    	
    	if (gradebookLogic.isCurrentUserAbleToGradeAll(assignment.getContextId())) {
    		viewable = true;
    	} else if (assignment.getAssignmentGroupSet() == null || assignment.getAssignmentGroupSet().isEmpty()) {
    		viewable = true;
    	} else {
    		// the user must be a member of a restricted group to view assignment
        	viewable = isUserAMemberOfARestrictedGroup(userId, assignment.getAssignmentGroupSet());
    	}
    	
    	return viewable;
    }
    
    public boolean isUserAMemberOfARestrictedGroup(String userId, Set<AssignmentGroup> assignmentGroupSet) {
    	if (userId == null) {
    		throw new IllegalArgumentException("null userId passed to userId");
    	}
    	
    	if (assignmentGroupSet != null) {
    		List<String> groupIdList = externalLogic.getUserMembershipGroupIdList(externalLogic.getCurrentUserId());
        	if (groupIdList != null) {
	        	for (Iterator aGroupIter = assignmentGroupSet.iterator(); aGroupIter.hasNext();) {
	        		AssignmentGroup aGroup = (AssignmentGroup) aGroupIter.next();
	        		if (aGroup != null) {
	        			if (groupIdList.contains(aGroup.getGroupId())) {
	        				return true;
	        			}
	        		}
	        	}
        	}
    	}
    	
    	return false;
    }
    
	public boolean isUserAbleToAccessInstructorView(String contextId) {
		if (contextId == null) {
			throw new IllegalArgumentException("null contextId passed to isUserAbleToAccessInstructorView");
		}
		
		boolean instructorView = false;
		
		if (gradebookLogic.isCurrentUserAbleToEdit(contextId) ||
				gradebookLogic.isCurrentUserAbleToGrade(contextId)) {
			instructorView = true;
		}
		
		return instructorView;
	}
	
	/**
	 * Given 2 lists of group ids, will return true if any of the group ids overlap
	 * @param user1GroupIds
	 * @param user2GroupIds
	 * @return
	 */
	private boolean userMembershipsOverlap(List<String> user1GroupIds, List<String> user2GroupIds) {
		if (user1GroupIds != null && user2GroupIds != null ) {
			for (Iterator user1Iter = user1GroupIds.iterator(); user1Iter.hasNext();) {
				String user1Group = (String) user1Iter.next();
				if (user1Group != null && user2GroupIds.contains(user1Group)) {
					return true;
				}
			}
		}
		
		return false;
	}

}
