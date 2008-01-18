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
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Collection;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.dao.AssignmentDao;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;

/**
 * This is the implementation for logic to answer common permissions questions
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class AssignmentPermissionLogicImpl implements AssignmentPermissionLogic {

    private static Log log = LogFactory.getLog(AssignmentPermissionLogicImpl.class);

    public void init() {
    	log.debug("init");
    }
    
    private AssignmentDao dao;
    public void setDao(AssignmentDao dao) {
        this.dao = dao;
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
    	
    	boolean viewable = false;
    	
    	if (externalLogic.getCurrentUserId().equals(studentId)) {
    		viewable = true;
    	} else if (gradebookLogic.isCurrentUserAbleToGradeAll(assignment.getContextId())) {
    		viewable = true;
    	} else {
    		if (assignment.isUngraded()) {
    			viewable = isUserAbleToViewSubmissionForUngradedAssignment(studentId, assignment);
    		} else {
    			Long gbItemId = assignment.getGradableObjectId();
    			if (gbItemId != null) {
    				String function = gradebookLogic.getGradeViewPermissionForCurrentUserForStudentForItem(externalLogic.getCurrentContextId(), 
    						studentId, gbItemId);
    				if (function != null && (function.equals(AssignmentConstants.GRADE) ||
    						function.equals(AssignmentConstants.VIEW))) {
    					viewable = true;
    				}
    			}
    		}
    	}
    	
    	return viewable;	
    }
    
    public boolean isUserAbleToProvideFeedbackForSubmission(AssignmentSubmission submission) {
    	if (submission == null) {
    		throw new IllegalArgumentException("null submission passed to isUserAbleToProvideFeedbackForSubmission");
    	}
    	
    	boolean allowed = false;
    	
    	Assignment2 assignment = submission.getAssignment();
    	if (assignment != null) {
    		if (assignment.isUngraded()) {
    			allowed = isUserAbleToViewSubmissionForUngradedAssignment(submission.getUserId(), assignment);
    		} else {
    			if (assignment.getGradableObjectId() != null) {
    				allowed = gradebookLogic.isCurrentUserAbleToGradeStudentForItem(externalLogic.getCurrentContextId(), 
    					submission.getUserId(), assignment.getGradableObjectId());
    			}
    		}
    	}
    	
    	return allowed;
    }
    
    public boolean isUserAbleToViewSubmissionForUngradedAssignment(String studentId, Assignment2 assignment) {
    	if (studentId == null || assignment == null) {
    		throw new IllegalArgumentException("null studentId or assignment passed to isUserAbleToViewSubmissionForUngradedAssignment");
    	}
    	
    	boolean viewable = false;
    	if (gradebookLogic.isCurrentUserAbleToGradeAll(assignment.getContextId())) {
    		viewable = true;
    	} else if (gradebookLogic.isCurrentUserAbleToGrade(assignment.getContextId())) {
    		List currentUserMemberships = externalLogic.getUserMembershipGroupIdList(externalLogic.getCurrentUserId());
    		List studentMemberships = externalLogic.getUserMembershipGroupIdList(studentId);
    		if (userMembershipsOverlap(currentUserMemberships, studentMemberships)) {
    			viewable = true;
    		}
    	}
    	
    	return viewable;
    }
    
    public boolean isUserAbleToViewUngradedAssignment(Assignment2 assignment, Collection<String> groupMembershipIds) {
    	if (assignment == null) {
    		throw new IllegalArgumentException("Null assignment passed to isUserAbleToViewUngradedAssignment");
    	}
    	
    	boolean viewable = false;
    	
    	if (gradebookLogic.isCurrentUserAbleToGradeAll(assignment.getContextId())) {
    		viewable = true;
    	} else if (assignment.getAssignmentGroupSet() == null || assignment.getAssignmentGroupSet().isEmpty()) {
    		viewable = true;
    	} else {
    		// the user must be a member of a restricted group to view assignment
        	viewable = isUserAMemberOfARestrictedGroup(groupMembershipIds, assignment.getAssignmentGroupSet());
    	}
    	
    	return viewable;
    }
    
    public boolean isUserAMemberOfARestrictedGroup(Collection<String> groupMembershipIds, Collection<AssignmentGroup> assignmentGroupSet) {    	
    	if (assignmentGroupSet != null && !assignmentGroupSet.isEmpty()) {
        	if (groupMembershipIds != null) {
	        	for (Iterator aGroupIter = assignmentGroupSet.iterator(); aGroupIter.hasNext();) {
	        		AssignmentGroup aGroup = (AssignmentGroup) aGroupIter.next();
	        		if (aGroup != null) {
	        			if (groupMembershipIds.contains(aGroup.getGroupId())) {
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
	
	public boolean isUserAbleToMakeSubmissionForAssignment(String contextId, Assignment2 assignment) {
		if (contextId == null || assignment == null) {
			throw new IllegalArgumentException("null contextId or assignment passed to isUserAbleToMakeSubmission");
		}
		
		boolean userAbleToSubmit = false;
	
		if (assignment.isUngraded()) {
			// TODO right now, we don't have any checks for ungraded assignments...
			userAbleToSubmit = true;
		} else {
			// we must obey the gradebook permissions
			if (gradebookLogic.isCurrentUserAStudentInGb(contextId)) {
				userAbleToSubmit = true;
			}
		}
		
		// check to make sure any group restrictions have been upheld
		if (userAbleToSubmit) {
			List<AssignmentGroup> assignGroupRestrictions = 
				dao.findByProperties(AssignmentGroup.class, new String[] {"assignment"}, new Object[] {assignment});
			
			List<String> groupMembershipIds = externalLogic.getUserMembershipGroupIdList(externalLogic.getCurrentUserId());
			if (assignGroupRestrictions != null && !assignGroupRestrictions.isEmpty()) {
				if (!isUserAMemberOfARestrictedGroup(groupMembershipIds, assignGroupRestrictions)) {
					userAbleToSubmit = false;
				}
			}
		}
		
		return userAbleToSubmit;
	}
	
	public List<String> getViewableStudentsForUserForItem(Assignment2 assignment) {
		if (assignment == null) {
			throw new IllegalArgumentException("null assignment passed to getViewableStudentsForUserForItem");
		}
		
		return getAvailableStudentsForUserForItem(assignment, AssignmentConstants.VIEW);
	}
	
	public List<String> getGradableStudentsForUserForItem(Assignment2 assignment) {
		if (assignment == null) {
			throw new IllegalArgumentException("null assignment passed to getGradableStudentsForUserForItem");
		}
		
		return getAvailableStudentsForUserForItem(assignment, AssignmentConstants.GRADE);
	}
	
	private List<String> getAvailableStudentsForUserForItem(Assignment2 assignment, String gradeOrView) {
		if (assignment == null) {
			throw new IllegalArgumentException("null assignment passed to getAvailableStudentsForUserForItem");
		}
		
		if (gradeOrView == null || (!gradeOrView.equals(AssignmentConstants.GRADE) && !gradeOrView.equals(AssignmentConstants.VIEW))) {
			throw new IllegalArgumentException("Invalid gradeOrView " + gradeOrView + " passed to getAvailableStudentsForUserForItem");
		}
		
		List<String> availStudents = new ArrayList();
		
		String contextId = externalLogic.getCurrentContextId();
		
		List<String> allStudentsInSite = externalLogic.getStudentsInSite(contextId);
		
		if (gradebookLogic.isCurrentUserAbleToGradeAll(contextId)) {
			availStudents = allStudentsInSite;
		} else if(gradebookLogic.isCurrentUserAbleToGrade(contextId)) {
			if (assignment.isUngraded()) {
				Set sharedStudents = getStudentsInCurrentUsersSections();
				if (sharedStudents != null) {
					availStudents.addAll(sharedStudents);
				}
			} else {
				// we need to get the students that are viewable in the gb
				if (assignment.getGradableObjectId() != null) {
					Map studentIdFunctionMap = 
						gradebookLogic.getViewableStudentsForGradedItemMap(contextId, assignment.getGradableObjectId());
					if (studentIdFunctionMap != null) {
						if (gradeOrView.equals(AssignmentConstants.VIEW)) {
							availStudents.addAll(studentIdFunctionMap.keySet());
						} else {
							for (Iterator stIter = studentIdFunctionMap.keySet().iterator(); stIter.hasNext();) {
								String studentId = (String)stIter.next();
								if (studentId != null) {
									String function = (String) studentIdFunctionMap.get(studentId);
									if (function != null && function.equals(AssignmentConstants.GRADE)) {
										availStudents.add(studentId);
									}
								}
							}
						}
					}
				}
			}
		}
		
		return availStudents;
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
	
	private Set<String> getStudentsInCurrentUsersSections() {
		// get the user's memberships
		List<String> groupMemberships = externalLogic.getUserMembershipGroupIdList(externalLogic.getCurrentUserId());
		
		// use a set to eliminate section overlap with duplicate students
		Set<String> sharedStudents = new HashSet();
		if (groupMemberships != null) {
			for (Iterator groupIter = groupMemberships.iterator(); groupIter.hasNext();) {
				String groupId = (String) groupIter.next();
				if (groupId != null) {
					List students = externalLogic.getStudentsInSection(groupId);
					if (students != null) {
						sharedStudents.addAll(students);
					}
				}
			}

		}
		
		return sharedStudents;
	}
	
	public boolean isUserAllowedToReleaseFeedbackForAssignment(Assignment2 assignment) {
		// returns true if user is authorized to view at least one student for
		// this assignment
		if (assignment == null) {
			throw new IllegalArgumentException("null assignment passed to isUserAllowedToReleaseFeedbackForAssignment");
		}
		boolean allowedToRelease = false;
		String contextId = externalLogic.getCurrentContextId();
		
		// current user must have some sort of grading privileges in the gb
		if (gradebookLogic.isCurrentUserAbleToGrade(contextId)) {

			List<AssignmentGroup> assignGroupRestrictions = 
				dao.findByProperties(AssignmentGroup.class, new String[] {"assignment"}, new Object[] {assignment});
			
			if (assignment.isUngraded()) {
				if (assignGroupRestrictions == null || assignGroupRestrictions.isEmpty()) {
					allowedToRelease = true;
				} else {
					// the user must be a member of a restricted group
					List<String> userMemberships = externalLogic.getUserMembershipGroupIdList(externalLogic.getCurrentUserId());
					assignment.setAssignmentGroupSet(new HashSet(assignGroupRestrictions));
					if (userMembershipsOverlap(userMemberships, assignment.getListOfAssociatedGroupReferences())) {
						allowedToRelease = true;
					}
				}
			} else {
				// we need to respect grading permissions
				if (assignment.getGradableObjectId() != null) {
					Map<String, String> studentIdFunctionMap = 
						gradebookLogic.getViewableStudentsForGradedItemMap(contextId, assignment.getGradableObjectId());
					if (studentIdFunctionMap != null && studentIdFunctionMap.containsValue(AssignmentConstants.GRADE)) {
						allowedToRelease = true;
					}
				}
			}
		}
		
		return allowedToRelease;
	}

}
