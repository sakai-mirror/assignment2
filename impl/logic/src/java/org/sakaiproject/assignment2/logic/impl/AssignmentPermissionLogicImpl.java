/**********************************************************************************
 * $URL:https://source.sakaiproject.org/contrib/assignment2/trunk/impl/logic/src/java/org/sakaiproject/assignment2/logic/impl/AssignmentPermissionLogicImpl.java $
 * $Id:AssignmentPermissionLogicImpl.java 48274 2008-04-23 20:07:00Z wagnermr@iupui.edu $
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
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Collection;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.dao.AssignmentDao;
import org.sakaiproject.assignment2.exception.AssignmentNotFoundException;
import org.sakaiproject.assignment2.exception.SubmissionNotFoundException;
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
    	if (log.isDebugEnabled()) log.debug("init");
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
    	if (contextId == null) {
    		throw new IllegalArgumentException("null contextId passed to isCurrentUserAbleToEditAssignments");
    	}
    	return gradebookLogic.isCurrentUserAbleToEdit(contextId);
    }
    
    public boolean isUserAbleToViewStudentSubmissionForAssignment(String studentId, Long assignmentId) {
    	if (studentId == null || assignmentId == null) {
    		throw new IllegalArgumentException("Null studentId or assignmentId passed to isUserAbleToViewStudentSubmissionForAssignment");
    	}
    	
    	boolean viewable = false;
    	
    	Assignment2 assignment = (Assignment2)dao.findById(Assignment2.class, assignmentId);
    	if (assignment == null) {
    		throw new AssignmentNotFoundException("No assignment found with id: " + assignmentId + " found in isUserAbleToViewStudentSubmissionForAssignment");
    	}
    	
    	if (externalLogic.getCurrentUserId().equals(studentId)) {
    		viewable = true;
    	} else if (gradebookLogic.isCurrentUserAbleToGradeAll(assignment.getContextId())) {
    		viewable = true;
    	} else {
    		if (!assignment.isGraded()) {
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
    
    public boolean isUserAbleToProvideFeedbackForStudentForAssignment(String studentId, Assignment2 assignment) {
    	if (studentId == null || assignment == null) {
    		throw new IllegalArgumentException("null parameter passed to isUserAbleToProvideFeedbackForSubmission");
    	}
    	
    	boolean allowed = false;
    	
    	if (assignment != null) {
    		if (!assignment.isGraded()) {
    			allowed = isUserAbleToViewSubmissionForUngradedAssignment(studentId, assignment);
    		} else {
    			if (assignment.getGradableObjectId() != null) {
    				allowed = gradebookLogic.isCurrentUserAbleToGradeStudentForItem(externalLogic.getCurrentContextId(), 
    						studentId, assignment.getGradableObjectId());
    			}
    		}
    	}
    	
    	return allowed;
    }
    
    public boolean isUserAbleToProvideFeedbackForSubmission(Long submissionId) {
    	if (submissionId == null) {
    		throw new IllegalArgumentException("Null submissionId passed to isUserAbleToProvideFeedbackForSubmission");
    	}
    	
    	AssignmentSubmission submission = (AssignmentSubmission)dao.findById(AssignmentSubmission.class, submissionId);
    	if (submission == null) {
    		throw new SubmissionNotFoundException("No submission exists with id: " + submissionId);
    	}
    	
    	return isUserAbleToProvideFeedbackForStudentForAssignment(submission.getUserId(), submission.getAssignment());
    }
    
    public boolean isUserAbleToViewSubmissionForUngradedAssignment(String studentId, Assignment2 assignment) {
    	if (studentId == null || assignment == null) {
    		throw new IllegalArgumentException("null studentId or assignment passed to isUserAbleToViewSubmissionForUngradedAssignment");
    	}
    	
    	boolean viewable = false;
    	if (gradebookLogic.isCurrentUserAbleToGradeAll(assignment.getContextId())) {
    		viewable = true;
    	} else if (gradebookLogic.isCurrentUserAbleToGrade(assignment.getContextId())) {
    		String currContextId = externalLogic.getCurrentContextId();
    		List<String> currentUserMemberships = externalLogic.getUserMembershipGroupIdList(externalLogic.getCurrentUserId(), currContextId);
    		List<String> studentMemberships = externalLogic.getUserMembershipGroupIdList(studentId, currContextId);
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
    	
    	if (assignment.isGraded()) {
    		throw new IllegalArgumentException("A graded assignment was passed to isUserAbleToViewUngradedAssignment");
    	}
    	
    	boolean viewable = false;
    	
    	// user can grade all
    	if (gradebookLogic.isCurrentUserAbleToGradeAll(assignment.getContextId())) {
    		viewable = true;
    		
    	// user has some grading privileges or they are a student and assign is open
    	} else if (gradebookLogic.isCurrentUserAbleToGrade(assignment.getContextId()) || 
    			(gradebookLogic.isCurrentUserAStudentInGb(assignment.getContextId()) && 
    					assignment.getOpenTime().before(new Date()))) {
    		
    		if (assignment.getAssignmentGroupSet() == null || assignment.getAssignmentGroupSet().isEmpty()) {
    			viewable = true;
    		} else {
    			// the user must be a member of a restricted group to view assignment
    			viewable = isUserAMemberOfARestrictedGroup(groupMembershipIds, assignment.getAssignmentGroupSet());
    		}
    	}
    	
    	return viewable;
    }
    
    public boolean isUserAbleToViewGradedAssignment(Assignment2 assignment, Collection<String> groupMembershipIds) {
    	if (assignment == null) {
    		throw new IllegalArgumentException("Null assignment passed to isUserAbleToViewGradedAssignment");
    	}
    	
    	if (!assignment.isGraded()) {
    		throw new IllegalArgumentException("An ungraded assignment was passed to isUserAbleToViewGradedAssignment");
    	}
    	
    	boolean viewable = false;
    	
    	boolean userIsStudentAndAssignmentNotOpen = gradebookLogic.isCurrentUserAStudentInGb(assignment.getContextId()) && 
    		assignment.getOpenTime().after(new Date());

    	if (!userIsStudentAndAssignmentNotOpen) {

    		boolean viewableInGb = gradebookLogic.isCurrentUserAbleToViewGradebookItem(
    				assignment.getContextId(), assignment.getGradableObjectId());

    		if (viewableInGb) {
    			// if it is a student, we may need to filter by group
    			if (assignment.getAssignmentGroupSet() != null && !assignment.getAssignmentGroupSet().isEmpty() &&
    					gradebookLogic.isCurrentUserAStudentInGb(assignment.getContextId())) {
    				viewable = isUserAMemberOfARestrictedGroup(groupMembershipIds, assignment.getAssignmentGroupSet());
    			} else {
    				viewable = true;
    			}
    		}
    	}
    	
    	return viewable;
    }
    
    public boolean isUserAMemberOfARestrictedGroup(Collection<String> groupMembershipIds, Collection<AssignmentGroup> assignmentGroupSet) {    	
    	if (assignmentGroupSet != null && !assignmentGroupSet.isEmpty()) {
        	if (groupMembershipIds != null) {
	        	for (AssignmentGroup aGroup : assignmentGroupSet) {
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
		
		if (assignment.getId() == null) {
			throw new IllegalArgumentException("null data in not-null fields for assignment passed to isUserAbleToMakeSubmission");
		} 
		
		// TODO - how do we handle certain roles that shouldn't be able to submit?
		// ie guests? they don't have a section flag on their role
		boolean userAbleToSubmit = false;
	
		if (!assignment.isGraded()) {
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
			
			List<String> groupMembershipIds = externalLogic.getUserMembershipGroupIdList(externalLogic.getCurrentUserId(), contextId);
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
		
		List<String> availStudents = new ArrayList<String>();
		
		String contextId = externalLogic.getCurrentContextId();
		String userId = externalLogic.getCurrentUserId();

		List<AssignmentGroup> assignGroupRestrictions = 
			dao.findByProperties(AssignmentGroup.class, new String[] {"assignment"}, new Object[] {assignment});
		
		// if user may grade all, then return all of the students who have this assignment
		// we need to check for group restrictions
		if (gradebookLogic.isCurrentUserAbleToGradeAll(contextId)) {
			if (assignGroupRestrictions == null || assignGroupRestrictions.isEmpty()) {
				availStudents.addAll(externalLogic.getStudentsInSite(contextId));
			} else {
				availStudents = getAllAvailableStudentsGivenGroupRestrictions(
						contextId, assignGroupRestrictions);
			}
			
		} else if(gradebookLogic.isCurrentUserAbleToGrade(contextId)) {
			if (!assignment.isGraded()) {
				// if there are no restrictions, return students in user's section(s)
				if (assignGroupRestrictions == null || assignGroupRestrictions.isEmpty()) {
					Set<String> sharedStudents = getStudentsInCurrentUsersSections(contextId);
					if (sharedStudents != null) {
						availStudents.addAll(sharedStudents);
					}
				} else {
					// otherwise, only return students in his/her section if it is one
					// of the group restrictions
					List<String> memberships = externalLogic.getUserMembershipGroupIdList(userId, contextId);
					if (memberships != null && !memberships.isEmpty()) {
						for (AssignmentGroup group : assignGroupRestrictions) {
							if (group != null && memberships.contains(group.getGroupId())) {
								availStudents.addAll(externalLogic.getStudentsInSection(group.getGroupId()));
							}
						}
					}
				}
				
			} else {
				// we need to get the students that are viewable in the gb
				List<String> viewableInGb = new ArrayList<String>();
				if (assignment.getGradableObjectId() != null) {
					Map<String, String> studentIdFunctionMap = 
						gradebookLogic.getViewableStudentsForGradedItemMap(contextId, assignment.getGradableObjectId());
					if (studentIdFunctionMap != null) {
						if (gradeOrView.equals(AssignmentConstants.VIEW)) {
							viewableInGb.addAll(studentIdFunctionMap.keySet());
						} else {
							for (Map.Entry<String, String> entry : studentIdFunctionMap.entrySet()) {
								String studentId = entry.getKey();
								String function = entry.getValue();
								if (studentId != null && function != null
										&& function.equals(AssignmentConstants.GRADE)) {
									viewableInGb.add(studentId);
								}
							}
						}
					}
				}
				
				// now we need to filter out the ones who aren't associated w/ this assignment
				if (assignGroupRestrictions == null || assignGroupRestrictions.isEmpty()) {
					availStudents.addAll(viewableInGb);
				} else {
					List<String> availForAssign = getAllAvailableStudentsGivenGroupRestrictions(
							contextId, assignGroupRestrictions);
					if (availForAssign != null && !availForAssign.isEmpty()) {
						for (String studentId : availForAssign) {
							if (studentId != null && viewableInGb.contains(studentId)) {
								availStudents.add(studentId);
							}
						}
					}
				}
			}
		} else {
			// this user does not have grading privileges so should not be
			// accessing this method!
			throw new SecurityException("User " + userId + " attempted to view/grade student " +
					"submissions without authorization!");
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
			for (String user1Group : user1GroupIds) {
				if (user1Group != null && user2GroupIds.contains(user1Group)) {
					return true;
				}
			}
		}
		
		return false;
	}
	
	private Set<String> getStudentsInCurrentUsersSections(String contextId) {
		// get the user's memberships
		List<String> groupMemberships = externalLogic.getUserMembershipGroupIdList(externalLogic.getCurrentUserId(), contextId);
		
		// use a set to eliminate section overlap with duplicate students
		Set<String> sharedStudents = new HashSet<String>();
		if (groupMemberships != null) {
			for (String groupId : groupMemberships) {
				if (groupId != null) {
					List<String> students = externalLogic.getStudentsInSection(groupId);
					if (students != null) {
						sharedStudents.addAll(students);
					}
				}
			}

		}
		
		return sharedStudents;
	}
	
	public boolean isUserAllowedToProvideFeedbackForAssignment(Assignment2 assignment) {
		// returns true if user is authorized to grade at least one student for
		// this assignment
		if (assignment == null) {
			throw new IllegalArgumentException("null assignment passed to isUserAllowedToProvideFeedbackForAssignment");
		}
		boolean allowedToGrade = false;
		
		try {
			List<String> gradableStudents = getGradableStudentsForUserForItem(assignment);
			if (gradableStudents != null && gradableStudents.size() > 0) {
				allowedToGrade = true;
			}
		} catch (SecurityException se) {
			// this user does not have grading privileges, so may not release
			allowedToGrade = false;
		}
		
		return allowedToGrade;
	}
	
	public boolean isCurrentUserAbleToSubmit(String contextId) {
		return gradebookLogic.isCurrentUserAStudentInGb(contextId);
	}
	
	/**
	 * 
	 * @param contextId
	 * @param assignGroupRestrictions
	 * @return a list of all of the students who are able to view this assignment
	 * given any group restrictions
	 */
	private List<String> getAllAvailableStudentsGivenGroupRestrictions(String contextId, Collection<AssignmentGroup> assignGroupRestrictions) {
		List<String> allStudentsForAssign = new ArrayList<String>();
		
		// if there are group restrictions, only a subset of all of the students in the
		// class will be available for this assignment
		if (assignGroupRestrictions == null || assignGroupRestrictions.isEmpty()) {
			allStudentsForAssign = externalLogic.getStudentsInSite(contextId);
		} else {
			// use a set to make sure students only appear once, even if they
			// are in multiple sections
			Set<String> studentsInRestrictedGroups = new HashSet<String>();
			for (AssignmentGroup group : assignGroupRestrictions) {
				if (group != null) {
					studentsInRestrictedGroups.addAll(externalLogic.getStudentsInSection(group.getGroupId()));
				}
			}
			
			allStudentsForAssign.addAll(studentsInRestrictedGroups);
		}
		
		return allStudentsForAssign;
	}

	public boolean isUserAbleToViewAssignment(String contextId, Long assignmentId) {
		if (contextId == null || assignmentId == null) {
			throw new IllegalArgumentException("Null contextId or assignmentId passed to " +
					"isUserAbleToViewAssignment- contextId: " + contextId + " assignmentId: " + assignmentId);
		}
		boolean allowed = false;

		// retrieve the assignment
		Assignment2 assign = dao.getAssignmentByIdWithGroups(assignmentId);
		List<String> groupMembershipIds = new ArrayList<String>();
		if (assign.getAssignmentGroupSet() != null && !assign.getAssignmentGroupSet().isEmpty()) {
			String currUser = externalLogic.getCurrentUserId();
			groupMembershipIds = externalLogic.getUserMembershipGroupIdList(currUser, contextId);
		}

		if (assign.isGraded()) {
			allowed = isUserAbleToViewGradedAssignment(assign, groupMembershipIds);
		} else {
			allowed = isUserAbleToViewUngradedAssignment(assign, groupMembershipIds);
		}
		
		return allowed;
	}

}
