/**********************************************************************************
 * $URL:https://source.sakaiproject.org/contrib/assignment2/trunk/api/logic/src/java/org/sakaiproject/assignment2/logic/AssignmentPermissionLogic.java $
 * $Id:AssignmentPermissionLogic.java 48274 2008-04-23 20:07:00Z wagnermr@iupui.edu $
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

package org.sakaiproject.assignment2.logic;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.sakaiproject.assignment2.exception.SubmissionNotFoundException;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.site.api.Group;

/**
 * This is the interface for logic which is related to the permission 
 * questions in the Assignment2 tool
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public interface AssignmentPermissionLogic {
    
    /**
     * 
     * @param contextId
     * @param permissions a list of the permissions you want to check for in this context.
     * (see {@link AssignmentAuthzLogic#getSiteLevelPermissions()} for allowed values.
     * if null, will return all site-level permissions
     * @return a map of the permission to true/false indicating whether the current
     * user has this permission. Only permissions returned by {@link AssignmentAuthzLogic#getSiteLevelPermissions()}
     * will be considered and returned
     */
    public Map<String, Boolean> getPermissionsForSite(String contextId, List<String> permissions);
    
    /**
     * 
     * @param assignments
     * @param permissions a list of the permissions you want to check for each assignment.
     * (see {@link AssignmentAuthzLogic#getAssignmentLevelPermissions()} for allowed values.
     * if null, will return all assignment-level permissions
     * @return a map of the assignment id to a map of the permission to a true/false value indicating
     * whether the current user has that permission for this assignment 
     * (ie <assignmentId <{@link AssignmentConstants#PERMISSION_EDIT_ASSIGNMENTS}, TRUE>).
     * Only permissions returned by {@link AssignmentAuthzLogic#getAssignmentLevelPermissions()}
     * will be considered and returned
     */
    public Map<Long, Map<String, Boolean>> getPermissionsForAssignments(List<Assignment2> assignments, List<String> permissions);
    
    /**
     * @param contextId
     * @return true if the current user has permission to add assignments in the given context.
     * Note that this does not mean that a user is allowed to add any assignment. This just
     * answers the general question, "Does this user have any sort of add assignment permission 
     * in this site?"  If you want to know if a user may add a specific assignment, see
     * {@link AssignmentPermissionLogic#isUserAllowedToAddAssignment(Assignment2)}
     */
    public boolean isUserAllowedToAddAssignments(String contextId);
    
    /**
     * 
     * @param assignment
     * @return true if the current user is allowed to add the given assignment. Users without
     * the "all groups" permission are not allowed to add assignments that aren't restricted
     * to that user's groups. If you want an answer to the general question,"Does
     * this user have any sort of add assignment permission in this site?" use
     * {@link AssignmentPermissionLogic#isUserAllowedToAddAssignments(String)}
     */
    public boolean isUserAllowedToAddAssignment(Assignment2 assignment);
    
    /**
     * @param assignment
     * @return true if the current user has permission to edit the given assignment
     */
    public boolean isUserAllowedToEditAssignment(Assignment2 assignment);
    
    /**
     * 
     * @param contextId
     * @return true if current user is allowed to edit all assignments in the
     * given context
     */
    public boolean isUserAllowedToEditAllAssignments(String contextId);
    
    /**
     * 
     * @param assignment
     * @return true if current user has permission to delete this assignment
     */
    public boolean isUserAllowedToDeleteAssignment(Assignment2 assignment);

    /**
     * 
     * @param studentId
     * @param assignmentId
     * @return true if the current user is allowed to view the given student's
     * submission for the given assignment
     */
    public boolean isUserAbleToViewStudentSubmissionForAssignment(String studentId, Long assignmentId);

    /**
     * 
     * @param studentId
     * @param assignment
     * @return true if the current user is allowed to provide feedback for the
     * given student and assignment
     */
    public boolean isUserAbleToProvideFeedbackForStudentForAssignment(String studentId, Assignment2 assignment);

    /**
     * 
     * @param submissionId
     * @return true if the current user is allowed to provide feedback for the
     * AssignmentSubmission associated with the given submissionId
     * @throws SubmissionNotFoundException if no submission exists with the given submissionId
     */
    public boolean isUserAbleToProvideFeedbackForSubmission(Long submissionId);

    /**
     * 
     * @param groupIdList
     * 		a list of the group ids for the user's membership
     * @param assignmentGroupSet
     * 		set of AssignmentGroups that the assignments is restricted to
     * @return true if the user is a member of one of the given AssignmentGroup restrictions
     */
    public boolean isUserAMemberOfARestrictedGroup(Collection<String> groupIdList, Collection<AssignmentGroup> assignmentGroups);

    /**
     * 
     * @return true if the user has at least one of the following permissions:
     * {@link AssignmentConstants#PERMISSION_ADD_ASSIGNMENTS},
     * {@link AssignmentConstants#PERMISSION_EDIT_ASSIGNMENTS},
     * {@link AssignmentConstants#PERMISSION_REMOVE_ASSIGNMENTS}, or
     * {@link AssignmentConstants#PERMISSION_MANAGE_SUBMISSIONS} 
     *
     * This will allow the user to access the "instructor view" for the assignments tool. 
     */
    public boolean isUserAbleToAccessInstructorView(String contextId);

    /**
     * @param userId
     * @param assignment
     * @return a list of student ids that the current user is able to view
     * for the given assignment. if assignment has been removed, no 
     * students will be returned
     */
    public List<String> getViewableStudentsForUserForItem(String userId, Assignment2 assignment);

    /**
     * 
     * @param userId
     * @param contextId
     * @param assignmentList
     * @return a map of Assignment2 object to a list of student uids of the students
     * this user is allowed to view for that assignment
     */
    public Map<Assignment2, List<String>> getViewableStudentsForUserForAssignments(String userId, String contextId, List<Assignment2> assignmentList);

    /**
     * @param userId
     * @param assignment
     * @return a list of student ids that the current user is able to submit
     * feedback for on the given assignment. if assignment has been removed,
     * no students will be returned
     */
    public List<String> getGradableStudentsForUserForItem(String userId, Assignment2 assignment);

    /**
     * 
     * @param assignment
     * @return true if the current user has permission to make a submission for the
     * given assignment. only answers permission question. does not check to see
     * if assignment is open, if student already submitted, etc
     */
    public boolean isUserAbleToMakeSubmissionForAssignment(Assignment2 assignment);

    /**
     * 
     * @param assignment
     * @return true if the current user is authorized to submit feedback (not just view) for
     * at least one submission associated with the given assignment
     */
    public boolean isUserAllowedToProvideFeedbackForAssignment(Assignment2 assignment);

    /**
     * @param contextId
     * @return true if the current user is a "student" according to the gradebook's definition
     * and is allowed to make submissions for assignments in this site
     */
    public boolean isCurrentUserAbleToSubmit(String contextId);

    /**
     * 
     * @param assignmentId
     * @return true if the current user has access to this assignment. some scenarios that
     * would be false: if user is a student and assignment is restricted to groups outside of student's memberships
     * or not open;
     * if user is TA but does not have grading privileges for the assign's associated gb item;
     * note: if assignment has been removed, only a student with an existing
     * submission for that assignment may view the assignment
     */
    public boolean isUserAbleToViewAssignment(Long assignmentId);

    /**
     * @param studentId
     * @param assignment
     * @return a list of the userIds of users who are able to view the given
     * student's submission(s) for the given assignment. does not include the student
     * as a user who may view the student. 
     */
    public List<String> getUsersAllowedToViewStudentForAssignment(String studentId, Assignment2 assignment);

    /**
     * 
     * @param contextId
     * @return a list of Groups that the current user is allowed to view in
     * the given context. If the user has grading privileges, will return the groups
     * that the user is able to view in the gradebook. if the user is a student,
     * will return their group memberships
     */
    public List<Group> getViewableGroupsForCurrentUser(String contextId);

    /**
     * 
     * @param assignmentId
     * @return a list of Groups that the current user is allowed
     * to view filtered for the given assignment. 
     * See {@link #getViewableGroupsForCurrentUser(String)}
     */
    public List<Group> getViewableGroupsForCurrUserForAssignment(Long assignmentId);

    /**
     * 
     * @param studentUids
     * @param assignment
     * @return true if the current user is allowed to provide feedback for every
     * student in the given collection for the given assignment
     */
    public boolean isUserAbleToProvideFeedbackForStudents(Collection<String> studentUids, Assignment2 assignment);

    /**
     * 
     * @param contextId
     * @return true if the current user is allowed to provide feedback without
     * restriction for all students in the given context
     */
    public boolean isUserAbleToProvideFeedbackForAllStudents(String contextId);

    /**
     * 
     * @param contextId - all assignments in the assignmentList must be associated with this contextId
     * @param assignmentList - a collection of Assignment2 objects with the AssignmentGroupSet initialized
     * that you want to filter
     * @return a filtered set of assignments from the given assignmentList that the current user
     * is allowed to view. if an assignment is graded but the associated gb item no longer exists, 
     * it is treated as "ungraded" for determining permission.  if assignment has been removed, only a 
     * student with an existing submission for that assignment may view the assignment
     */
    public List<Assignment2> filterViewableAssignments(String contextId, Collection<Assignment2> assignmentList);
}
