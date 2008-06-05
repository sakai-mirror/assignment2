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

package org.sakaiproject.assignment2.logic;

import java.util.List;
import java.util.Collection;

import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentGroup;

/**
 * This is the interface for logic which is related to the permission 
 * questions in the Assignment2 tool
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public interface AssignmentPermissionLogic {

	/**
	 * @param contextId
	 * 		the context id of the site
	 * @return true if the current user is allowed to edit assignments
	 */
	public boolean isCurrentUserAbleToEditAssignments(String contextId);
	
	/**
	 * 
	 * @param studentId
	 * @param assignment
	 * @return true if the current user is allowed to view the given student's
	 * submission for the given assignment
	 */
	public boolean isUserAbleToViewStudentSubmissionForAssignment(String studentId, Assignment2 assignment);
	
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
	 * @param assignment
	 * @param groupMembershipIds
	 * 			a collection of the ids for all of the current user's group memberships for this site
	 * @return true if the current user is allowed to view the given ungraded
	 * assignment
	 */
	public boolean isUserAbleToViewUngradedAssignment(Assignment2 assignment, Collection<String> groupMembershipIds);
	
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
	 * @return true if the user has either grading or editing permission for the
	 * assignments in the given context id and will thus be able to access
	 * the "instructor view" for the assignments tool. 
	 */
	public boolean isUserAbleToAccessInstructorView(String contextId);
	
	/**
	 * 
	 * @param assignment
	 * @return a list of student ids that the current user is able to view
	 * for the given assignment
	 * @throws SecurityException if current user does not have grading privileges
	 */
	public List<String> getViewableStudentsForUserForItem(Assignment2 assignment);
	
	/**
	 * 
	 * @param assignment
	 * @return a list of student ids that the current user is able to submit
	 * feedback for on the given assignment
	 * @throws SecurityException if current user does not have grading privileges
	 */
	public List<String> getGradableStudentsForUserForItem(Assignment2 assignment);

	/**
	 * 
	 * @param assignment
	 * @return true if the current user is allowed to make a submission for the given assignment
	 */
	public boolean isUserAbleToMakeSubmissionForAssignment(Assignment2 assignment);

	/**
	 * 
	 * @param contextId
	 * @param assignment
	 * @return true if the current user is allowed to make a submission for the
	 * given assignment
	 */
	public boolean isUserAbleToMakeSubmissionForAssignment(String contextId, Assignment2 assignment);
	
	/**
	 * 
	 * @param assignment
	 * @return true if the current user is authorized to submit feedback (not just view) for
	 * at least one submission associated with the given assignment
	 */
	public boolean isUserAllowedToProvideFeedbackForAssignment(Assignment2 assignment);
	
	/**
	 * @param contextId
	 * @return true if the current user is a student in the gradebook
	 */
	 public boolean isCurrentUserAbleToSubmit(String contextId);
}
