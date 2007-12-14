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
import java.util.Map;
import java.util.Set;

import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentGroup;

/**
 * This is the interface for logic which is related to the permission 
 * questions in the Assignment2 tool
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public interface PermissionLogic {

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
	 * @param userId
	 * @param assignment
	 * @return true if the current user is allowed to view the given ungraded
	 * assignment
	 */
	public boolean isUserAbleToViewUngradedAssignment(String userId, Assignment2 assignment);
	
	/**
	 * 
	 * @param userId
	 * @param assignmentGroupSet
	 * 		set of AssignmentGroups that the assignments is restricted to
	 * @return true if the user is a member of one of the given AssignmentGroup restrictions
	 */
	public boolean isUserAMemberOfARestrictedGroup(String userId, Set<AssignmentGroup> assignmentGroupSet);
	
	/**
	 * 
	 * @return true if the user has either grading or editing permission for the
	 * assignments in the given context id and will thus be able to access
	 * the "instructor view" for the assignments tool. 
	 */
	public boolean isUserAbleToAccessInstructorView(String contextId);
}
