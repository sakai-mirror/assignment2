/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/api/logic/src/java/org/sakaiproject/assignment2/dao/AssignmentDao.java $
 * $Id: AssignmentDao.java 12544 2006-05-03 15:06:26Z wagnermr@iupui.edu $
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
import org.sakaiproject.assignment2.model.Assignment2;


/**
 * This is the interface for the Assignment object
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public interface AssignmentLogic {
	
	/**
	 * 
	 * @param assignmentId
	 * @return Returns the Assignment based on its assignmentId
	 */
	public Assignment2 getAssignmentById(Long assignmentId);
	
	/**
	 * Create or update an assignment
	 * @param assignment
	 * the assignment to create or update
	 * @throws SecurityException -
	 * user must have "revise" permission to update or "create" permission if
	 * assignment is new
	 * @throws IllegalArgumentException -
	 * if assignment is null
	 * @throws ConflictingAssignmentNameException -
	 * if it is a new assignment and the title already exists
	 */
	public void saveAssignment(Assignment2 assignment);
	
	/**
	 * Delete an Assignment 
	 * note: no assignments are actually deleted; the "removed" property
	 * will be set to true
	 * @param assignment
	 * 			the Assignment to delete
	 * @throws SecurityException -
	 * user must have "delete" permission
	 * @throws IllegalArgumentException -
	 * if assignment is null
	 */	
	public void deleteAssignment(Assignment2 assignment);
	
	/**
	 * Returns list of Assignment objects that the given user has permission
	 * to view or grade. Assignments that the user does not have permission 
	 * to view or grade will not be returned.
	 * @return
	 */
	public List<Assignment2> getViewableAssignments();
	
	public List<Assignment2> getViewableAssignments(String userId, String sortProperty, boolean ascending, int start, int limit);
	
	public int getTotalCountViewableAssignments(String userId);
	
	public void setAssignmentSortIndexes(Long[] assignmentIds);
	
	/**
	 * retrieve the Assignment2 object with the given id and populate associated
	 * data (ie attachments, groups, any gradebook data). Does not include
	 * student submission information
	 * @param assignmentId
	 * @return
	 */
	public Assignment2 getAssignmentByIdWithAssociatedData(Long assignmentId);

}
