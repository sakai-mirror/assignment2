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

import org.sakaiproject.assignment2.exception.AssignmentNotFoundException;
import org.sakaiproject.assignment2.model.Assignment2;


/**
 * This is the interface for the Assignment object
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public interface AssignmentLogic {
	// sorting information
    public static final String SORT_DIR_ASC = "asc";
    public static final String SORT_DIR_DESC = "desc";
    public static final String SORT_BY_INDEX = "sortIndex";
    public static final String SORT_BY_TITLE = "title";
    public static final String SORT_BY_OPEN = "openDate";
    public static final String SORT_BY_DUE = "dueDate";
    
    public static final String REDIRECT_ASSIGNMENT_VIEW_ID = "redirectAssignmentViewId";
	
	/**
	 * 
	 * @param assignmentId
	 * @return Returns the Assignment based on its assignmentId. Does not populate
	 * the AssignmentAttachments and AssignmentGroups
	 * @throws AssignmentNotFoundException if no assignment exists with the given id
	 */
	public Assignment2 getAssignmentById(Long assignmentId);
	
	/**
	 * Create or update an assignment. The contextId must be populated on the assignment
	 * @param assignment
	 * the assignment to create or update
	 * @throws SecurityException -
	 * user must have "edit" permission to add or update an assignment
	 * @throws NoGradebookItemForGradedAssignmentException - if the
	 * assignment is marked as graded but there is no gradebookItemId
	 * @throws AssignmentNotFoundException if the id associated with the
	 * assignment object does not exist
	 */
	public void saveAssignment(Assignment2 assignment);
	
	/**
	 * Delete an Assignment 
	 * note: no assignments are actually deleted; the "removed" property
	 * will be set to true
	 * @param assignment
	 * 			the Assignment to delete
	 * @throws SecurityException - user must have "edit" permission
	 * @throws AnnouncmentPermissionException if the user does not have
	 * permission to delete announcements - assignment will be 'deleted' regardless
	 * @throws CalendarPermissionException if the user does not have
	 * permission to delete events in the Schedule tool - assignment will be
	 * 'deleted' regardless
	 */	
	public void deleteAssignment(Assignment2 assignment);
	
	/**
	 * Returns list of Assignment objects that the given user has permission
	 * to view or grade. Assignments that the user does not have permission 
	 * to view or grade will not be returned. 
	 * @return A non-null list of viewable assignments ordered by sort index
	 */
	public List<Assignment2> getViewableAssignments();
	
	/**
	 * Reorder the assignments in your site. The array of assignment ids must
	 * represent all of the assignments in your site
	 * @param assignmentIds - an array of Long assignment ids that are ordered in the
	 * order that you would like the assignments in the site to appear. 
	 */
	public void reorderAssignments(List<Long> assignmentIds);
	
	/**
	 * @param assignmentId
	 * @return the Assignment2 object with the given id and populate associated
	 * data (ie attachments, groups). Does not include student submission information
	 * @throws AssignmentNotFoundException if no assignment exists with the given id
	 */
	public Assignment2 getAssignmentByIdWithAssociatedData(Long assignmentId);
	
	/** 
	 * @param assignmentId
	 * @return the Assignment2 object with the given id and populate the
	 * associated AssignmentGroups
	 * @throws AssignmentNotFoundException if no assignment exists with the given id
	 */
	public Assignment2 getAssignmentByIdWithGroups(Long assignmentId);
	
	/**
	 * @param assignmentId
	 * @return the Assignment2 object with the given id and populate the
	 * associated AssignmentGroups and AssignmentAttachments
	 * @throws AssignmentNotFoundException if no assignment exists with the given id
	 */
	public Assignment2 getAssignmentByIdWithGroupsAndAttachments(Long assignmentId);

	/**
	 * Uses the open, due, and accept until dates to determine the current status
	 * of the given assignment
	 * @param assignment
	 * @return a constant equivalent to the assignment's status
	 */
	public int getStatusForAssignment(Assignment2 assignment);

}
