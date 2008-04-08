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
	// sorting information
    public static final String SORT_DIR_ASC = "asc";
    public static final String SORT_DIR_DESC = "desc";
    public static final String SORT_BY_INDEX = "sortIndex";
    public static final String SORT_BY_TITLE = "title";
    public static final String SORT_BY_FOR = "for";
    public static final String SORT_BY_STATUS = "status";
    public static final String SORT_BY_OPEN = "openTime";
    public static final String SORT_BY_DUE = "dueDate";
	
	/**
	 * 
	 * @param assignmentId
	 * @return Returns the Assignment based on its assignmentId. Does not populate
	 * the AssignmentAttachments and AssignmentGroups
	 */
	public Assignment2 getAssignmentById(Long assignmentId);
	
	/**
	 * Create or update an assignment in the current context
	 * @param assignment
	 * the assignment to create or update
	 * @throws SecurityException -
	 * user must have "edit" permission to add or update an assignment
	 * @throws NoGradebookItemForGradedAssignmentException - if the
	 * assignment is marked as graded but there is no gradableObjectId
	 */
	public void saveAssignment(Assignment2 assignment);
	
	/**
	 * Create or update an assignment in the given context
	 * @param assignment
	 * the assignment to create or update
	 * @param contextId - the contextId that this new assignment is associated
	 * with.
	 * @throws SecurityException -
	 * user must have "edit" permission to add or update an assignment
	 * @throws NoGradebookItemForGradedAssignmentException - if the
	 * assignment is marked as graded but there is no gradableObjectId
	 */
	public void saveAssignment(Assignment2 assignment, String contextId);
	
	/**
	 * Delete an Assignment 
	 * note: no assignments are actually deleted; the "removed" property
	 * will be set to true
	 * @param assignment
	 * 			the Assignment to delete
	 * @throws SecurityException -
	 * user must have "delete" permission
	 * @throws AnnouncmentPermissionException if the user does not have
	 * permission to delete announcements - assignment will be 'deleted' regardless
	 */	
	public void deleteAssignment(Assignment2 assignment);
	
	/**
	 * Returns list of Assignment objects that the given user has permission
	 * to view or grade. Assignments that the user does not have permission 
	 * to view or grade will not be returned. If the assignment is graded,
	 * will populate the gradebook-related info (ie due date, points possible)
	 * @return
	 */
	public List<Assignment2> getViewableAssignments();
	
	/**
	 * Reorder the assignments in your site. The array of assignment ids must
	 * represent all of the assignments in your site
	 * @param assignmentIds - an array of Long assignment ids that are ordered in the
	 * order that you would like the assignments in the site to appear. 
	 */
	public void setAssignmentSortIndexes(Long[] assignmentIds);
	
	/**
	 * retrieve the Assignment2 object with the given id and populate associated
	 * data (ie attachments, groups, any gradebook data). Does not include
	 * student submission information
	 * @param assignmentId
	 * @return
	 */
	public Assignment2 getAssignmentByIdWithAssociatedData(Long assignmentId);
	
	/**
	 * retrieve the Assignment2 object with the given id and populate the
	 * associated AssignmentGroups
	 * @param assignmentId
	 * @return
	 */
	public Assignment2 getAssignmentByIdWithGroups(Long assignmentId);
	
	/**
	 * retrieve the Assignment2 object with the given id and populate the
	 * associated AssignmentGroups and AssignmentAttachments
	 * @param assignmentId
	 * @return
	 */
	public Assignment2 getAssignmentByIdWithGroupsAndAttachments(Long assignmentId);

	/**
	 * Uses the open, due, and accept until dates to determine the current status
	 * of the given assignment
	 * @param assignment
	 * @return a constant equivalent to the assignment's status
	 */
	public int getStatusForAssignment(Assignment2 assignment);
	
	/**
	 * Given the originalAssignment and the updated (or newly created) version, will determine if an
	 * announcement needs to be added, updated, or deleted. Announcements are updated
	 * if there is a change in title, open date, or group restrictions. They are
	 * deleted if the assignment is changed to draft status. 
	 * @param originalAssignmentWithGroups - original assignment with the group info populated
	 * @param updatedAssignment - updated (or newly created) assignment with the group info populated
	 */
	public void saveAssignmentAnnouncement(Assignment2 originalAssignmentWithGroups, 
			Assignment2 updatedAssignmentWithGroups);
}
