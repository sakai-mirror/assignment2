/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/api/logic/src/java/org/sakaiproject/assignment2/logic/ExternalGradebookLogic.java $
 * $Id: ExternalGradebookLogic.java 12544 2006-05-03 15:06:26Z wagnermr@iupui.edu $
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

import java.util.Date;
import java.util.List;
import java.util.Map;

import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;

/**
 * This is the interface for logic which is related to the integration
 * with the Gradebook tool
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public interface ExternalGradebookLogic {

    /**
     * Given a list of graded Assignment2 objects, filter out the objects
     * that the user is not authorized to view according to the gradebook
     * permissions and return a list of graded assignments that the user is
     * authorized to view. Also populates the gb-specific fields for each
     * Assignment2 object. Does NOT handle filtering according to AssignmentGroup 
     * restrictions 
     * @param gradedAssignments
     * @param contextId
     * @return
     */
	public List<Assignment2> getViewableAssignmentsWithGbData(List<Assignment2> gradedAssignments, String contextId);
	
	/**
	 * The Assignment2 tool stores all grading information in the gradebook. Thus,
	 * the gradebook backend must exist for the Assignment2 tool to work. This
	 * method will check to see if the gradebook data exists and, if not, will
	 * add it
	 * @param contextId
	 */
	public void createGradebookDataIfNecessary(String contextId);
	
	/**
	 * 
	 * @param contextId
	 * @return a map of gradable object id to title for all of the gradebook
	 * items that the current user may view or grade.
	 */
	public Map getViewableGradableObjectIdTitleMap(String contextId);
	
	/**
	 * returns a list of GradebookItem objects that represent all of
	 * the gradebook items currently defined in the gradebook tool
	 * @param contextId
	 * @return
	 * @throws SecurityException if user does not have edit or grade perm
	 */
	public List<GradebookItem> getAllGradebookItems(String contextId);
	
	/**
	 * returns a map of the group id to name for
	 * all of the groups/sections that the current user is authorized to view
	 * according to the gradebook grader permissions
	 * @param contextId
	 * @return
	 */
	public Map<String, String> getViewableGroupIdToTitleMap(String contextId);
	
	/**
	 * Using the grader permissions, returns a map of all of the student ids of 
	 * the students that the current user is allowed to view or grade
	 * to the actual function (grade/view)
	 * @param contextId
	 * @param gradableObjectId
	 * @return
	 */
	public Map<String, String> getViewableStudentsForGradedItemMap(String contextId, Long gradableObjectId);
	
	/**
	 * @param contextId
	 * @return true if the current user is authorized to edit the gradebook
	 */
	public boolean isCurrentUserAbleToEdit(String contextId);
	
	/**
	 * @param contextId
	 * @return true if the current user is authorized to grade all in gradebook
	 */
	public boolean isCurrentUserAbleToGradeAll(String contextId);
	
	/**
	 * @param contextId
	 * @return true if the current user is authorized to grade in some 
	 * capacity in the gradebook.  (ie they may grade all or grade
	 * section)
	 */
	public boolean isCurrentUserAbleToGrade(String contextId);
	
	/**
	 * @param contextId
	 * @return true if current user is authorized to view their own
	 * grades in the gradebook
	 */
	public boolean isCurrentUserAbleToViewOwnGrades(String contextId);
	
	/**
	 * 
	 * @param contextId
	 * @return true if the current user does not have grading or editing 
	 * privileges in the gradebook but does have the view own grades perm
	 */
	public boolean isCurrentUserAStudentInGb(String contextId);
	
	/**
	 * @contextId
	 * @param studentId
	 * @param gradableObjectId
	 * @return true if the current user is authorized to grade the given student
	 * for the given gb item id
	 */
	public boolean isCurrentUserAbleToGradeStudentForItem(String contextId, String studentId, Long gradableObjectId);
	
	/**
	 * 
	 * @param contextId
	 * @param studentId
	 * @param gbItemId
	 * @return AssignmentConstants.GRADE if current user is able to grade this item for this student,
	 * AssignmentConstants.VIEW if user is only able to view the grade, and null if
	 * the user may not view this student for the given item at all
	 */
	public String getGradeViewPermissionForCurrentUserForStudentForItem(String contextId, String studentId, Long gbItemId);
	
	/**
	 * 
	 * @param contextId
	 * @param studentId
	 * @param gbItemId
	 * @return the grade in the gb for the given gradable object id and student. null if no
	 * grade or if the gb item does not exist
	 */
	public String getStudentGradeForItem(String contextId, String studentId, Long gbItemId);
	
	/**
	 * 
	 * @param contextId
	 * @param studentId
	 * @param gbItemId
	 * @return the grade comment in the gb for the given gradable object id and student. null if no
	 * comment or if the gb item does not exist
	 */
	public String getStudentGradeCommentForItem(String contextId, String studentId, Long gbItemId);
	
	/**
	 * given a list of submissions and the parent assignment, populates the grade information
	 * for each submission
	 * @param submissionList
	 * @param assignment
	 */
	public void populateGradesForSubmissions(List<AssignmentSubmission> submissionList, Assignment2 assignment);
	
	/**
	 * will populate gradebook-related fields on the submission record 
	 * ie grade, comment, released, etc. if the submission is for the currUserId
	 * you pass and the grade info has not been released, grade and comment will
	 * not be populated
	 * @param contextId
	 * @param currUserId
	 * @param submission
	 */
	public void populateAllGradeInfoForSubmission(String contextId, String currUserId, AssignmentSubmission submission);
	
	/**
	 * will populate the gradebook-related fields for the given assignment
	 * ie due date, points possible. if the gradebook item doesn't exist any
	 * more will set the needsUserAttention flag to true
	 * @param contextId
	 * @param assignment
	 */
	public void populateGradebookItemDetailsForAssignment(String contextId, Assignment2 assignment);

	/**
	 * 
	 * @param contextId
	 * @return the type of grade
	 */
	public String getGradeType(String contextId);
	
	/**
	 * Create a gradebook item in the gradebook tool with the given information.
	 * @param contextId
	 * @param title
	 * @param pointsPossible
	 * @param dueDate
	 * @param releasedToStudents - true if this item should be available to students
	 * @param countedInCourseGrade - true if grades for this gb item will be included
	 * 			in course grade - may only be true if releasedToStudents is true
	 * @return id of the newly created gradebook item in the gradebook.
	 */
	public Long createGbItemInGradebook(String contextId, String title, Double pointsPossible, Date dueDate,
			boolean releasedToStudents, boolean countedInCourseGrade);
	
	/**
	 * Return the GradebookItem object associated with the given gradableObjectId
	 * in the given contextId in the gradebook tool. Throws IllegalArgumentException
	 * if given id does not exist
	 * @param contextId
	 * @param gradableObjectId
	 * @return
	 */
	public GradebookItem getGradebookItemById(String contextId, Long gradableObjectId);
}
