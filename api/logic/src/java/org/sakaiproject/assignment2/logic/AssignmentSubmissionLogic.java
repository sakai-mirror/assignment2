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
import org.sakaiproject.assignment2.model.AssignmentSubmission;


/**
 * This is the interface for the AssignmentSubmission object
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public interface AssignmentSubmissionLogic {
	
	/**
	 * 
	 * @param submissionId
	 * @return Returns the AssignmentSubmission based on its assignmentSubmissionId.
	 * Does not populate current version information.
	 */
	public AssignmentSubmission getAssignmentSubmissionById(Long submissionId);
	
	
	/**
	 * 
	 * @param assignmentId
	 * @param userId
	 * @param includeDraft
	 * 		false if you want the most recent non-draft submission
	 * @return AssignmentSubmission associated with the given Assignment and studentId
	 * 		will return null if there is no submission info for this student yet
	 * @throws SecurityException if current user not allowed to view student's submission
	 */
	public AssignmentSubmission getCurrentSubmissionByAssignmentIdAndStudentId(Long assignmentId, String studentId, boolean includeDraft);
	
	/**
	 * Create or update an AssignmentSubmission
	 * @param assignmentSubmission
	 * 			the AssignmentSubmission to create or update
	 */
	public void saveStudentSubmission(AssignmentSubmission assignmentSubmission);
	
	/**
	 * 
	 * @param assignmentId
	 * @return all AssignmentSubmissions for this assignmentId that the current
	 * user is allowed to view or grade with the currentVersion information. If
	 * no submission exists yet, returns an empty AssigmentSubmission rec for the
	 * student
	 */
	public List<AssignmentSubmission> getViewableSubmissionsForAssignmentId(Long assignmentId);
	
	/**
	 * sets the submissionStatus (not persisted) for the given assignments
	 * ie submitted, not started, draft, etc
	 * @param assignments
	 * @param studentId
	 */
	public void setSubmissionStatusForAssignments(List<Assignment2> assignments, String studentId);
	
}
