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
	 * @return Returns the AssignmentSubmission based on its assignmentSubmissionId
	 */
	public AssignmentSubmission getCurrentSubmissionById(Long submissionId);
	
	/**
	 * 
	 * @param assignment
	 * @param userId
	 * @return AssignmentSubmission associated with the given Assignment and userId
	 */
	public AssignmentSubmission getCurrentSubmissionByAssignmentAndUser(Assignment2 assignment, String userId);
	
	/**
	 * Create or update an AssignmentSubmission
	 * @param assignmentSubmission
	 * 			the AssignmentSubmission to create or update
	 */
	public void saveAssignmentSubmission(AssignmentSubmission assignmentSubmission);
	
	/**
	 * Given an assignmentId, returns the associated AssignmentSubmission records 
	 * without the current submission information populated
	 * @param assignment
	 * @return
	 */
	public List<AssignmentSubmission> getAllSubmissionsByAssignmentIdNoVersionData(Long assignmentId);
	
}
