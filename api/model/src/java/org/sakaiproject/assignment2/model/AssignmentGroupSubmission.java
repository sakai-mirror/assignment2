/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/api/model/src/java/org/sakaiproject/assignment2/model/AssignmentGroupSubmission.java $
 * $Id: AssignmentGroupSubmission.java 12544 2006-05-03 15:06:26Z wagnermr@iupui.edu $
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

package org.sakaiproject.assignment2.model;

/**
 * The AssignmentGroupSubmission object.  AssignmentGroupSubmissions are used 
 * when an assignment is configured to accept a group submission instead of
 * requiring submissions from each member of the group.
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class AssignmentGroupSubmission {

	private String assignGroupSubmissionId;
	private String assignmentId;
	private String groupId;
	
	public AssignmentGroupSubmission() {
		
	}

	/**
	 * 
	 * @return the assignGroupSubmissionId for this AssignmentGroupSubmission
	 */
	public String getAssignGroupSubmissionId() {
		return assignGroupSubmissionId;
	}

	/**
	 * set the assignGroupSubmissionId for this AssignmentGroupSubmission
	 * @param assignGroupSubmissionId
	 */
	public void setAssignGroupSubmissionId(String assignGroupSubmissionId) {
		this.assignGroupSubmissionId = assignGroupSubmissionId;
	}

	/**
	 * 
	 * @return the assignmentId for the associated assignment
	 */
	public String getAssignmentId() {
		return assignmentId;
	}

	/**
	 * set the assignmentId for the associated assignment
	 * @param assignmentId
	 */
	public void setAssignmentId(String assignmentId) {
		this.assignmentId = assignmentId;
	}

	/**
	 * 
	 * @return the AuthzGroup id of the group submitting this assignment
	 */
	public String getGroupId() {
		return groupId;
	}

	/**
	 * set the AuthzGroup id of the group submitting this assignment
	 * @param groupId
	 */
	public void setGroupId(String groupId) {
		this.groupId = groupId;
	}
}
