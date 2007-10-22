/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/api/model/src/java/org/sakaiproject/assignment2/model/AssignmentGroup.java $
 * $Id: AssignmentGroup.java 12544 2006-05-03 15:06:26Z wagnermr@iupui.edu $
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
 * The AssignmentGroup object.  AssignmentGroups are used when the 
 * assignment is restricted to select group(s).
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class AssignmentGroup {

	private Long assignmentGroupId;
	private Assignment2 assignment;
	private String groupId;
	private int version;
	
	public AssignmentGroup() {
		
	}
	
	/**
	 * 
	 * @return id
	 */
	public Long getAssignmentGroupId() {
		return assignmentGroupId;
	}
	
	/**
	 * set the assignmentGroupId
	 * @param assignmentGroupId
	 */
	public void setAssignmentGroupId(Long assignmentGroupId) {
		this.assignmentGroupId = assignmentGroupId;
	}
	
	/**
	 * 
	 * @return the assignment that is restricted to this AssignmentGroup
	 */
	public Assignment2 getAssignment() {
		return assignment;
	}
	
	/**
	 * the assignment that is restricted to this AssignmentGroup
	 * @param assignment
	 */
	public void setAssignment(Assignment2 assignment) {
		this.assignment = assignment;
	}

	/**
	 * 
	 * @return the realm id for this group/section that is allowed
	 * access to the associated group-restricted assignment
	 */
	public String getGroupId() {
		return groupId;
	}

	/**
	 * access to the assignment will be allowed for this realm id
	 * @param groupId
	 */
	public void setGroupId(String groupId) {
		this.groupId = groupId;
	}
	
	public int getVersion() {
		return version;
	}
	
	public void setVersion(int version) {
		this.version = version;
	}
	
}
