/**********************************************************************************
 * $URL: https://source.sakaiproject.org/svn/syllabus/trunk/syllabus-api/src/java/org/sakaiproject/api/app/syllabus/SyllabusItem.java $
 * $Id: SyllabusItem.java 8802 2006-05-03 15:06:26Z cwen@iupui.edu $
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

	private String assignmentGroupId;
	private String assignmentId;
	private String groupId;
	
	public AssignmentGroup() {
		
	}
	
	/**
	 * 
	 * @return id
	 */
	public String getAssignmentGroupId() {
		return assignmentGroupId;
	}
	
	/**
	 * set the assignmentGroupId
	 * @param assignmentGroupId
	 */
	public void setAssignmentGroupId(String assignmentGroupId) {
		this.assignmentGroupId = assignmentGroupId;
	}
	
	/**
	 * 
	 * @return the assignmentId of the assignment associated with this
	 *  AssignmentGroup
	 */
	public String getAssignmentId() {
		return assignmentId;
	}
	
	/**
	 * set the assignmentId of the assignment associated with this 
	 * AssigmentGroup.  This assignment has a group restriction.
	 * @param assignmentId
	 */
	public void setAssignmentId(String assignmentId) {
		this.assignmentId = assignmentId;
	}

	/**
	 * 
	 * @return the AuthzGroup id for this group/section that is allowed
	 * access to the associated group-restricted assignment
	 */
	public String getGroupId() {
		return groupId;
	}

	/**
	 * access to the assignment will be allowed to this AuthzGroup
	 * @param groupId
	 */
	public void setGroupId(String groupId) {
		this.groupId = groupId;
	}
	
}
