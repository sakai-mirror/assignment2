/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/api/model/src/java/org/sakaiproject/assignment2/model/AssignmentAttachment.java $
 * $Id: AssignmentAttachment.java 12544 2006-05-03 15:06:26Z wagnermr@iupui.edu $
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
* The AssignmentAttachment object.  AssignmentAttachments are attachments
* associated with the assignment.
* 
* @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
*/
public class AssignmentAttachment {

	private Long assignAttachId;
	private Assignment2 assignment;
	private String attachmentReference;
	private int version;
	
	public AssignmentAttachment() {
	}

	/**
	 * @return the assignAttachId
	 */
	public Long getAssignAttachId() {
		return assignAttachId;
	}

	/**
	 * set the assignAttachId
	 * @param assignAttachId
	 */
	public void setAssignAttachId(Long assignAttachId) {
		this.assignAttachId = assignAttachId;
	}

	/**
	 * 
	 * @return the assignment that this attachment is associated with
	 */
	public Assignment2 getAssignment() {
		return assignment;
	}

	/**
	 * set the assignment that this attachment is associated with
	 * @param assignment
	 */
	public void setAssignment(Assignment2 assignment) {
		this.assignment = assignment;
	}

	/**
	 * 
	 * @return the reference to this attachment
	 */
	public String getAttachmentReference() {
		return attachmentReference;
	}

	/**
	 * set the reference to this attachment
	 * @param attachmentReference
	 */
	public void setAttachmentReference(String attachmentReference) {
		this.attachmentReference = attachmentReference;
	}
	
	public int getVersion() {
		return version;
	}
	
	public void setVersion(int version) {
		this.version = version;
	}
}
