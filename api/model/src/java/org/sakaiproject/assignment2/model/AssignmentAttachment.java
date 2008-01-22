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

	private Long id;
	private Assignment2 assignment;
	private String attachmentReference;
	private int version;

	public AssignmentAttachment() {
	}

	public AssignmentAttachment(Assignment2 assignment, String attachmentReference) {
		this.assignment = assignment;
		this.attachmentReference = attachmentReference;
	}

	/**
	 * @return the id of this assignment attachment
	 */
	public Long getId() {
		return id;
	}

	/**
	 * set the the id of this assignment attachment
	 * @param id
	 */
	public void setId(Long id) {
		this.id = id;
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

	@Override
	public boolean equals(Object obj) {
		if (null == obj) return false;
		if (!(obj instanceof AssignmentAttachment)) return false;
		else {
			AssignmentAttachment compAttach = (AssignmentAttachment) obj;
			if (this.id == null || compAttach.id == null) {
				return false;
			}
			if (null == this.id || null == compAttach.id) return false;
			else return (
					this.id.equals(compAttach.id)
			);
		}
	}

	@Override
	public int hashCode() {
		if (null == this.id) return super.hashCode();
		String hashStr = this.getClass().getName() + ":" + this.id.hashCode();
		return hashStr.hashCode();
	}
}
