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

package org.sakaiproject.assignment2.model;


/**
 * The AssignmentAttachment object.  AssignmentAttachments are attachments
 * associated with the assignment.
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class AssignmentAttachment extends AttachmentBase {

	private Assignment2 assignment;

	public AssignmentAttachment() {
	}

	public AssignmentAttachment(Assignment2 assignment, String attachmentReference) {
		this.attachmentReference = attachmentReference;
		this.assignment = assignment;
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

}
