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
* The AssignmentSubmissionAttachment object.  AssignmentSubmissionAttachments 
* are attachments associated with the submission.
* 
* @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
*/
public class AssignmentSubmissionAttachment {

	private String assignAttachId;
	private String submissionId;
	private String attachmentId;
	
	public AssignmentSubmissionAttachment() {
		
	}

	/**
	 * 
	 * @return the assignAttachId for this submission attachment
	 */
	public String getAssignAttachId() {
		return assignAttachId;
	}

	/**
	 * set the assignAttachId for this submission attachment
	 * @param assignAttachId
	 */
	public void setAssignAttachId(String assignAttachId) {
		this.assignAttachId = assignAttachId;
	}

	/**
	 * 
	 * @return the id of the submission associated with this attachment
	 */
	public String getSubmissionId() {
		return submissionId;
	}

	/**
	 * set the id of the submission associated with this attachment
	 * @param submissionId
	 */
	public void setSubmissionId(String submissionId) {
		this.submissionId = submissionId;
	}

	/**
	 * 
	 * @return the reference to this submission attachment
	 */
	public String getAttachmentId() {
		return attachmentId;
	}

	/**
	 * set the reference to this submission attachment
	 * @param attachmentId
	 */
	public void setAttachmentId(String attachmentId) {
		this.attachmentId = attachmentId;
	}
}
