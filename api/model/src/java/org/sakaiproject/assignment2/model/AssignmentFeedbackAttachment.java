/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/api/model/src/java/org/sakaiproject/assignment2/model/AssignmentFeedbackAttachment.java $
 * $Id: AssignmentFeedbackAttachment.java 12544 2006-05-03 15:06:26Z wagnermr@iupui.edu $
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
* The AssignmentFeedbackAttachment object.  AssignmentFeedbackAttachments 
* are attachments added during grading as part of the grader's feedback on
* a submission.
* 
* @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
*/
public class AssignmentFeedbackAttachment {

	private Long id;
	private AssignmentSubmissionVersion submissionVersion;
	private String attachmentReference;
	
	public AssignmentFeedbackAttachment() {
		
	}

	/**
	 * 
	 * @return the id of this feedback attachment
	 */
	public Long getId() {
		return id;
	}

	/**
	 * set the id of this feedback attachment
	 * @param id
	 */
	public void setId(Long id) {
		this.id = id;
	}

	/**
	 * 
	 * @return the AssignmentSubmissionVersion rec that this feedback attachment is associated with
	 */
	public AssignmentSubmissionVersion getSubmissionVersion() {
		return submissionVersion;
	}

	/**
	 * set the AssignmentSubmissionVersion that this feedback attachment is associated with
	 * @param submissionVersion
	 */
	public void setSubmissionVersion(AssignmentSubmissionVersion submissionVersion) {
		this.submissionVersion = submissionVersion;
	}

	/**
	 * 
	 * @return the reference to this feedback attachment
	 */
	public String getAttachmentReference() {
		return attachmentReference;
	}

	/**
	 * set the reference to this feedback attachment
	 * @param attachmentReference
	 */
	public void setAttachmentReference(String attachmentReference) {
		this.attachmentReference = attachmentReference;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (null == obj) return false;
		if (!(obj instanceof AssignmentAttachment)) return false;
		else {
			AssignmentFeedbackAttachment compAttach = (AssignmentFeedbackAttachment) obj;
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
