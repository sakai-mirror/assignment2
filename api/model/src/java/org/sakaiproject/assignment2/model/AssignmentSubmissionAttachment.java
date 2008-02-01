/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/api/model/src/java/org/sakaiproject/assignment2/model/AssignmentSubmissionAttachment.java $
 * $Id: AssignmentSubmissionAttachment.java 12544 2006-05-03 15:06:26Z wagnermr@iupui.edu $
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

	private Long id;
	private AssignmentSubmissionVersion submissionVersion;
	private String attachmentReference;
	
	public AssignmentSubmissionAttachment() {
		
	}
	
	public AssignmentSubmissionAttachment(AssignmentSubmissionVersion submissionVersion, String attachmentReference) {
		this.submissionVersion = submissionVersion;
		this.attachmentReference = attachmentReference;
	}

	/**
	 * 
	 * @return the id for this submission attachment
	 */
	public Long getId() {
		return id;
	}

	/**
	 * set the sid for this submission attachment
	 * @param id
	 */
	public void setId(Long id) {
		this.id = id;
	}

	/**
	 * 
	 * @return the AssignmentSubmissionVersion rec associated with this attachment
	 */
	public AssignmentSubmissionVersion getSubmissionVersion() {
		return submissionVersion;
	}

	/**
	 * set the AssignmentSubmissionVersion rec associated with this attachment
	 * @param submissionVersion
	 */
	public void setSubmissionVersion(AssignmentSubmissionVersion submissionVersion) {
		this.submissionVersion = submissionVersion;
	}

	/**
	 * 
	 * @return the reference to this submission attachment
	 */
	public String getAttachmentReference() {
		return attachmentReference;
	}

	/**
	 * set the reference to this submission attachment
	 * @param attachmentReference
	 */
	public void setAttachmentReference(String attachmentReference) {
		this.attachmentReference = attachmentReference;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (null == obj) return false;
		if (!(obj instanceof AssignmentSubmissionAttachment)) return false;
		else {
			AssignmentSubmissionAttachment compAttach = (AssignmentSubmissionAttachment) obj;
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
