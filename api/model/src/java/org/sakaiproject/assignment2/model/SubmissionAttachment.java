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
* The SubmissionAttachment object.  SubmissionAttachments 
* are attachments associated with the submitter's submission.
* 
* @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
*/
public class SubmissionAttachment extends SubmissionAttachmentBase {
	
	public SubmissionAttachment() {
		
	}
	
	public SubmissionAttachment(AssignmentSubmissionVersion submissionVersion, String attachmentReference) {
		this.submissionVersion = submissionVersion;
		this.attachmentReference = attachmentReference;
	}

	
	public static SubmissionAttachment deepCopy(SubmissionAttachment attachToCopy) {
		SubmissionAttachment attach = new SubmissionAttachment();
		attach.setAttachmentReference(attachToCopy.getAttachmentReference());
		attach.setId(attachToCopy.getId());
		
		return attach;
	}
}
