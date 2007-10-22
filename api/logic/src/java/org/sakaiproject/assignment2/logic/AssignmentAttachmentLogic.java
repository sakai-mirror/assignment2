/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/api/logic/src/java/org/sakaiproject/assignment2/dao/AssignmentDao.java $
 * $Id: AssignmentDao.java 12544 2006-05-03 15:06:26Z wagnermr@iupui.edu $
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

package org.sakaiproject.assignment2.logic;

import org.sakaiproject.assignment2.model.AssignmentAttachment;


/**
 * This is the interface for the AssignmentAttachment object
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public interface AssignmentAttachmentLogic {
	
	/**
	 * 
	 * @param assignmentAttachmentId
	 * @return the AssignmentAttachment based upon its assignmentAttachmentId
	 */
	public AssignmentAttachment getAssignmentAttachmentById(Long assignmentAttachmentId);
	
	/**
	 * Create or update an AssignmentAttachment
	 * @param assignment
	 * 			the AssignmentAttachment to create or update
	 */
	public void saveAssignmentAttachment(AssignmentAttachment assignmentAttachment);
	
	/**
	 * Delete an AssignmentAttachment
	 * @param assignmentAttachment
	 * 			the AssignmentAttachment to delete
	 */	
	public void deleteAssignmentAttachment(AssignmentAttachment assignmentAttachment);

}
