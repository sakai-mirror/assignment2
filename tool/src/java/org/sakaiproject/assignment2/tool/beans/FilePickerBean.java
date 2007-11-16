/**********************************************************************************
 *
 * Copyright (c) 2005, 2006 The Sakai Foundation.
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

package org.sakaiproject.assignment2.tool.beans;

import java.util.List;

import org.sakaiproject.assignment2.logic.AssignmentLogic;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.content.api.ContentHostingService;
import org.sakaiproject.content.api.FilePickerHelper;
import org.sakaiproject.entity.api.Reference;
import org.sakaiproject.exception.IdUnusedException;
import org.sakaiproject.exception.PermissionException;
import org.sakaiproject.exception.TypeException;
import org.sakaiproject.tool.api.SessionManager;
import org.sakaiproject.tool.api.ToolSession;

/**
 * This is the backing bean of the XML data import process.
 * 
 */
public class FilePickerBean {
		
	//injection
	private SessionManager sessionManager;
	public void setSessionManager(SessionManager sessionManager) {
		this.sessionManager = sessionManager;
	}

	private ContentHostingService contentHostingService;
	public void setContentHostingService(ContentHostingService contentHostingService) {
		this.contentHostingService = contentHostingService;
	}
	
	private AssignmentLogic assignmentLogic;
	public void setAssignmentLogic (AssignmentLogic assignmentLogic) {
		this.assignmentLogic = assignmentLogic;
	}
	
	public String assignment_id;
	
	/**
	 * Parse and load selected XML data file
	 * 
	 * @return String that is used to determine the place where control is to be sent
	 * 			in ControlImportProducer (reportNavigationCases method)
	 * @throws SecurityException 
	 */
	public String process() throws SecurityException {
/***		    
		    ToolSession session = sessionManager.getCurrentToolSession();
		    if (session.getAttribute(FilePickerHelper.FILE_PICKER_CANCEL) == null &&
		        session.getAttribute(FilePickerHelper.FILE_PICKER_ATTACHMENTS) != null) 
		    {
		      List refs = (List)session.getAttribute(FilePickerHelper.FILE_PICKER_ATTACHMENTS);
		      for (int i = 0; i < refs.size(); i++) {
		        Reference ref = (Reference) refs.get(i);
		        SyllabusAttachment thisAttach = syllabusManager.createSyllabusAttachmentObject(
		          ref.getId(), ref.getProperties().getProperty(ref.getProperties().getNamePropDisplayName()));
		        attachments.addAttachmentToSyllabii(syllabusdataid, thisAttach);
		      }
		      
		    }
		    session.removeAttribute(FilePickerHelper.FILE_PICKER_ATTACHMENTS);
		    session.removeAttribute(FilePickerHelper.FILE_PICKER_CANCEL);
**/
		    return "processed";

	}

}

