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

import java.util.List;

import org.sakaiproject.assignment2.logic.entity.AssignmentDefinition;


//import org.sakaiproject.assignment2.logic.entity.AssignmentDefinition;

/**
 * This is the interface for importing and exporting data in the Assignments tool
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public interface ImportExportLogic {
	
	/**
	 * Get an archivable definition of assignment2 data suitable for migration
	 * between sites. The AssignmentToolDefinition includes the AssignmentDefinitions
	 * representing the assignments defined for the given contextId. Will not
	 * include student-specific information such as submissions or feedback.
	 * @param contextId
	 * @return a versioned XML string
	 */
	public String getAssignmentToolDefinitionXML(String contextId);
	
	/**
	 * 
	 * @param contextId
	 * @return a list of the AssignmentDefinitions that correspond to the assignments
	 * in the given context (does not include removed assignments).
	 */
	public List<AssignmentDefinition> getAssignmentDefinitionsInContext(String contextId);
	
	/**
	 * Given the xml archive of the assignments tool in another site, merge
	 * this new data into the assignments tool in the given toContext.
	 * If assignment with the same title already exists, will append _1
	 * to the new assignment title. Announcement will be added in the new
	 * site if imported assignment had an announcement. Attachments will be
	 * copied.  If a group in the new site has the same name as the group an
	 * assignment was restricted to, we will restrict the assignment to the
	 * group with this same name in the site with the given context.
	 * @param toContext
	 * @param fromAssignmentToolXml
	 */
	public void mergeAssignmentToolDefinitionXml(String toContext, String fromAssignmentToolXml);
	
	/**
	 * Will convert data from the original Assignments tool in the given fromContext
	 * into an AssignmentToolDefintion (with associated AssignmentDefinitions) suitable
	 * for importing into the new assignments tool. Returns string representation of
	 * the versioned xml
	 * @param fromContext
	 * @param toContext
	 * @return a versioned xml string
	 */
	public String getAssignmentToolDefinitionXmlFromOriginalAssignmentsTool(
			String fromContext, String toContext);
	
}
