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

package org.sakaiproject.assignment2.logic.impl;

import java.util.List;
import java.util.ArrayList;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.dao.AssignmentDao;
import org.sakaiproject.genericdao.api.finders.ByPropsFinder;


/**
 * This is the interface for the Assignment object
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class AssignmentLogicImpl implements AssignmentLogic{
	
	private static Log log = LogFactory.getLog(AssignmentLogicImpl.class);
	
	private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }
    
    private Assignment2 dao;
    public void setDao(Assignment2 dao) {
        this.dao = dao;
    }
    
	public void init(){
		log.debug("init");
	}
	/**
	 * 
	 * @param assignmentId
	 * @return Returns the Assignment based on its assignmentId
	 */
	public Assignment2 getAssignmentById(Long assignmentId)
	{
		return new Assignment2();
	}
	
	/**
	 * Create or update an Assignment
	 * @param assignment
	 * 			the Assignment to create or update
	 */
	public void saveAssignment(Assignment2 assignment)
	{
		
		
	}
	
	/**
	 * Delete an Assignment
	 * @param assignment
	 * 			the Assignment to delete
	 */	
	public void deleteAssignment(Assignment2 assignment)
	{
		
	}
	
	/**
	 * Returns list of Assignment objects that the given user has permission
	 * to view or grade. Assignments that the user does not have permission 
	 * to view or grade will not be returned.
	 * @param userId
	 * @return
	 */
	public List<Assignment2> getViewableAssignments(String userId)
	{
		List results = new ArrayList();
		return results;
	}

}
