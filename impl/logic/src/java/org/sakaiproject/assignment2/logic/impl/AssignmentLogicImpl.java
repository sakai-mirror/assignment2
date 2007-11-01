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
import org.sakaiproject.assignment2.exception.ConflictingAssignmentNameException;
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
    
    private AssignmentDao dao;
    public void setDao(AssignmentDao dao) {
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
		return (Assignment2) dao.findById(Assignment2.class, assignmentId);
    }
	
	/*
	 * (non-Javadoc)
	 * @see org.sakaiproject.assignment2.logic.AssignmentLogic#saveAssignment(org.sakaiproject.assignment2.model.Assignment2)
	 */
	public void saveAssignment(Assignment2 assignment) throws SecurityException, IllegalArgumentException, ConflictingAssignmentNameException
	{
		if (assignment == null) {
			throw new IllegalArgumentException("Null assignment passed to saveAssignment");
		}
		boolean isNewAssignment = true;
		Assignment2 existingAssignment = null;
		
		// determine if this is a new assignment
		if (assignment.getAssignmentId() != null) {
			// check to see if assignment exists
			existingAssignment = (Assignment2)dao.findById(Assignment2.class, assignment.getAssignmentId());	
			if (existingAssignment != null) {
				isNewAssignment = false;
			}
		}
		
		if (isNewAssignment) {
	        // the user must have the "create" permission
	        if (externalLogic.getCurrentUserHasPermission(ExternalLogic.ASSIGNMENT2_CREATE)) {
	        	// check to ensure it is not a duplicate title
	        	if (assignmentNameExists(assignment.getTitle())) {
	        		throw new ConflictingAssignmentNameException("An assignment with the title " + assignment.getTitle() + " already exists");
	        	}
	        	dao.create(assignment);
	            log.info("Created assignment: " + assignment.getTitle());
	        } else {
	            throw new SecurityException("Current user may not create assignment " + assignment.getTitle()
	                    + " because they do not have create permission");
	        }
		} else {
			if (externalLogic.getCurrentUserHasPermission(ExternalLogic.ASSIGNMENT2_REVISE)) {
				if (!assignment.getTitle().equals(existingAssignment.getTitle())) {
					// check to see if this new title already exists
					if (assignmentNameExists(assignment.getTitle())) {
		        		throw new ConflictingAssignmentNameException("An assignment with the title " + assignment.getTitle() + " already exists");
		        	}
				}
	        	dao.update(assignment);
	            log.info("Updated assignment: " + assignment.getTitle() + "with id: " + assignment.getAssignmentId());
	        } else {
	            throw new SecurityException("Current user may not udpate assignment " + assignment.getTitle()
	                    + " because they do not have revise permission");
	        }
		}
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sakaiproject.assignment2.logic.AssignmentLogic#deleteAssignment(org.sakaiproject.assignment2.model.Assignment2)
	 */
	public void deleteAssignment(Assignment2 assignment) throws SecurityException, IllegalArgumentException
	{
		if (assignment == null) {
			throw new IllegalArgumentException("Null assignment passed to deleteAssignment");
		}
		
		if (externalLogic.getCurrentUserHasPermission(ExternalLogic.ASSIGNMENT2_DELETE)) {
        	assignment.setRemoved(true);
        	dao.update(assignment);
            log.info("Deleted assignment: " + assignment.getTitle() + " with id " + assignment.getAssignmentId());
        } else {
            throw new SecurityException("Current user may not delete assignment " + assignment.getTitle()
                    + " because they do not have delete permission");
        }
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sakaiproject.assignment2.logic.AssignmentLogic#getViewableAssignments(String)
	 */
	public List<Assignment2> getViewableAssignments(String userId)
	{
		List<Assignment2> assignments = 
			dao.findByProperties(Assignment2.class, new String[] {"siteId", "removed"}, new Object[] {externalLogic.getCurrentLocationId(), Boolean.FALSE});
		
		return assignments;
	}
	
	public List<Assignment2> getViewableAssignments(String userId, String sortProperty, boolean ascending, int start, int limit) {
		
		if (!ascending) {
            sortProperty += ByPropsFinder.DESC;
        }
		List<Assignment2> assignments = 
			dao.findByProperties(Assignment2.class, new String[] {"siteId", "removed"}, new Object[] {externalLogic.getCurrentLocationId(), Boolean.FALSE},
					new int[] { ByPropsFinder.EQUALS, ByPropsFinder.EQUALS }, new String[] { sortProperty }, start, limit);
		return assignments;
	}
	
	public void setAssignmentSortIndexes(Long[] assignmentIds)
	{
		for (int i=0; i < assignmentIds.length; i++){
    		Assignment2 assignment = getAssignmentById(assignmentIds[i]);
    		if (assignment != null){
    			if (assignment.getSortIndex() != i){
	    			assignment.setSortIndex(i);
	    			saveAssignment(assignment);
    			}
    		}
    	}
	}
	
	/**
	 * 
	 * @param assignmentName
	 * @return true if there is an existing assignment (removed = false) with
	 * the given title
	 */
	private boolean assignmentNameExists(String assignmentName) {
		int count = dao.countByProperties(Assignment2.class, 
	               new String[] {"siteId", "title", "removed"}, 
	               new Object[] {externalLogic.getCurrentLocationId(), assignmentName, Boolean.FALSE});
		
		return count > 0;
	}

}
