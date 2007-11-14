/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/impl/logic/src/java/org/sakaiproject/assignment2/dao/AssignmentDaoImpl.java $
 * $Id: AssignmentDaoImpl.java 12544 2006-05-03 15:06:26Z wagnermr@iupui.edu $
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

package org.sakaiproject.assignment2.dao.impl;

import java.lang.Integer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.Query;

import org.sakaiproject.assignment2.dao.AssignmentDao;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.genericdao.hibernate.HibernateCompleteGenericDao;

/**
 * Implementations of any specialized DAO methods from the specialized DAO that allows the developer to extend the functionality of the
 * generic dao package
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class AssignmentDaoImpl extends HibernateCompleteGenericDao implements AssignmentDao {

    private static Log log = LogFactory.getLog(AssignmentDaoImpl.class);

    public void init() {
        log.debug("init");
    }
    
    public Integer getHighestSortIndexInSite(String contextId) {
    	String hql = "select max(assignment.sortIndex) from Assignment2 as assignment where assignment.contextId = :contextId and assignment.removed != true";
    	
    	Query query = getSession().createQuery(hql);
    	query.setParameter("contextId", contextId);

        return (Integer)query.uniqueResult();
    }
    
    public List<Assignment2> getAssignmentsWithGroups(String contextId) {
    	if (contextId == null) {
    		throw new IllegalArgumentException("Null contextId passed to getAssignmentsWithGroups");
    	}
    	Query query = getSession().getNamedQuery("findAssignmentsWithGroups");
    	query.setParameter("contextId", contextId);
    	
    	return (List)query.list();
    }
    
    public Assignment2 getAssignmentByIdWithGroupsAndAttachments(Long assignmentId) {
    	if (assignmentId == null) {
    		throw new IllegalArgumentException("Null assignmentId passed to getAssignmentByIdWithGroupsAndAttachments");
    	}
    	Query query = getSession().getNamedQuery("getAssignmentWithGroupsAndAttachments");
    	query.setParameter("assignmentId",assignmentId);
    	
    	return (Assignment2) query.uniqueResult();
    }
    
    public List<AssignmentSubmission> findCurrentSubmissionsForAssignment(Assignment2 assignment) {
    	if (assignment == null) {
    		throw new IllegalArgumentException("Null assignment passed to findCurrentSubmissionsForAssignment");
    	}
    	Query query = getSession().getNamedQuery("findCurrentSubmissionsForAssignment");
    	query.setParameter("assignment", assignment);
    	
    	return (List)query.list();
    }
    
    public AssignmentSubmission findSubmissionForAssignmentAndUserWithAttachments(Assignment2 assignment, String userId) {
    	if (assignment == null || userId == null) {
    		throw new IllegalArgumentException("null assignment or userId passed to findSubmissionForAssignmentAndUserWithAttachments");
    	}
    	
    	Query query = getSession().getNamedQuery("findSubmissionForAssignmentAndUserWithAttachments");
    	query.setParameter("assignment", assignment);
    	query.setParameter("userId", userId);
    	
    	return (AssignmentSubmission) query.uniqueResult();
    }

}
