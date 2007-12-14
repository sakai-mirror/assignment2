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
import java.util.HashSet;
import java.util.Set;

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
    
    public static final int MAX_NUMBER_OF_SQL_PARAMETERS_IN_LIST = 1000;

    public void init() {
        log.debug("init");
    }
    
    public Integer getHighestSortIndexInSite(String contextId) {
    	if (contextId == null) {
    		throw new IllegalArgumentException("contextId cannot be null");
    	}
    	String hql = "select max(assignment.sortIndex) from Assignment2 as assignment where assignment.contextId = :contextId and assignment.removed != true";
    	
    	Query query = getSession().createQuery(hql);
    	query.setParameter("contextId", contextId);
    	
    	Integer highestIndex = (Integer)query.uniqueResult();
    	if (highestIndex == null) {
    		highestIndex = 0;
    	}

        return highestIndex; 
    }
    
    public Set<Assignment2> getAssignmentsWithGroupsAndAttachments(String contextId) {
    	if (contextId == null) {
    		throw new IllegalArgumentException("Null contextId passed to getAssignmentsWithGroupsAndAttachments");
    	}
    	Query query = getSession().getNamedQuery("findAssignmentsWithGroupsAndAttachments");
    	query.setParameter("contextId", contextId);
    	
    	List<Assignment2> assignmentList = query.list();
    	
    	Set<Assignment2> assignmentSet = new HashSet();
    	
    	if (assignmentList != null) {
    		assignmentSet = new HashSet(assignmentList);
    	}
    	
    	return assignmentSet;
    }
    
    public Assignment2 getAssignmentByIdWithGroupsAndAttachments(Long assignmentId) {
    	if (assignmentId == null) {
    		throw new IllegalArgumentException("Null assignmentId passed to getAssignmentByIdWithGroupsAndAttachments");
    	}
    	Query query = getSession().getNamedQuery("findAssignmentByIdWithGroupsAndAttachments");
    	query.setParameter("assignmentId",assignmentId);
    	
    	return (Assignment2) query.uniqueResult();
    }

	public Assignment2 getAssignmentByIdWithGroups(Long assignmentId) {
		if (assignmentId == null) {
    		throw new IllegalArgumentException("Null assignmentId passed to getAssignmentByIdWithGroups");
    	}
    	Query query = getSession().getNamedQuery("findAssignmentByIdWithGroups");
    	query.setParameter("assignmentId",assignmentId);
    	
    	return (Assignment2) query.uniqueResult();
	}
    
    public List<AssignmentSubmission> findCurrentSubmissionsForAssignment(Assignment2 assignment) {
    	if (assignment == null) {
    		throw new IllegalArgumentException("Null assignment passed to findCurrentSubmissionsForAssignment");
    	}
    	Query query = getSession().getNamedQuery("findCurrentSubmissionsForAssignment");
    	query.setParameter("assignment", assignment);
    	
    	return query.list();
    }
    
    public AssignmentSubmission getSubmissionVersionForUserIdAndAssignmentWithAttachments(Assignment2 assignment, String userId) {
    	if (assignment == null || userId == null) {
    		throw new IllegalArgumentException("null assignment or userId passed to findSubmissionForAssignmentAndUserWithAttachments");
    	}
    	
    	String hqlGetVersionNoDraft = "select max(subVersion.submissionVersionId) " +
		"from AssignmentSubmissionVersion as subVersion " +
		"where subVersion.submissionId in :submissionIdList " +
		"and subVersion.draft = false group by subVersion.submissionId";
	
    	String hqlGetVersionWithDraft = "select max(subVersion.submissionVersionId) " +
    	"from AssignmentSubmissionVersion as subVersion " +
		"where subVersion.submissionId in :submissionIdList " +
		"group by subVersion.submissionId";
    	
    	Query query = getSession().getNamedQuery("findSubmissionForAssignmentAndUserWithAttachments");
    	query.setParameter("assignment", assignment);
    	query.setParameter("userId", userId);
    	
    	return (AssignmentSubmission) query.uniqueResult();
    }
    
    public List<AssignmentSubmission> getAssignmentSubmissionsWithCurrentVersionDataWithAttach(List<Long> submissionIdList, boolean includeDraft) {
    	if (submissionIdList == null || submissionIdList.isEmpty()) {
    		return new ArrayList();
    	}
    	
    	List versionIdList = new ArrayList();
    	
    	String hqlGetVersionIdsNoDrafts = "select max(subVersion.submissionVersionId) " +
    		"from AssignmentSubmissionVersion as subVersion " +
    		"where subVersion.submissionId in :submissionIdList " +
    		"and subVersion.draft = false group by subVersion.submissionId";
    	
    	String hqlGetVersionIdsWithDrafts = "select max(subVersion.submissionVersionId) " +
	    	"from AssignmentSubmissionVersion as subVersion " +
			"where subVersion.submissionId in :submissionIdList " +
			"group by subVersion.submissionId";
    	
    	// if submissionId list is > than the max length allowed in sql, we need
    	// to cycle through the list
    	if (submissionIdList.size() > MAX_NUMBER_OF_SQL_PARAMETERS_IN_LIST) {
    		
    	} else {
    		
    	}
    	
    	return versionIdList;
    	

    }
    
    public List <AssignmentSubmission> getAssignmentSubmissionsWithCurrentVersionDataNoAttach(List<Long> submissionIdList, boolean includeDraft) {
    	return null;
    }

}
