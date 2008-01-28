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
import java.util.Iterator;
import java.util.Collection;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.Query;

import org.sakaiproject.assignment2.dao.AssignmentDao;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
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
	
	
	// Assignment Submissions DAO
    
    public List<AssignmentSubmission> findCurrentSubmissionsForAssignment(Assignment2 assignment) {
    	if (assignment == null) {
    		throw new IllegalArgumentException("Null assignment passed to findCurrentSubmissionsForAssignment");
    	}
    	Query query = getSession().getNamedQuery("findCurrentSubmissionsForAssignment");
    	query.setParameter("assignment", assignment);
    	
    	return query.list();
    }
    
    public AssignmentSubmissionVersion getCurrentSubmissionVersionWithAttachments(AssignmentSubmission submission) {
    	if (submission == null || submission.getId() == null) {
    		throw new IllegalArgumentException("null or transient submission passed to getSubmissionVersionForUserIdWithAttachments");
    	}
    	
    	AssignmentSubmissionVersion currentVersion = null;
	
    	String queryString = "select max(submissionVersion.id) " +
		"from AssignmentSubmissionVersion as submissionVersion " +
		"where submissionVersion.assignmentSubmission = :submission";
    	
    	Query query = getSession().createQuery(queryString);
    	query.setParameter("submission", submission);
    	
    	Long submissionVersionId = (Long) query.uniqueResult();
    	
    	if (submissionVersionId != null) {
    		currentVersion = getAssignmentSubmissionVersionByIdWithAttachments(submissionVersionId);
    	}
    	
    	return currentVersion;
    }
    
    private List<Long> getCurrentVersionIdsForSubmissions(Collection<AssignmentSubmission> submissions) {    	
    	List<Long> versionIdList = new ArrayList();

    	if (submissions != null && !submissions.isEmpty()) {
    		
    		List submissionList = new ArrayList(submissions);
    		
    		Query query = getSession().getNamedQuery("findCurrentVersionIds");

    		// if submission list is > than the max length allowed in sql, we need
    		// to cycle through the list

    		versionIdList = queryWithParameterList(query, "submissionList", submissionList);
    	}

    	return versionIdList;
    }
    
    public List<AssignmentSubmission> getCurrentAssignmentSubmissionsForStudent(List<Assignment2> assignments, String studentId) {
		if (studentId == null) {
			throw new IllegalArgumentException("null studentId passed to getAllSubmissionRecsForStudentWithVersionData");
		}
		
		List<AssignmentSubmission> submissions = new ArrayList();
		
		if (assignments != null && !assignments.isEmpty()) {
			// retrieve the submissions
			Query query = getSession().getNamedQuery("findSubmissionsForStudentForAssignments");
	    	query.setParameter("studentId",studentId);
    	
	    	submissions = queryWithParameterList(query, "assignmentList", assignments);
	    	
	    	// now, populate the version information
    		populateCurrentVersion(submissions);
		}
		
		return submissions;
    }
    
    public AssignmentSubmissionVersion getAssignmentSubmissionVersionByIdWithAttachments(Long submissionVersionId) {
    	if (submissionVersionId == null) {
    		throw new IllegalArgumentException("Null submissionVersionId passed to getAssignmentSubmissionVersionByIdWithAttachments");
    	}
    	
    	Query query = getSession().getNamedQuery("findSubmissionVersionByIdWithAttachments");
    	query.setParameter("submissionVersionId", submissionVersionId);
    	
    	return (AssignmentSubmissionVersion) query.uniqueResult();
    }
    
    private List<AssignmentSubmissionVersion> getAssignmentSubmissionVersionsById(List<Long> versionIds) {
    	List<AssignmentSubmissionVersion> versions = new ArrayList();
    	
    	if (versionIds != null && !versionIds.isEmpty()) {
    		String hql = "from AssignmentSubmissionVersion as version where version.id in (:versionIdList)";
    		Query query = getSession().createQuery(hql);
    		
    		versions = queryWithParameterList(query, "versionIdList", versionIds);
    	}
    	
    	return versions;
    }
    
    public Set<AssignmentSubmission> getCurrentSubmissionsForStudentsForAssignment(List<String> studentIds, Assignment2 assignment) {
    	if (assignment == null) {
    		throw new IllegalArgumentException("null assignment passed to getSubmissionsForStudentsForAssignment");    		
    	}
    	
    	Set<AssignmentSubmission> submissionSet = new HashSet();
    	
    	if (studentIds != null && !studentIds.isEmpty()) {
    		Query query = getSession().getNamedQuery("findSubmissionsForStudentsForAssignment");
    		query.setParameter("assignment", assignment);
    		
    		List<AssignmentSubmission> submissionList = queryWithParameterList(query, "studentIdList", studentIds);
    			
    		if (submissionList != null) {
    			submissionSet = new HashSet(submissionList);
    			
        		// now retrieve the current version information
        		populateCurrentVersion(submissionSet);
    		}
    	}
    	
    	return submissionSet;
    }
    
    private void populateCurrentVersion(Collection<AssignmentSubmission> submissions) {
    	if (submissions != null && !submissions.isEmpty()) {
			// then, we will populate the version data
			
			// first, retrieve the ids of the current versions
			List<Long> versionIds = getCurrentVersionIdsForSubmissions(submissions);
			
			// now retrieve the associated AssignmentSubmissionVersion recs
			List<AssignmentSubmissionVersion> currentVersions = getAssignmentSubmissionVersionsById(versionIds);
			
			if (currentVersions != null) {
				Map submissionIdVersionMap = new HashMap();
				for (Iterator versionIter = currentVersions.iterator(); versionIter.hasNext();) {
					AssignmentSubmissionVersion version = (AssignmentSubmissionVersion) versionIter.next();
					if (version != null) {
						submissionIdVersionMap.put(version.getAssignmentSubmission().getId(), version);
					}
				}
				
				for (Iterator submissionIter = submissions.iterator(); submissionIter.hasNext();) {
					AssignmentSubmission submission = (AssignmentSubmission) submissionIter.next();
					if (submission != null) {
						AssignmentSubmissionVersion currVersion = 
							(AssignmentSubmissionVersion)submissionIdVersionMap.get(submission.getId());
						if (currVersion != null) {
							submission.setCurrentSubmissionVersion(currVersion);
						}
					}
				}
			}
			
		}
    }
    
    public List<AssignmentSubmission> getSubmissionsWithVersionHistoryForStudentListAndAssignment(List<String> studentIdList, Assignment2 assignment) {
    	if (assignment == null) {
    		throw new IllegalArgumentException("null assignment passed to getSubmissionsWithVersionHistoryForStudentListAndAssignment");
    	}
    	
    	List<AssignmentSubmission> submissionList = new ArrayList();
    	
    	if (studentIdList != null && !studentIdList.isEmpty()) {
    		Query query = getSession().getNamedQuery("findSubmissionsWithHistoryForAssignmentAndStudents");	
        	query.setParameter("assignment", assignment);
        	
        	submissionList = queryWithParameterList(query, "studentIdList", studentIdList);
    	}
    	
    	return submissionList;
    }
    
    public AssignmentSubmission getSubmissionWithVersionHistoryForStudentAndAssignment(String studentId, Assignment2 assignment) {
    	if (studentId == null || assignment == null) {
    		throw new IllegalArgumentException("null parameter passed to getSubmissionWithVersionHistoryForStudentAndAssignment");
    	}
    	
    	Query query = getSession().getNamedQuery("findStudentSubmissionWithHistoryForAssignment");
    	query.setParameter("studentId", studentId);
    	query.setParameter("assignment", assignment);
    	
    	AssignmentSubmission submission = (AssignmentSubmission) query.uniqueResult();
    	
    	if (submission != null) {
    		setCurrentSubmissionGivenHistory(submission);
    	}
    	
    	return submission;
    }
    
    /**
     * given a submission with populated version history, sets the currentVersion to
     * the version with the highest id 
     * @param submission
     */
    private void setCurrentSubmissionGivenHistory(AssignmentSubmission submission) {
    	if (submission != null && submission.getSubmissionHistorySet() != null) {
    		Map versionIdVersionMap = new HashMap();
    		Long maxVersionId = new Long(-1);
    		for (Iterator versionIter = submission.getSubmissionHistorySet().iterator(); versionIter.hasNext();) {
    			AssignmentSubmissionVersion version = (AssignmentSubmissionVersion) versionIter.next();
    			if (version != null) {
    				versionIdVersionMap.put(version.getId(), version);
    				if (version.getId() > maxVersionId) {
    					maxVersionId = version.getId();
    				}
    			}
    		}
    		
    		if (maxVersionId.longValue() > -1) {
    			submission.setCurrentSubmissionVersion((AssignmentSubmissionVersion)versionIdVersionMap.get(maxVersionId));
    		}
    	}
    }
    
    public AssignmentSubmission getSubmissionWithVersionHistoryById(Long submissionId) {
    	if (submissionId == null) {
    		throw new IllegalArgumentException("null submissionId passed to getSubmissionWithVersionHistoryById");
    	}
    	
    	Query query = getSession().getNamedQuery("findSubmissionByIdWithHistory");
    	query.setParameter("submissionId", submissionId);
    	
    	AssignmentSubmission submission = (AssignmentSubmission) query.uniqueResult();
    	
    	if (submission != null) {
    		setCurrentSubmissionGivenHistory(submission);
    	}
    	
    	return submission;
    }
    
    /**
     * 
     * @param query - your query with all other parameters already defined
     * @param queryParamName - the name of the list parameter referenced in the query
     * @param fullList - the list that you are using as a parameter
     * @return the resulting list from a query that takes in a list as a parameter;
     * this will cycle through with sublists if the size of the list exceeds the
     * allowed size for an sql query
     */
    private List queryWithParameterList(Query query, String queryParamName, List fullList) {
    	// sql has a limit for the size of a parameter list, so we may need to cycle
		// through with sublists
    	List queryResultList = new ArrayList();
    	
		if (fullList.size() < MAX_NUMBER_OF_SQL_PARAMETERS_IN_LIST) {
			query.setParameterList(queryParamName, fullList);
			queryResultList = query.list();
    		
		} else {
			// if there are more than MAX_NUMBER_OF_SQL_PARAMETERS_IN_LIST, we need to do multiple queries
			int begIndex = 0;
			int endIndex = 0;

			while (begIndex < fullList.size()) {
				endIndex = begIndex + MAX_NUMBER_OF_SQL_PARAMETERS_IN_LIST;
				if (endIndex > fullList.size()) {
					endIndex = fullList.size();
				}
				List tempSubList = new ArrayList();
				tempSubList.addAll(fullList.subList(begIndex, endIndex));
				
				query.setParameterList(queryParamName, tempSubList);
				
				queryResultList.addAll(query.list());
				begIndex = endIndex;
			}
		}
		
		return queryResultList;
    }

}
