/**********************************************************************************
 * $URL$
 * $Id$
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

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.Hibernate;
import org.hibernate.HibernateException;
import org.hibernate.Query;
import org.hibernate.Session;
import org.sakaiproject.assignment2.dao.AssignmentDao;
import org.sakaiproject.assignment2.exception.AssignmentNotFoundException;
import org.sakaiproject.assignment2.exception.SubmissionNotFoundException;
import org.sakaiproject.assignment2.exception.VersionNotFoundException;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.genericdao.hibernate.HibernateGeneralGenericDao;
import org.springframework.orm.hibernate3.HibernateCallback;

/**
 * Implementations of any specialized DAO methods from the specialized DAO that allows the developer to extend the functionality of the
 * generic dao package
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class AssignmentDaoImpl extends HibernateGeneralGenericDao implements AssignmentDao {

    private static Log log = LogFactory.getLog(AssignmentDaoImpl.class);
    
    public static final int MAX_NUMBER_OF_SQL_PARAMETERS_IN_LIST = 1000;

    public void init() {
    	if (log.isDebugEnabled()) log.debug("init");
    }
    
    public Integer getHighestSortIndexInSite(final String contextId) {
    	if (contextId == null) {
    		throw new IllegalArgumentException("contextId cannot be null");
    	}
    	
    	HibernateCallback hc = new HibernateCallback() {
			public Object doInHibernate(Session session) throws HibernateException ,SQLException {
				String hql = "select max(assignment.sortIndex) from Assignment2 as assignment where assignment.contextId = :contextId and assignment.removed != true";
		    	
		    	Query query = session.createQuery(hql);
		    	query.setParameter("contextId", contextId);
		    	
		    	Integer highestIndex = (Integer)query.uniqueResult();
		    	if (highestIndex == null) {
		    		highestIndex = 0;
		    	}

		        return highestIndex; 
			}
		};
		
		return (Integer)getHibernateTemplate().execute(hc);
    	
    }
    
    public Set<Assignment2> getAssignmentsWithGroupsAndAttachments(final String contextId) {
    	if (contextId == null) {
    		throw new IllegalArgumentException("Null contextId passed to getAssignmentsWithGroupsAndAttachments");
    	}
    	
    	HibernateCallback hc = new HibernateCallback() {
			public Object doInHibernate(Session session) throws HibernateException ,SQLException {
				Query query = session.getNamedQuery("findAssignmentsWithGroupsAndAttachments");
		    	query.setParameter("contextId", contextId);
		    	
		    	List<Assignment2> assignmentList = query.list();
		    	
		    	Set<Assignment2> assignmentSet = new HashSet<Assignment2>();
		    	
		    	if (assignmentList != null) {
		    		assignmentSet = new HashSet<Assignment2>(assignmentList);
		    	}
		    	
		    	return assignmentSet;
			}
		};
    	
		return (Set<Assignment2>)getHibernateTemplate().execute(hc);
    }
    
    public Assignment2 getAssignmentByIdWithGroupsAndAttachments(final Long assignmentId) {
    	if (assignmentId == null) {
    		throw new IllegalArgumentException("Null assignmentId passed to getAssignmentByIdWithGroupsAndAttachments");
    	}
    	
    	HibernateCallback hc = new HibernateCallback() {
			public Object doInHibernate(Session session) throws HibernateException ,SQLException {
				Query query = session.getNamedQuery("findAssignmentByIdWithGroupsAndAttachments");
		    	query.setParameter("assignmentId",assignmentId);
		    	
		    	return (Assignment2) query.uniqueResult();
			}
		};
		
		Assignment2 assign = (Assignment2)getHibernateTemplate().execute(hc);
		if (assign == null) {
			throw new AssignmentNotFoundException("No assignment exists with id: " + assignmentId);
		}
		
		return assign;
    }

	public Assignment2 getAssignmentByIdWithGroups(final Long assignmentId) {
		if (assignmentId == null) {
    		throw new IllegalArgumentException("Null assignmentId passed to getAssignmentByIdWithGroups");
    	}
		
		HibernateCallback hc = new HibernateCallback() {
			public Object doInHibernate(Session session) throws HibernateException ,SQLException {
				Query query = session.getNamedQuery("findAssignmentByIdWithGroups");
		    	query.setParameter("assignmentId",assignmentId);
		    	
		    	return (Assignment2) query.uniqueResult();
			}
		};
		
		Assignment2 assign = (Assignment2)getHibernateTemplate().execute(hc);
		if (assign == null) {
			throw new AssignmentNotFoundException("No assignment exists with id: " + assignmentId);
		}
		
		return assign;
	}
	
	
	// Assignment Submissions DAO
    
    public AssignmentSubmissionVersion getCurrentSubmissionVersionWithAttachments(final AssignmentSubmission submission) {
    	if (submission == null || submission.getId() == null) {
    		throw new IllegalArgumentException("null submission or submission w/o id passed to getSubmissionVersionForUserIdWithAttachments");
    	}
    	
    	HibernateCallback hc = new HibernateCallback() {
			public Object doInHibernate(Session session) throws HibernateException ,SQLException {
				AssignmentSubmissionVersion currentVersion = null;
				
		    	String queryString = "select max(submissionVersion.id) " +
				"from AssignmentSubmissionVersion as submissionVersion " +
				"where submissionVersion.assignmentSubmission = :submission";
		    	
		    	Query query = session.createQuery(queryString);
		    	query.setParameter("submission", submission);
		    	
		    	Long submissionVersionId = (Long) query.uniqueResult();
		    	
		    	if (submissionVersionId != null) {
		    		currentVersion = getAssignmentSubmissionVersionByIdWithAttachments(submissionVersionId);
		    	}
		    	
		    	return currentVersion;
			}
		};
    	
		return (AssignmentSubmissionVersion)getHibernateTemplate().execute(hc);
    }
    
    private List<Long> getCurrentVersionIdsForSubmissions(final Collection<AssignmentSubmission> submissions) {   
    	HibernateCallback hc = new HibernateCallback() {
			public Object doInHibernate(Session session) throws HibernateException ,SQLException {
				List<Long> versionIdList = new ArrayList<Long>();

		    	if (submissions != null && !submissions.isEmpty()) {
		    		
		    		List<AssignmentSubmission> submissionList = new ArrayList<AssignmentSubmission>(submissions);
		    		
		    		Query query = session.getNamedQuery("findCurrentVersionIds");

		    		// if submission list is > than the max length allowed in sql, we need
		    		// to cycle through the list

		    		versionIdList = queryWithParameterList(query, "submissionList", submissionList);
		    	}

		    	return versionIdList;
			}
		};
    	
		return (List<Long>)getHibernateTemplate().execute(hc);
    	
    }
    
    public List<AssignmentSubmission> getCurrentAssignmentSubmissionsForStudent(final List<Assignment2> assignments, final String studentId) {
		if (studentId == null) {
			throw new IllegalArgumentException("null studentId passed to getAllSubmissionRecsForStudentWithVersionData");
		}
		
		HibernateCallback hc = new HibernateCallback() {
			public Object doInHibernate(Session session) throws HibernateException ,SQLException {
				List<AssignmentSubmission> submissions = new ArrayList<AssignmentSubmission>();
				
				if (assignments != null && !assignments.isEmpty()) {
					// retrieve the submissions
					Query query = session.getNamedQuery("findSubmissionsForStudentForAssignments");
			    	query.setParameter("studentId",studentId);
		    	
			    	submissions = queryWithParameterList(query, "assignmentList", assignments);
			    	
			    	// now, populate the version information
		    		populateCurrentVersion(submissions);
				}
				
				return submissions;
			}
		};
		return (List<AssignmentSubmission>)getHibernateTemplate().execute(hc);
		
    }
    
    public AssignmentSubmissionVersion getAssignmentSubmissionVersionByIdWithAttachments(final Long submissionVersionId) {
    	if (submissionVersionId == null) {
    		throw new IllegalArgumentException("Null submissionVersionId passed to getAssignmentSubmissionVersionByIdWithAttachments");
    	}
    	HibernateCallback hc = new HibernateCallback() {
			public Object doInHibernate(Session session) throws HibernateException ,SQLException {
				Query query = session.getNamedQuery("findSubmissionVersionByIdWithAttachments");
		    	query.setParameter("submissionVersionId", submissionVersionId);
				return query.uniqueResult();
			}
		};
		
		AssignmentSubmissionVersion version = (AssignmentSubmissionVersion)getHibernateTemplate().execute(hc);
		if (version == null) {
			throw new VersionNotFoundException("No version exists with id: " + submissionVersionId);
		}
		
		return version;
    }
    
    private List<AssignmentSubmissionVersion> getAssignmentSubmissionVersionsById(final List<Long> versionIds) {
    	
    	HibernateCallback hc = new HibernateCallback() {
			public Object doInHibernate(Session session) throws HibernateException ,SQLException {
		    	List<AssignmentSubmissionVersion> versions = new ArrayList<AssignmentSubmissionVersion>();
		    	
				if (versionIds != null && !versionIds.isEmpty()) {
		    		String hql = "from AssignmentSubmissionVersion as version where version.id in (:versionIdList)";
		    		Query query = session.createQuery(hql);
		    		
		    		versions = queryWithParameterList(query, "versionIdList", versionIds);
		    	}
		    	
		    	return versions;
			}
		};
		return (List<AssignmentSubmissionVersion>)getHibernateTemplate().execute(hc);
    	
    }
    
    public Set<AssignmentSubmission> getCurrentSubmissionsForStudentsForAssignment(final List<String> studentIds, final Assignment2 assignment) {
    	if (assignment == null) {
    		throw new IllegalArgumentException("null assignment passed to getSubmissionsForStudentsForAssignment");    		
    	}
    	
    	HibernateCallback hc = new HibernateCallback() {
			public Object doInHibernate(Session session) throws HibernateException ,SQLException {
				Set<AssignmentSubmission> submissionSet = new HashSet<AssignmentSubmission>();
		    	
		    	if (studentIds != null && !studentIds.isEmpty()) {
		    		Query query = session.getNamedQuery("findSubmissionsForStudentsForAssignment");
		    		query.setParameter("assignment", assignment);
		    		
		    		List<AssignmentSubmission> submissionList = queryWithParameterList(query, "studentIdList", studentIds);
		    			
		    		if (submissionList != null) {
		    			submissionSet = new HashSet<AssignmentSubmission>(submissionList);
		    			
		        		// now retrieve the current version information
		        		populateCurrentVersion(submissionSet);
		    		}
		    	}
		    	
		    	return submissionSet;
			}
		};
		return (Set<AssignmentSubmission>)getHibernateTemplate().execute(hc);
    	
    	
    }
    
    /**
     * populates the most recent version for the given submissions.
     * @param submissions
     */
    private void populateCurrentVersion(Collection<AssignmentSubmission> submissions) {
    	if (submissions != null && !submissions.isEmpty()) {
			// then, we will populate the version data
			
			// first, retrieve the ids of the current versions
			List<Long> versionIds = getCurrentVersionIdsForSubmissions(submissions);
			
			// now retrieve the associated AssignmentSubmissionVersion recs
			List<AssignmentSubmissionVersion> currentVersions = getAssignmentSubmissionVersionsById(versionIds);
			
			if (currentVersions != null) {
				Map<Long, AssignmentSubmissionVersion> submissionIdVersionMap = new HashMap<Long, AssignmentSubmissionVersion>();
				for (AssignmentSubmissionVersion version : currentVersions) {
					if (version != null) {
						submissionIdVersionMap.put(version.getAssignmentSubmission().getId(), version);
					}
				}
				
				for (AssignmentSubmission submission : submissions) {
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
    
    public Set<AssignmentSubmission> getSubmissionsWithVersionHistoryForStudentListAndAssignment(final List<String> studentIdList, final Assignment2 assignment) {
    	if (assignment == null) {
    		throw new IllegalArgumentException("null assignment passed to getSubmissionsWithVersionHistoryForStudentListAndAssignment");
    	}
    	
    	HibernateCallback hc = new HibernateCallback() {
			public Object doInHibernate(Session session) throws HibernateException ,SQLException {
				Set<AssignmentSubmission> submissionSet = new HashSet<AssignmentSubmission>();
		    	
		    	if (studentIdList != null && !studentIdList.isEmpty()) {
		    		Query query = session.getNamedQuery("findSubmissionsWithHistoryForAssignmentAndStudents");	
		        	query.setParameter("assignment", assignment);
		        	
		        	List<AssignmentSubmission> submissionList = queryWithParameterList(query, "studentIdList", studentIdList);
		        	
		        	if (submissionList != null) {
		    			submissionSet = new HashSet<AssignmentSubmission>(submissionList);
		    			
		        		// now retrieve the current version information
		        		populateCurrentVersion(submissionSet);
		    		}
		    	}
		    	
		    	return submissionSet;
			}
		};
		
		return (Set<AssignmentSubmission>)getHibernateTemplate().execute(hc);
    }
    
    public AssignmentSubmission getSubmissionWithVersionHistoryForStudentAndAssignment(final String studentId, final Assignment2 assignment) {
    	if (studentId == null || assignment == null) {
    		throw new IllegalArgumentException("null parameter passed to getSubmissionWithVersionHistoryForStudentAndAssignment");
    	}
    	HibernateCallback hc = new HibernateCallback() {
			public Object doInHibernate(Session session) throws HibernateException ,SQLException {
				Query query = session.getNamedQuery("findStudentSubmissionWithHistoryForAssignment");
		    	query.setParameter("studentId", studentId);
		    	query.setParameter("assignment", assignment);
		    	
		    	AssignmentSubmission submission = (AssignmentSubmission) query.uniqueResult();
		    	
		    	if (submission != null) {
		    		setCurrentSubmissionGivenHistory(submission);
		    	}
		    	
		    	return submission;
			}
		};
		
		return (AssignmentSubmission)getHibernateTemplate().execute(hc);
    	
    }
    
    /**
     * given a submission with populated version history, sets the currentVersion to
     * the version with the highest id 
     * @param submission
     */
    private void setCurrentSubmissionGivenHistory(AssignmentSubmission submission) {
    	if (submission != null && submission.getSubmissionHistorySet() != null) {
    		Map<Long, AssignmentSubmissionVersion> versionIdVersionMap = new HashMap<Long, AssignmentSubmissionVersion>();
    		Long maxVersionId = -1L;
    		for (AssignmentSubmissionVersion version : submission.getSubmissionHistorySet()) {
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
    
    public AssignmentSubmission getSubmissionWithVersionHistoryById(final Long submissionId) {
    	if (submissionId == null) {
    		throw new IllegalArgumentException("null submissionId passed to getSubmissionWithVersionHistoryById");
    	}
    	
    	HibernateCallback hc = new HibernateCallback() {
			public Object doInHibernate(Session session) throws HibernateException ,SQLException {
				Query query = session.getNamedQuery("findSubmissionByIdWithHistory");
		    	query.setParameter("submissionId", submissionId);
		    	
		    	AssignmentSubmission submission = (AssignmentSubmission) query.uniqueResult();
		    	
		    	if (submission != null) {
		    		setCurrentSubmissionGivenHistory(submission);
		    	}
		    	
		    	return submission;
			}
		};
		
		AssignmentSubmission submission = (AssignmentSubmission)getHibernateTemplate().execute(hc);
		if (submission == null) {
			throw new SubmissionNotFoundException("No AssignmentSubmission exists with id: " + submissionId);
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
    
    public Set<AssignmentSubmissionVersion> getVersionHistoryForSubmission(final AssignmentSubmission submission) {
    	if (submission == null) {
    		throw new IllegalArgumentException("null submission passed to getVersionHistoryForSubmission");
    	}

    	HibernateCallback hc = new HibernateCallback() {
    		public Object doInHibernate(Session session) throws HibernateException ,SQLException {
    			Set<AssignmentSubmissionVersion> versionSet = new HashSet<AssignmentSubmissionVersion>();

    			Query query = session.getNamedQuery("findVersionHistoryForSubmission");	
    			query.setParameter("submission", submission);

    			List<AssignmentSubmissionVersion> versionList = query.list();

    			if (versionList != null) {
    				versionSet = new HashSet<AssignmentSubmissionVersion>(versionList);
    			}

    			return versionSet;
    		}
    	};

    	return (Set<AssignmentSubmissionVersion>)getHibernateTemplate().execute(hc);
    }

    public AssignmentSubmissionVersion getVersionByUserIdAndSubmittedDate(final String userId,
			final Date submittedDate)
	{
		if (userId == null || submittedDate == null)
		{
			throw new IllegalArgumentException(
					"userId and submittedDate must be non-null when looking up version [" + userId
							+ "," + submittedDate + "]");
		}

		HibernateCallback hc = new HibernateCallback()
		{
			public Object doInHibernate(Session session) throws HibernateException, SQLException
			{
				Query query = session.getNamedQuery("findVersionByUserIdAndSubmittedDate");
				query.setParameter("userId", userId);
				query.setParameter("submittedDate", submittedDate);

				Object o = query.uniqueResult();
				return o;
			}
		};
		return (AssignmentSubmissionVersion) getHibernateTemplate().execute(hc);
	}
    
    public int getNumSubmittedVersions(final String studentId, final Long assignmentId) {
    	if (studentId == null || assignmentId == null) {
    		throw new IllegalArgumentException("Null studentId or assignmentId passed " +
    				"to getTotalNumSubmissionsForStudentForAssignment");
    	}
    	
    	HibernateCallback hc = new HibernateCallback()
		{
			public Object doInHibernate(Session session) throws HibernateException, SQLException
			{
				Query query = session.getNamedQuery("countNumSubmittedVersions");
				query.setParameter("studentId", studentId, Hibernate.STRING);
				query.setParameter("assignmentId", assignmentId, Hibernate.LONG);
				
				return query.uniqueResult();
			}
		};
		return ((Number) getHibernateTemplate().execute(hc)).intValue();
    }
    
    public int getNumStudentsWithASubmission(final Assignment2 assignment, final List<String> studentIdList) {
    	if (assignment == null) {
    		throw new IllegalArgumentException("null assignment passed to getNumStudentsWithASubmission");
    	}
    	
    	int numStudentsWithSubmission = 0;
    	
    	HibernateCallback hc = new HibernateCallback() {
			public Object doInHibernate(Session session) throws HibernateException ,SQLException {
		    	
				List<String> studentsWithSubmission = new ArrayList<String>();
		    	if (studentIdList != null && !studentIdList.isEmpty()) {
		    		Query query = session.getNamedQuery("countNumStudentsWithASubmission");	
		        	query.setParameter("assignment", assignment);
		        	
		        	studentsWithSubmission = queryWithParameterList(query, "studentIdList", studentIdList);
		        	
		    	}
		    	
		    	return studentsWithSubmission;
			}
		};
		
		List<String> studentsWithSubmission = (List<String>)getHibernateTemplate().execute(hc);
		if (studentsWithSubmission != null) {
			numStudentsWithSubmission = studentsWithSubmission.size();
		}
		
		return numStudentsWithSubmission;
    }
    
    
    public int getHighestSubmittedVersionNumber(final AssignmentSubmission submission) {
    	if (submission == null) {
    		throw new IllegalArgumentException("submission cannot be null in getNextSubmittedVersionNumber");
    	}
    	
    	HibernateCallback hc = new HibernateCallback() {
			public Object doInHibernate(Session session) throws HibernateException ,SQLException {
				String hql = "select max(version.submittedVersionNumber) from org.sakaiproject.assignment2.model.AssignmentSubmissionVersion as version where version.assignmentSubmission = :submission";
		    	
		    	Query query = session.createQuery(hql);
		    	query.setParameter("submission", submission);
		    	
		    	Integer currHighestVersionNum = (Integer)query.uniqueResult();
		    	
		    	if (currHighestVersionNum == null) {
		    		currHighestVersionNum = 0;
		    	}

		        return currHighestVersionNum; 
			}
		};
		
		return ((Integer)getHibernateTemplate().execute(hc)).intValue();
    }
    
    public void evictObject(Object obj) {
    	if (obj != null) {
    		getHibernateTemplate().evict(obj);
    	}
    }
}