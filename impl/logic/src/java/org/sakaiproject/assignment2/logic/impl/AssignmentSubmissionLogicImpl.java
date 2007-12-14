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

import java.util.Date;
import java.util.List;
import java.util.ArrayList;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.PermissionLogic;
import org.sakaiproject.assignment2.dao.AssignmentDao;

/**
 * This is the interface for interaction with the AssignmentSubmission object
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class AssignmentSubmissionLogicImpl implements AssignmentSubmissionLogic{
	
	private static Log log = LogFactory.getLog(AssignmentSubmissionLogicImpl.class);
	
	private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }
    
    private AssignmentDao dao;
    public void setDao(AssignmentDao dao) {
        this.dao = dao;
    }
    
    private ExternalGradebookLogic gradebookLogic;
    public void setExternalGradebookLogic(ExternalGradebookLogic gradebookLogic) {
        this.gradebookLogic = gradebookLogic;
    }
    
    private PermissionLogic permissionLogic;
    public void setPermissionLogic(PermissionLogic permissionLogic) {
        this.permissionLogic = permissionLogic;
    }
    
	public void init(){
		log.debug("init");
	}
	
	public AssignmentSubmission getAssignmentSubmissionById(Long submissionId) {
		if (submissionId == null) {
			throw new IllegalArgumentException("Null submissionId passed to getAssignmentSubmissionById");
		}
		
		return (AssignmentSubmission)dao.findById(AssignmentSubmission.class, submissionId);
	}
	
	public AssignmentSubmission getCurrentSubmissionByAssignmentIdAndStudentId(Long assignmentId, String studentId, boolean includeDraft) {
		if (assignmentId == null || studentId == null) {
			throw new IllegalArgumentException("Null assignmentId or userId passed to getCurrentSubmissionByAssignmentAndUser");
		}
		
		AssignmentSubmission submission = null;
		
		Assignment2 assignment = dao.getAssignmentByIdWithGroups(assignmentId);
		if (assignment != null) {
			if (permissionLogic.isUserAbleToViewStudentSubmissionForAssignment(studentId, assignment)) {
				throw new SecurityException("Current user is not allowed to view submission for " + studentId + " for assignment " + assignment.getAssignmentId());
			}
			
			
		}
		
		return submission;
	}

	public void saveStudentSubmission(AssignmentSubmission assignmentSubmission) {
		// TESTING
		/*Assignment2 assign = (Assignment2)dao.findById(Assignment2.class, new Long(1));
		assignmentSubmission = new AssignmentSubmission(assign, "8c8d9fe3-dfc1-4fa7-be91-e420b865b20c");
		AssignmentSubmissionVersion version = new AssignmentSubmissionVersion();
		version.setAssignmentSubmission(assignmentSubmission);
		version.setDraft(false);
		version.setCreatedBy("8c8d9fe3-dfc1-4fa7-be91-e420b865b20c");
		version.setCreatedTime(new Date());
		version.setSubmittedText("This is my first submission");
		version.setSubmittedTime(new Date());
		assignmentSubmission.setCurrentSubmissionVersion(version);*/
		
		
		// END TESTING
		if (assignmentSubmission == null) {
			throw new IllegalArgumentException("null assignmentSubmission passed to saveAssignmentSubmission");
		}
		
		AssignmentSubmissionVersion newVersion = assignmentSubmission.getCurrentSubmissionVersion();
		
		if (newVersion == null) {
			throw new IllegalArgumentException("null currentSubmissionVersion associated with the assignmentSubmission in saveStudentSubmission");
		}
		if (assignmentSubmission.getSubmissionId() == null) {
			dao.create(assignmentSubmission);
			log.debug("New student submission rec added for user " + assignmentSubmission.getUserId() + " for assignment " + assignmentSubmission.getAssignment().getTitle());
			
			newVersion.setAssignmentSubmission(assignmentSubmission);
		}
		
		dao.save(newVersion);
		log.debug("New student submission version added for user " + assignmentSubmission.getUserId() + " for assignment " + assignmentSubmission.getAssignment().getTitle());
	}
	
	public List<AssignmentSubmission> getViewableSubmissionsForAssignment(Assignment2 assignment) {
		return new ArrayList();
	}
	
	public List<AssignmentSubmission> getAllSubmissionsByAssignmentIdNoVersionData(Long assignmentId) {
    	if (assignmentId == null) {
    		throw new IllegalArgumentException("null assignmentId passed to getSubmissionsByAssignmentIdNoVersionData");
    	}
    	
    	List<AssignmentSubmission> submissionList = dao.findByProperties(AssignmentSubmission.class, 
    			new String[] {"assignmentId"}, new Object[] {assignmentId});
    	
    	return submissionList;
    }
	
	public AssignmentSubmission getSubmissionVersionForUserIdAndAssignmentWithAttachments(Assignment2 assignment, String userId, boolean includeDraft) {
		return null;
	}

}
