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

package org.sakaiproject.assignment2.dao;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.genericdao.api.CompleteGenericDao;

/**
 * Basic DAO functionality for the Assignment2 tool
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public interface AssignmentDao extends CompleteGenericDao {

	/**
	 * Used to identify the next sort index for new assignments
	 * @param contextId
	 * @return the highest sort index for the existing assignments
	 */
	public Integer getHighestSortIndexInSite(String contextId);
	
	/**
	 * 
	 * @param contextId
	 * @return all of the assignments in the given site with the associated
	 * AssignmentGroup and AssignmentAttachment data populated
	 */
	public Set<Assignment2> getAssignmentsWithGroupsAndAttachments(String contextId);
	
	/**
	 * 
	 * @param assignmentId
	 * @return the Assignment2 object associated with the given id with the
	 * associated AssignmentGroup and AssignmentAttachment data populated
	 */
	public Assignment2 getAssignmentByIdWithGroupsAndAttachments(Long assignmentId);
	
	/**
	 * 
	 * @param assignmentId
	 * @return the Assignment2 object associated with the given id with the
	 * associated AssignmentGroup data populated. No attachments or submission info
	 */
	public Assignment2 getAssignmentByIdWithGroups(Long assignmentId);
	
	/**
	 * 
	 * @param submission
	 * @return returns the current submission version for the given AssignmentSubmission and userId
	 */
	public AssignmentSubmissionVersion getCurrentSubmissionVersionWithAttachments(AssignmentSubmission submission);
	
	/**
	 * 
	 * @param assignments
	 * @param studentId
	 * @return all AssignmentSubmission records with the currentVersion populated for
	 * the given student and assignments. if there is no submission for an assignment,
	 * nothing is returned
	 */
	public List<AssignmentSubmission> getCurrentAssignmentSubmissionsForStudent(List<Assignment2> assignments, String studentId);
	
	/**
	 * 
	 * @param studentIds
	 * @param assignment
	 * @return given a list of studentIds and an assignment, returns the associated 
	 * AssignmentSubmissions for the given assignment. If
	 * no submission exists for the student yet, no submission is returned
	 * will populate the currentSubmissionVersion 
	 */
	Set<AssignmentSubmission> getCurrentSubmissionsForStudentsForAssignment(List<String> studentIds, Assignment2 assignment);
	
	/**
	 * 
	 * @param studentId
	 * @param assignment
	 * @return the AssignmentSubmission rec for the given student and assignment with the
	 * submissionHistorySet populated with all of the AssignmentSubmissionVersions associated
	 * with this submission. returns null if no submission has been made. will populate the
	 * currentSubmissionVersion and associated attachments
	 */
	public AssignmentSubmission getSubmissionWithVersionHistoryForStudentAndAssignment(String studentId, Assignment2 assignment);
	
	/**
	 * 
	 * @param studentIdList
	 * @param assignment
	 * @return the AssignmentSubmission recs for the given students for the given assignment.
	 * populates the submissionHistorySet with all of the AssignmentSubmissionVersions for
	 * each submission. if no submission has been made, no rec will be returned. will
	 * populate currentVersion. will also populate attachments for the versions
	 * 
	 */
	public Set<AssignmentSubmission> getSubmissionsWithVersionHistoryForStudentListAndAssignment(List<String> studentIdList, Assignment2 assignment);
	
	/**
	 * 
	 * @param submissionVersionId
	 * @return the AssignmentSubmissionVersion with associated attachments for the given id.
	 * returns null if no version exists with the given id
	 */
	public AssignmentSubmissionVersion getAssignmentSubmissionVersionByIdWithAttachments(Long submissionVersionId);
	
	/**
	 * 
	 * @param submissionId
	 * @return the AssignmentSubmission with the given id with the history set populated. Version
	 * attachments are NOT populated. Will populate currentVersion 
	 */
	public AssignmentSubmission getSubmissionWithVersionHistoryById(Long submissionId);
	
	/**
	 * 
	 * @param submission
	 * @return set of AssignmentSubmissionVersions associated with the given submission.
	 * will populate the feedback and submission attachments for each version
	 */
	public Set<AssignmentSubmissionVersion> getVersionHistoryForSubmission(final AssignmentSubmission submission);

}
