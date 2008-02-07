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

import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalAnnouncementLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.dao.AssignmentDao;
import org.sakaiproject.assignment2.exception.AnnouncementPermissionException;
import org.sakaiproject.assignment2.exception.ConflictingAssignmentNameException;
import org.sakaiproject.assignment2.exception.NoGradebookItemForGradedAssignmentException;
import org.sakaiproject.genericdao.api.finders.ByPropsFinder;
import org.sakaiproject.service.gradebook.shared.StaleObjectModificationException;
import org.sakaiproject.assignment2.logic.utils.ComparatorsUtils;
import org.springframework.orm.hibernate3.HibernateOptimisticLockingFailureException;


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
    
    private ExternalGradebookLogic gradebookLogic;
    public void setExternalGradebookLogic(ExternalGradebookLogic gradebookLogic) {
        this.gradebookLogic = gradebookLogic;
    }
    
    private ExternalAnnouncementLogic announcementLogic;
    public void setExternalAnnouncementLogic(ExternalAnnouncementLogic announcementLogic) {
        this.announcementLogic = announcementLogic;
    }
    
    private AssignmentPermissionLogic permissionLogic;
    public void setPermissionLogic(AssignmentPermissionLogic permissionLogic) {
        this.permissionLogic = permissionLogic;
    }
    
    private AssignmentSubmissionLogic submissionLogic;
    public void setAssignmentSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
        this.submissionLogic = submissionLogic;
    }
    
    private AssignmentDao dao;
    public void setDao(AssignmentDao dao) {
        this.dao = dao;
    }
    
	public void init(){
		if(log.isDebugEnabled()) log.debug("init");
	}

	public Assignment2 getAssignmentById(Long assignmentId)
	{
		if (assignmentId == null) {
			throw new IllegalArgumentException("Null assignmentId passed to getAssignmentById");
		}
		
		return (Assignment2) dao.findById(Assignment2.class, assignmentId);
    }
	
	public Assignment2 getAssignmentByIdWithAssociatedData(Long assignmentId) {
		if (assignmentId == null) {
			throw new IllegalArgumentException("Null assignmentId passed to getAssignmentByIdWithAssociatedData");
		}
		// first, retrieve Assignment2 object
		Assignment2 assign = (Assignment2) dao.getAssignmentByIdWithGroupsAndAttachments(assignmentId);
		
		if (assign != null) {
			// populate any non-persisted fields that are applicable to this view
			boolean restrictedToGroups = assign.getAssignmentGroupSet() != null &&
				!assign.getAssignmentGroupSet().isEmpty();
			assign.setRestrictedToGroups(restrictedToGroups);
			
			if (!assign.isUngraded()) {
				gradebookLogic.populateGradebookItemDetailsForAssignment(externalLogic.getCurrentContextId(), assign);
			}
		}
		
		return assign;
	}
	
	public Assignment2 getAssignmentByIdWithGroups(Long assignmentId) {
		if (assignmentId == null) {
			throw new IllegalArgumentException("Null assignmentId passed to getAssignmentByIdWithGroups");
		}
		
		return (Assignment2) dao.getAssignmentByIdWithGroups(assignmentId);
	}
	
	public Assignment2 getAssignmentByIdWithGroupsAndAttachments(Long assignmentId) {
		if (assignmentId == null) {
			throw new IllegalArgumentException("Null assignmentId passed to getAssignmentByIdWithGroupsAndAttachments");
		}
		
		return (Assignment2) dao.getAssignmentByIdWithGroupsAndAttachments(assignmentId);
	}
	
	public void saveAssignment(Assignment2 assignment) throws SecurityException, 
		ConflictingAssignmentNameException, NoGradebookItemForGradedAssignmentException
	{
		if (assignment == null) {
			throw new IllegalArgumentException("Null assignment passed to saveAssignment");
		}
		
		String currentContextId = externalLogic.getCurrentContextId();
		String currentUserId = externalLogic.getCurrentUserId();
		
		if (!assignment.isUngraded() && assignment.getGradableObjectId() == null) {
			throw new NoGradebookItemForGradedAssignmentException("The assignment to save " + 
					"was defined as graded but it had a null gradableObjectId");
		}
		
		if (!permissionLogic.isCurrentUserAbleToEditAssignments(currentContextId)) {
			throw new SecurityException("Current user may not save assignment " + assignment.getTitle()
                    + " because they do not have edit permission");
		}
		
		boolean isNewAssignment = true;
		Assignment2 existingAssignment = null;
		
		// determine if this is a new assignment
		if (assignment.getId() != null) {
			// check to see if assignment exists
			existingAssignment = (Assignment2)dao.getAssignmentByIdWithGroupsAndAttachments(assignment.getId());	
			if (existingAssignment != null) {
				isNewAssignment = false;
			}
		}
		
		if (isNewAssignment) {
        	// check to ensure it is not a duplicate title
        	if (assignmentNameExists(assignment.getTitle())) {
        		throw new ConflictingAssignmentNameException("An assignment with the title " + assignment.getTitle() + " already exists");
        	}
        	// identify the next sort index to be used
        	Integer highestIndex = dao.getHighestSortIndexInSite(currentContextId);
        	if (highestIndex != null) {
        		assignment.setSortIndex(highestIndex + 1);
        	} else {
        		assignment.setSortIndex(0);
        	}
        	
        	assignment.setRemoved(Boolean.FALSE);
        	assignment.setCreateTime(new Date());
        	assignment.setCreator(currentUserId);
        	
        	Set<AssignmentAttachment> attachSet = new HashSet();
        	if (assignment.getAttachmentSet() != null) {
        		attachSet = assignment.getAttachmentSet();
        	}
        	Set<AssignmentGroup> groupSet = new HashSet();
        	if (assignment.getAssignmentGroupSet() != null) {
        		groupSet = assignment.getAssignmentGroupSet();
        	}
        	
        	// make sure the assignment has been set for the attachments and groups
        	populateAssignmentForAttachmentAndGroupSets(attachSet, groupSet, assignment);
        	
        	Set<Assignment2> assignSet = new HashSet();
        	assignSet.add(assignment);
        	
        	dao.saveMixedSet(new Set[] {assignSet, attachSet, groupSet});
        	if(log.isDebugEnabled()) log.debug("Created assignment: " + assignment.getTitle());
  
		} else {
			if (!assignment.getTitle().equals(existingAssignment.getTitle())) {
				// check to see if this new title already exists
				if (assignmentNameExists(assignment.getTitle())) {
	        		throw new ConflictingAssignmentNameException("An assignment with the title " + assignment.getTitle() + " already exists");
	        	}
			}
			
			assignment.setRemoved(Boolean.FALSE);
			assignment.setModifiedBy(currentUserId);
			assignment.setModifiedTime(new Date());
			
			Set<AssignmentAttachment> attachToDelete = identifyAttachmentsToDelete(existingAssignment, assignment);
			Set<AssignmentGroup> groupsToDelete = identifyGroupsToDelete(existingAssignment, assignment);
			
			try {
	        	Set<AssignmentAttachment> attachSet = new HashSet();
	        	if (assignment.getAttachmentSet() != null) {
	        		attachSet = assignment.getAttachmentSet();
	        	}
	        	Set<AssignmentGroup> groupSet = new HashSet();
	        	if (assignment.getAssignmentGroupSet() != null) {
	        		groupSet = assignment.getAssignmentGroupSet();
	        	}
	        	
	        	// make sure the assignment has been set for the attachments and groups
	        	populateAssignmentForAttachmentAndGroupSets(attachSet, groupSet, assignment);
	        	
	        	Set<Assignment2> assignSet = new HashSet();
	        	assignSet.add(assignment);
	        	
	        	dao.saveMixedSet(new Set[] {assignSet, attachSet, groupSet});
	        	if(log.isDebugEnabled())log.debug("Updated assignment: " + assignment.getTitle() + "with id: " + assignment.getId());
	            
	            if ((attachToDelete != null && !attachToDelete.isEmpty()) ||
	            		(groupsToDelete != null && !groupsToDelete.isEmpty())) {
	            	dao.deleteMixedSet(new Set[] {attachToDelete, groupsToDelete});
	            	if(log.isDebugEnabled())log.debug("Attachments and/or groups removed for updated assignment " + assignment.getId());
	            }
			} catch (HibernateOptimisticLockingFailureException holfe) {
				if(log.isInfoEnabled()) log.info("An optimistic locking failure occurred while attempting to update an assignment");
	            throw new StaleObjectModificationException(holfe);
			}
		}
	}
	

	public void deleteAssignment(Assignment2 assignment) throws SecurityException, AnnouncementPermissionException
	{
		if (assignment == null) {
			throw new IllegalArgumentException("Null assignment passed to deleteAssignment");
		}
		
		if (assignment.getId() == null) {
			throw new IllegalArgumentException("The passed assignment does not have an id. Can only delete persisted assignments");
		}
		
		String currentContextId = externalLogic.getCurrentContextId();
		
		if (!permissionLogic.isCurrentUserAbleToEditAssignments(currentContextId)) {
			throw new SecurityException("Current user may not delete assignment " + assignment.getTitle()
                    + " because they do not have edit permission");
		}

		assignment.setRemoved(true);
		assignment.setModifiedBy(externalLogic.getCurrentUserId());
		assignment.setModifiedTime(new Date());
		
		String announcementIdToDelete = null;
		if (assignment.getAnnouncementId() != null) {
			announcementIdToDelete = assignment.getAnnouncementId();
			assignment.setAnnouncementId(null);
			assignment.setHasAnnouncement(Boolean.FALSE);
		}

		try {
			dao.update(assignment);
			if(log.isDebugEnabled()) log.debug("Deleted assignment: " + assignment.getTitle() + " with id " + assignment.getId());
			
			// now remove the announcement, if applicable
			if (announcementIdToDelete != null) {
				announcementLogic.deleteOpenDateAnnouncement(announcementIdToDelete, currentContextId);
				if(log.isDebugEnabled()) log.debug("Deleted announcement with id " + announcementIdToDelete + " for assignment " + assignment.getId());
			}
		} catch (HibernateOptimisticLockingFailureException holfe) {
			if(log.isInfoEnabled()) log.info("An optimistic locking failure occurred while attempting to update an assignment");
			throw new StaleObjectModificationException(holfe);
		} catch (AnnouncementPermissionException ape) {
			if(log.isDebugEnabled()) log.debug("The current user is not authorized to remove announcements in the annc tool, " +
					"but the assignment was deleted");
			throw new AnnouncementPermissionException("The current user is not authorized to remove announcements in the annc tool, " +
					"but the assignment was deleted");
		}
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sakaiproject.assignment2.logic.AssignmentLogic#getViewableAssignments(String)
	 */
	public List<Assignment2> getViewableAssignments()
	{
		List<Assignment2> viewableAssignments = new ArrayList();
		String contextId = externalLogic.getCurrentContextId();
		String userId = externalLogic.getCurrentUserId();

		Set<Assignment2> allAssignments = dao.getAssignmentsWithGroupsAndAttachments(contextId);

		if (allAssignments != null && !allAssignments.isEmpty()) {

			List<Assignment2> gradedAssignments = new ArrayList();

			// users may view ungraded items if:
			//  a) it is not restricted to groups
			//  b) it is restricted, but user has grade all perm
			//  c) it is restricted, but user is a member of restricted group
			//  d) it is not draft or user has edit perm

			List<String> userGroupIds = externalLogic.getUserMembershipGroupIdList(userId);	
			boolean isUserAbleToEdit = permissionLogic.isCurrentUserAbleToEditAssignments(contextId);
			boolean isUserAStudent = gradebookLogic.isCurrentUserAStudentInGb(contextId);

			for (Iterator asnIter = allAssignments.iterator(); asnIter.hasNext();) {
				Assignment2 assignment = (Assignment2) asnIter.next();

				if (!assignment.isDraft() || isUserAbleToEdit) {
					// students may not view if not open
					if (!isUserAStudent || (isUserAStudent && assignment.getOpenTime().before(new Date()))) 
						if (assignment.isUngraded()) {
							if (permissionLogic.isUserAbleToViewUngradedAssignment(assignment, userGroupIds)) {
								viewableAssignments.add(assignment);
							} 

						} else {
							gradedAssignments.add(assignment);
						}
				}
			}

			if (gradedAssignments != null && !gradedAssignments.isEmpty()) {
				// now, we need to filter the assignments that are associated with
				// the gradebook according to grader permissions and populate the
				// gradebook data
				List viewableGbAssignments = gradebookLogic.getViewableAssignmentsWithGbData(gradedAssignments, externalLogic.getCurrentContextId());
				if (viewableGbAssignments != null) {

					for (Iterator gradedIter = viewableGbAssignments.iterator(); gradedIter.hasNext();) {
						Assignment2 assignment = (Assignment2) gradedIter.next();
						
						boolean restrictedToGroups = assignment.getAssignmentGroupSet() != null
						&& !assignment.getAssignmentGroupSet().isEmpty();
						
						// if user is a "student" in terms of the gb, we need to filter the view
						// by AssignmentGroup restrictions.
						if (restrictedToGroups && isUserAStudent) {
							if (permissionLogic.isUserAMemberOfARestrictedGroup(userGroupIds, assignment.getAssignmentGroupSet())) {
								viewableAssignments.add(assignment);
							}
						} else {
							viewableAssignments.add(assignment);
						}
					}
				}
			}
			
			if (isUserAStudent) {
				// if this is a student, we need to populate the submissionStatus for each assignment
				submissionLogic.setSubmissionStatusForAssignments(viewableAssignments, userId);
			}
		}
		
		return viewableAssignments;
	}
	
	public void setAssignmentSortIndexes(Long[] assignmentIds)
	{
		int numAssignsInSite = dao.countByProperties(Assignment2.class, 
				new String[] {"contextId", "removed"}, new Object[]{externalLogic.getCurrentContextId(), false});
		
		if ((assignmentIds == null && numAssignsInSite > 0) ||
				(assignmentIds != null && assignmentIds.length != numAssignsInSite)) {
			throw new IllegalArgumentException("The length of the id list passed does not match the num assignments in the site");
		}
		
		if (assignmentIds != null) {
			String userId = externalLogic.getCurrentUserId();
			//Assume array of longs is in correct order now
			//so that the index of the array is the new 
			//sort index
			Set<Assignment2> assignSet = new HashSet();
			for (int i=0; i < assignmentIds.length; i++){
				//get Assignment
	    		Assignment2 assignment = getAssignmentById(assignmentIds[i]);
	    		if (assignment != null){
	    			//check if we need to update
	    			if (assignment.getSortIndex() != i){
	    				//update and save
		    			assignment.setSortIndex(i);
		    			assignment.setModifiedBy(userId);
		    			assignment.setModifiedTime(new Date());
		    			assignSet.add(assignment);
		    			if(log.isDebugEnabled()) log.debug("Assignment " + assignment.getId() + " sort index changed to " + i);
	    			}
	    		}
	    	}
			try {
				dao.saveMixedSet(new Set[]{assignSet});
				if(log.isDebugEnabled()) log.debug("Reordered assignments saved. " + 
						assignSet.size() + " assigns were updated");
			} catch (HibernateOptimisticLockingFailureException holfe) {
				if(log.isInfoEnabled()) log.info("An optimistic locking failure occurred while attempting to reorder the assignments");
	            throw new StaleObjectModificationException(holfe);
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
	               new String[] {"contextId", "title", "removed", "draft"}, 
	               new Object[] {externalLogic.getCurrentContextId(), assignmentName, Boolean.FALSE, Boolean.FALSE});
		
		return count > 0;
	}
	
	public int getStatusForAssignment(Assignment2 assignment) {
		if (assignment == null){
			throw new IllegalArgumentException("Null assignment passed to check status");
		}
		if (assignment.isDraft())
			return AssignmentConstants.STATUS_DRAFT;
		
		Date currDate = new Date();
		
		if (currDate.before(assignment.getOpenTime()))
			return AssignmentConstants.STATUS_NOT_OPEN;
		
		if (assignment.getAcceptUntilTime() != null && currDate.after(assignment.getAcceptUntilTime())) {
			return AssignmentConstants.STATUS_CLOSED;
		}
		
		if (assignment.isUngraded()) {
			if (assignment.getDueDateForUngraded() != null && currDate.after(assignment.getDueDateForUngraded()))
				return AssignmentConstants.STATUS_DUE;
		}
		else if (assignment.getDueDate() != null) {
			if (currDate.after(assignment.getDueDate()))
				return AssignmentConstants.STATUS_DUE;				
		}
		
		return AssignmentConstants.STATUS_OPEN;	
	}
	
	private void populateAssignmentForAttachmentAndGroupSets(Set<AssignmentAttachment> attachSet, Set<AssignmentGroup> groupSet, Assignment2 assign) {
		if (attachSet != null && !attachSet.isEmpty()) {
			for (Iterator attachIter = attachSet.iterator(); attachIter.hasNext();) {
				AssignmentAttachment attach = (AssignmentAttachment) attachIter.next();
				if (attach != null) {
					attach.setAssignment(assign);
				}
			}
		}
		if (groupSet != null && !groupSet.isEmpty()) {
			for (Iterator groupIter = groupSet.iterator(); groupIter.hasNext();) {
				AssignmentGroup group = (AssignmentGroup) groupIter.next();
				if (group != null) {
					group.setAssignment(assign);
				}
			}
		}
	}
	
	private Set identifyAttachmentsToDelete(Assignment2 existingAssign, Assignment2 updatedAssign) {
		Set attachToRemove = new HashSet();
		
		if (updatedAssign != null && existingAssign != null && existingAssign.getAttachmentSet() != null) {
			for (Iterator existingIter = existingAssign.getAttachmentSet().iterator(); existingIter.hasNext();) {
				AssignmentAttachment attach = (AssignmentAttachment) existingIter.next();
				if (attach != null) {
					if (updatedAssign.getAttachmentSet() == null ||
							!updatedAssign.getAttachmentSet().contains(attach)) {
						// we need to delete this attachment
						attachToRemove.add(attach);
					} 
				}
			}
		}
		
		return attachToRemove;
	}
	
	private Set identifyGroupsToDelete(Assignment2 existingAssign, Assignment2 updatedAssign) {
		Set groupsToRemove = new HashSet();
		
		if (updatedAssign != null && existingAssign != null && existingAssign.getAssignmentGroupSet() != null) {
			for (Iterator existingIter = existingAssign.getAssignmentGroupSet().iterator(); existingIter.hasNext();) {
				AssignmentGroup attach = (AssignmentGroup) existingIter.next();
				if (attach != null) {
					if (updatedAssign.getAssignmentGroupSet() == null ||
							!updatedAssign.getAssignmentGroupSet().contains(attach)) {
						// we need to delete this group
						groupsToRemove.add(attach);
					} 
				}
			}
		}
		
		return groupsToRemove;
	}
	
	public void saveAssignmentAnnouncement(Assignment2 originalAssignment, Assignment2 updatedAssignment, 
			String newAnncSubject, String newAnncBody, String revAnncSubject, String revAnncBody) {
		if (updatedAssignment == null) {
			throw new IllegalArgumentException("Null updatedAssignment passed to saveAssignmentAnnouncement");
		}
		
		if (updatedAssignment.getId() == null) {
			throw new IllegalArgumentException("The updatedAssignment passed to saveAssignmentAnnouncement must be persisted");
		}
		
		if (newAnncSubject == null || newAnncBody == null || revAnncSubject == null || revAnncBody == null) {
			throw new IllegalArgumentException("Null announcement text passed to saveAssignmentAnnouncement");
		}
		
		String currentContextId = externalLogic.getCurrentContextId();
		
		if (originalAssignment == null) {
			// this was a new assignment
			// check to see if there will be an announcement for the open date
			if (updatedAssignment.getHasAnnouncement() && !updatedAssignment.isDraft()) {
				// add an announcement for the open date for this assignment
				String announcementId = announcementLogic.addOpenDateAnnouncement(
						updatedAssignment.getListOfAssociatedGroupReferences(), currentContextId,
						newAnncSubject, newAnncBody);
				updatedAssignment.setAnnouncementId(announcementId);
				dao.update(updatedAssignment);
			}
		} else if (updatedAssignment.isDraft()) {
			if (updatedAssignment.getAnnouncementId() != null) {
				announcementLogic.deleteOpenDateAnnouncement(updatedAssignment.getAnnouncementId(), currentContextId);
				updatedAssignment.setAnnouncementId(null);
				dao.update(updatedAssignment);
			}
		} else if (originalAssignment.getAnnouncementId() == null && updatedAssignment.getHasAnnouncement()) {
			// this is a new announcement
			String announcementId = announcementLogic.addOpenDateAnnouncement(updatedAssignment.getListOfAssociatedGroupReferences(), 
					currentContextId, newAnncSubject, newAnncBody);
			updatedAssignment.setAnnouncementId(announcementId);
			dao.update(updatedAssignment);
		} else if (originalAssignment.getAnnouncementId() != null && !updatedAssignment.getHasAnnouncement()) {
			// we must remove the original announcement
			announcementLogic.deleteOpenDateAnnouncement(updatedAssignment.getAnnouncementId(), currentContextId);
			updatedAssignment.setAnnouncementId(null);
			dao.update(updatedAssignment);
		} else if (updatedAssignment.getHasAnnouncement()){
			// if title, open date, or group restrictions were updated, we need to update the announcement
			Date oldTime = (Date)originalAssignment.getOpenTime();
			Date newTime = updatedAssignment.getOpenTime();
			if (!originalAssignment.getTitle().equals(updatedAssignment.getTitle()) ||
					(oldTime.after(newTime) || oldTime.before(newTime)) ||
					!originalAssignment.getListOfAssociatedGroupReferences().equals(updatedAssignment.getListOfAssociatedGroupReferences())) {
				announcementLogic.updateOpenDateAnnouncement(updatedAssignment.getAnnouncementId(), 
						updatedAssignment.getListOfAssociatedGroupReferences(), 
						currentContextId, revAnncSubject, revAnncBody);
				// don't need to re-save assignment b/c id already exists
			}
		}
	}
}
