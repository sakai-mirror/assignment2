/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/api/model/src/java/org/sakaiproject/assignment2/model/Assignment.java $
 * $Id: Assignment.java 12544 2006-05-03 15:06:26Z wagnermr@iupui.edu $
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

package org.sakaiproject.assignment2.model;

import java.util.Date;
import java.util.Set;

/**
 * The Assignment object
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class Assignment2 {

    private Long assignmentId;
    private Long gradableObjectId;
    private String contextId;
    private String title;
    private Boolean draft;
    private int sortIndex;
    private Date openTime;
    private Date acceptUntilTime;
    private Boolean ungraded;
    private Date dueDateForUngraded;
    private Boolean restrictedToGroups;
    private Boolean groupSubmission;
    private Boolean honorPledge;
    private String instructions;
    private int submissionType;
    private int notificationType;
    private String announcementId;
    private String calendarEventId;
    private Boolean allowResubmitUntilDue;
    private Boolean allowReviewService;
    private Boolean allowStudentViewReport;
    private String creator;
    private Date createTime;
    private String modifiedBy;
    private Date modifiedTime;
    private Boolean removed;
    private int version;
    private Set<AssignmentAttachment> attachmentSet;
    private Set<AssignmentGroup> assignmentGroupSet;
    
    // fields that are manually retrieved from the gradebook
    private Double pointsPossible;
    private Date dueDate;
    
    // fields that are not persisted but needed for UI
    private Integer numberOfTotalSubmissions;
    private Integer numberOfNewSubmissions;
    private Integer status;
    private String groupRestrictionList;
    private Boolean hasAnnouncement;

	public Assignment2(Long gradableObjectId, String contextId, String title,
			Boolean draft, int sortIndex, Date openTime, 
			Date acceptUntilTime, Boolean ungraded, Date dueDateForUngraded,
			Boolean restrictedToGroups, Boolean groupSubmission,
			Boolean honorPledge, String instructions, int submissionType,
			int notificationType, String announcementId,
			String calendarEventId, Boolean allowResubmitUntilDue,
			Boolean allowReviewService, Boolean allowStudentViewReport,
			String creator, Date createTime, String modifiedBy,
			Date modifiedTime, Double pointsPossible, Date dueDate) {

		this.gradableObjectId = gradableObjectId;
		this.contextId = contextId;
		this.title = title;
		this.draft = draft;
		this.sortIndex = sortIndex;
		this.openTime = openTime;
		this.acceptUntilTime = acceptUntilTime;
		this.ungraded = ungraded;
		this.dueDateForUngraded = dueDateForUngraded;
		this.restrictedToGroups = restrictedToGroups;
		this.groupSubmission = groupSubmission;
		this.honorPledge = honorPledge;
		this.instructions = instructions;
		this.submissionType = submissionType;
		this.notificationType = notificationType;
		this.announcementId = announcementId;
		this.calendarEventId = calendarEventId;
		this.allowResubmitUntilDue = allowResubmitUntilDue;
		this.allowReviewService = allowReviewService;
		this.allowStudentViewReport = allowStudentViewReport;
		this.creator = creator;
		this.createTime = createTime;
		this.modifiedBy = modifiedBy;
		this.modifiedTime = modifiedTime;
		
		// gb fields
		this.pointsPossible = pointsPossible;
		this.dueDate = dueDate;
	}

	/**
     * Default constructor
     */
    public Assignment2() {
    }
    
    /**
     * Getters and Setters
     */
    
    /**
     * @return Returns the assignment id
     */
    public Long getAssignmentId() {
        return assignmentId;
    }
    
    /**
     * set the assignment id
     * @param assignmentId
     */
    public void setAssignmentId(Long assignmentId) {
        this.assignmentId = assignmentId;
    }
    
    /**
     * @return Returns the id of the associated GradableObject in the Gradebook
     */
    public Long getGradableObjectId() {
        return gradableObjectId;
    }
    
    /**
     * set the GradableObject id
     * @param gradableObjectId
     */
    public void setGradableObjectId(Long gradableObjectId) {
        this.gradableObjectId = gradableObjectId;
    }
    
    /**
     * @return Returns the context id
     */
    public String getContextId() {
        return contextId;
    }
    
    /**
     * set the contextId
     * @param contextId
     */
    public void setContextId(String contextId) {
        this.contextId = contextId;
    }
    
    /**
     * 
     * @return the assignment's title
     */
    public String getTitle() {
    	return title;
    }
    
    /**
     * set the assignment title
     * @param title
     */
    public void setTitle(String title) {
    	this.title = title;
    }
    
    /**
     * @return Returns this assignment's draft status
     */
    public Boolean isDraft() {
        return draft;
    }
    
    /**
     * draft status
     * @param draft
     */
    public void setDraft(Boolean draft) {
        this.draft = draft;
    }
    
    /**
     * @return Returns the sort index that determines this assignment's ordering
     */
    public int getSortIndex() {
        return sortIndex;
    }
    
    /**
     * the sort index that determines this assignment's ordering
     * @param sortIndex
     */
    public void setSortIndex(int sortIndex) {
        this.sortIndex = sortIndex;
    }
    
    /**
     * @return The first time at which the assignment can be viewed
     */
    public Date getOpenTime() {
        return openTime;
    }
    
    /**
     * The first time at which the assignment can be viewed
     * @param openTime
     */
    public void setOpenTime(Date openTime) {
        this.openTime = openTime;
    }

    /**
     * @return The time after which responses to this assignment are considered late
     */
    public Date getAcceptUntilTime() {
        return acceptUntilTime;
    }
    
    /**
     * The time after which responses to this assignment are considered late
     * @param acceptUntilTime
     */
    public void setAcceptUntilTime(Date acceptUntilTime) {
        this.acceptUntilTime = acceptUntilTime;
    }
    
    /**
     * All assignments will be linked to the gradebook and store grade
     * information in the gradebook tables except ungraded assignments.  
     * @return true if this assignment is ungraded
     */
    public Boolean isUngraded() {
    	return ungraded;
    }
    
    /**
     * All assignments will be linked to the gradebook and store grade
     * information in the gradebook tables except ungraded assignments.  
     * @param ungraded
     */
    public void setUngraded(Boolean ungraded) {
    	this.ungraded = ungraded;
    }
    
    /**
     * All assignments will be linked to the gradebook except ungraded 
     * assignments.  This field is for storing the due date in this situation
     * since all other assignment due dates will be stored on the gb side.
     * @return due date
     */
    public Date getDueDateForUngraded() {
    	return dueDateForUngraded;
    }
    
    /**
     * All assignments will be linked to the gradebook except ungraded 
     * assignments.  This field is for storing the due date in this situation
     * since all other assignment due dates will be stored on the gb side.
     * @param dueDateForUngraded
     */
    public void setDueDateForUngraded(Date dueDateForUngraded) {
    	this.dueDateForUngraded = dueDateForUngraded;
    }
    
    /**
     * @return Returns true if viewing this assignment is restricted to members
     * of specific group(s)
     */
    public Boolean isRestrictedToGroups() {
        return restrictedToGroups;
    }
    
    /**
     * true if viewing this assignment is restricted to members
     * of specific group(s)
     * @param restrictedToGroups
     */
    public void setRestrictedToGroups(Boolean restrictedToGroups) {
        this.restrictedToGroups = restrictedToGroups;
    }
    
    /**
     * @return Returns true if this assignment will be submitted by a "group", not the
     * individual students in the group
     */
    public Boolean isGroupSubmission() {
        return groupSubmission;
    }
    
    /**
     * true if this assignment will be submitted by a "group", not the
     * individual students in the group
     * @param groupSubmission
     */
    public void setGroupSubmission(Boolean groupSubmission) {
        this.groupSubmission = groupSubmission;
    }
    
    /**
     * @return Returns true if this assignment requires an honor pledge
     */
    public Boolean isHonorPledge() {
        return honorPledge;
    }
    
    /**
     * true if this assignment requires an honor pledge
     * @param honorPledge
     */
    public void setHonorPledge(Boolean honorPledge) {
        this.honorPledge = honorPledge;
    }
    
    /**
     * @return Instructions for this assignment
     */
    public String getInstructions() {
        return instructions;
    }
    
    /**
     * Instructions for this assignment
     * @param instructions
     */
    public void setInstructions(String instructions) {
        this.instructions = instructions;
    }
    
    /**
     * @return Returns equivalent int value of the submission type
     * ie inline only, inline and attachments, non-electronic, etc
     */
    public int getSubmissionType() {
        return submissionType;
    }
    
    /**
     * equivalent int value of the submission type
     * ie inline only, inline and attachments, non-electronic, etc
     * @param submissionType
     */
    public void setSubmissionType(int submissionType) {
        this.submissionType = submissionType;
    }
    
    /**
     * @return Returns the equivalent int value of the notification setting for this
     * assignment
     * ie Do not send me notifications of student submissions,
     * 		Send me notification for each student submission, etc
     */
    public int getNotificationType() {
        return notificationType;
    }

    /**
     * the equivalent int value of the notification setting for this
     * assignment
     * ie Do not send me notifications of student submissions,
     * 		Send me notification for each student submission, etc
     * @param notificationType
     */
    public void setNotificationType(int notificationType) {
        this.notificationType = notificationType;
    }
    
	/**
	 * 
	 * @return true if the user wants to add an announcement of the open
	 * date. this field may be true and the announcementId field null if
	 * the assignment is in draft status
	 */
	public Boolean getHasAnnouncement() {
		return hasAnnouncement;
	}

	/**
	 * true if the user wants to add an announcement of the open
	 * date. this field may be true and the announcementId field null if
	 * the assignment is in draft status
	 * @param hasAnnouncement
	 */
	public void setHasAnnouncement(Boolean hasAnnouncement) {
		this.hasAnnouncement = hasAnnouncement;
	}
    
    /**
     * @return Returns id of the announcement announcing the open date of this assignment.
     * If null, the announcement no announcement exists for this event in the 
     * announcements tool
     */
    public String getAnnouncementId() {
        return announcementId;
    }
    
    /**
     * id of the announcement announcing the open date of this assignment.
     * If null, the announcement no announcement exists for this event in the 
     * announcements tool
     * @param announcementId
     */
    public void setAnnouncementId(String announcementId) {
        this.announcementId = announcementId;
    }
    
    /**
     * @return Returns the id of the calendar event with this assignment's due date
     * If null, there is not a calendar event for this item
     */
    public String getCalendarEventId() {
        return calendarEventId;
    }

    /**
     * the id of the calendar event with this assignment's due date
     * If null, there is not a calendar event for this item
     * @param calendarEventId
     */
    public void setCalendarEventId(String calendarEventId) {
        this.calendarEventId = calendarEventId;
    }
    
    /**
     * @return If true, will allow students to resubmit an unlimited number of times
     * until the due date
     */
    public Boolean isAllowResubmitUntilDue() {
        return allowResubmitUntilDue;
    }
    
    /**
     * If true, will allow students to resubmit an unlimited number of times
     * until the due date
     * @param allowResubmitUntilDue
     */
    public void setAllowResubmitUntilDue(Boolean allowResubmitUntilDue) {
        this.allowResubmitUntilDue = allowResubmitUntilDue;
    }
    
    /**
     * @return If true, this assignment allows a review service (ie TurnItIn)
     */
    public Boolean isAllowReviewService() {
        return allowReviewService;
    }

    /**
     * If true, this assignment allows a review service (ie TurnItIn)
     * @param allowReviewService
     */
    public void setAllowReviewService(Boolean allowReviewService) {
        this.allowReviewService = allowReviewService;
    }
    
    /**
     * @return If true, students are allowed to view the review service report
     */
    public Boolean isAllowStudentViewReport() {
        return allowStudentViewReport;
    }

    /**
     * If true, students are allowed to view the review service report
     * @param allowStudentViewReport
     */
    public void setAllowStudentViewReport(Boolean allowStudentViewReport) {
        this.allowStudentViewReport = allowStudentViewReport;
    }
    
    /**
     * @return User id of this assignment's creator
     */
    public String getCreator() {
        return creator;
    }
    
    /**
     * User id of this assignment's creator
     * @param creator
     */
    public void setCreator(String creator) {
        this.creator = creator;
    }
    
    /**
     * @return Time this assignment was created
     */
    public Date getCreateTime() {
        return createTime;
    }

    /**
     * Time this assignment was created
     * @param createTime
     */
    public void setCreateTime(Date createTime) {
        this.createTime = createTime;
    }
    
    /**
     * @return User id of the last modifier
     */
    public String getModifiedBy() {
        return modifiedBy;
    }

    /**
     * User id of the last modifier
     * @param modifiedBy
     */
    public void setModifiedBy(String modifiedBy) {
        this.modifiedBy = modifiedBy;
    }
    
    /**
     * @return Time this assignment was last modified
     */
    public Date getModifiedTime() {
        return modifiedTime;
    }

    /**
     * Time this assignment was last modified
     * @param modifiedTime
     */
    public void setModifiedTime(Date modifiedTime) {
        this.modifiedTime = modifiedTime;
    }
    
    /**
     * 
     * @return true if this assignment was deleted
     */
    public Boolean isRemoved() {
    	return removed;
    }
    
    /**
     * true if this assignment was deleted
     * @param removed
     */
    public void setRemoved(Boolean removed) {
    	this.removed = removed;
    }
    
    public int getVersion() {
    	return version;
    }
    
    public void setVersion(int version) {
    	this.version = version;
    }
    
    /**
     * 
     * @return Set of AssignmentAttachments associated with this assignment
     */
    public Set<AssignmentAttachment> getAttachmentSet() {
		return attachmentSet;
	}

    /**
     * 
     * @param attachmentSet
     * Set of AssignmentAttachments associated with this assignment
     */
	public void setAttachmentSet(Set<AssignmentAttachment> attachmentSet) {
		this.attachmentSet = attachmentSet;
	}

	/**
	 * 
	 * @return the AssignmentGroups that this assignment is restricted to
	 */
	public Set<AssignmentGroup> getAssignmentGroupSet() {
		return assignmentGroupSet;
	}

	/**
	 * 
	 * @param assignmentGroupSet
	 * the AssignmentGroups that this assignment is restricted to
	 */
	public void setAssignmentGroupSet(Set<AssignmentGroup> assignmentGroupSet) {
		this.assignmentGroupSet = assignmentGroupSet;
	}

	// the following fields are populated using data from the gradebook
	
	/**
	 * @return the points possible value for this assignment.  this value 
	 * actually comes from the points possible value in the associated gb item.
	 * null if this assignment is ungraded.
	 */
	public Double getPointsPossible() {
		return pointsPossible;
	}

	/**
	 * the points possible value for this assignment.  this value 
	 * actually sets the points possible value in the associated gb item. null if
	 * this assignment is ungraded
	 * @param pointsPossible
	 */
	public void setPointsPossible(Double pointsPossible) {
		this.pointsPossible = pointsPossible;
	}

	/**
	 * 
	 * @return the due date for this assignment.  actually returns the due
	 * date of the associated gb item.  null if ungraded or
	 * if there is no due date.
	 */
	public Date getDueDate() {
		return dueDate;
	}
	
	/**
	 * set the due date for this assignment.  Actually updates the due date
	 * for the associated gb item.  Null if ungraded or there is no due date.
	 * @param dueDate
	 */
	public void setDueDate(Date dueDate) {
		this.dueDate = dueDate;
	}
	
	// the following fields are not persisted but are used by the UI
	
	/**
	 * @return the number of submissions to date for this assignment
	 */
	public Integer getNumberOfTotalSubmissions() {
		return numberOfTotalSubmissions;
	}

	/**
	 * set the number of submissions to date for this assignment
	 * @param numberOfTotalSubmissions
	 */
	public void setNumberOfTotalSubmissions(Integer numberOfTotalSubmissions) {
		this.numberOfTotalSubmissions = numberOfTotalSubmissions;
	}

	/**
	 * 
	 * @return the number of submissions for this assignments that have not had
	 * any grading action taken on them (ie graded, returned, etc)
	 */
	public Integer getNumberOfNewSubmissions() {
		return numberOfNewSubmissions;
	}

	/**
	 * set the number of submissions for this assignments that have not had
	 * any grading action taken on them (ie graded, returned, etc)
	 * @param numberOfNewSubmissions
	 */
	public void setNumberOfNewSubmissions(Integer numberOfNewSubmissions) {
		this.numberOfNewSubmissions = numberOfNewSubmissions;
	}

	/**
	 * 
	 * @return Integer representation of this assignment's status:
	 * ie Draft, Not Open, Open, Closed, Due
	 * see AssignmentConstants for the equivalency
	 */
	public Integer getStatus() {
		return status;
	}

	/**
	 * Set Integer representation of this assignment's status:
	 * ie Draft, Not Open, Open, Closed, Due
	 * see AssignmentConstants for the equivalency
	 * @param assignmentStatus
	 */
	public void setStatus(Integer status) {
		this.status = status;
	}
	
	/**
	 * 
	 * @return a comma-delimited list of the names of the groups/sections
	 * that this site is restricted to. null if not restricted
	 */
	public String getGroupRestrictionList() {
		return groupRestrictionList;
	}

	/**
	 * a comma-delimited list of the names of the groups/sections
	 * that this site is restricted to. null if not restricted
	 * @param groupRestrictionList
	 */
	public void setGroupRestrictionList(String groupRestrictionList) {
		this.groupRestrictionList = groupRestrictionList;
	}

}
