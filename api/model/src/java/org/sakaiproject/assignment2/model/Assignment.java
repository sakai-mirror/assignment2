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

/**
 * The Assignment object
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class Assignment {

    private String assignmentId;
    private Long gradableObjectId;
    private String siteId;
    private String title;
    private boolean draft;
    private int sortIndex;
    private Date openTime;
    private Date closeTime;
    private Date dropDeadTime;
    private boolean ungraded;
    private Date dueDateForUngraded;
    private boolean restrictedToGroups;
    private boolean groupSubmission;
    private boolean honorPledge;
    private String instructions;
    private int submissionType;
    private int notificationType;
    private String announcementId;
    private String calendarEventId;
    private boolean allowResubmitUntilDue;
    private boolean allowReviewService;
    private boolean allowStudentViewReport;
    private String creator;
    private Date createTime;
    private String modifiedBy;
    private Date modifiedTime;


    /**
     * Default constructor
     */
    public Assignment() {
    }

    /**
     * Getters and Setters
     */
    
    /**
     * @return Returns the assignment id
     */
    public String getAssignmentId() {
        return assignmentId;
    }
    
    /**
     * set the assignment id
     * @param assignmentId
     */
    public void setAssignmentId(String assignmentId) {
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
     * @return Returns the site id
     */
    public String getSiteId() {
        return siteId;
    }
    
    /**
     * set the siteId
     * @param siteId
     */
    public void setSiteId(String siteId) {
        this.siteId = siteId;
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
    public boolean getDraft() {
        return draft;
    }
    
    /**
     * draft status
     * @param draft
     */
    public void setDraft(boolean draft) {
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
     * @return The time after which the assignment is closed
     */
    public Date getCloseTime() {
        return closeTime;
    }
    
    /**
     * The time after which the assignment is closed
     * @param closeTime
     */
    public void setCloseTime(Date closeTime) {
        this.closeTime = closeTime;
    }
    
    /**
     * @return The time after which responses to this assignment are considered late
     */
    public Date getDropDeadTime() {
        return dropDeadTime;
    }
    
    /**
     * The time after which responses to this assignment are considered late
     * @param dropDeadTime
     */
    public void setDropDeadTime(Date dropDeadTime) {
        this.dropDeadTime = dropDeadTime;
    }
    
    /**
     * All assignments will be linked to the gradebook and store grade
     * information in the gradebook tables except ungraded assignments.  
     * @return true if this assignment is ungraded
     */
    public boolean getUngraded() {
    	return ungraded;
    }
    
    /**
     * All assignments will be linked to the gradebook and store grade
     * information in the gradebook tables except ungraded assignments.  
     * @param ungraded
     */
    public void setUngraded(boolean ungraded) {
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
    public boolean getRestrictedToGroups() {
        return restrictedToGroups;
    }
    
    /**
     * true if viewing this assignment is restricted to members
     * of specific group(s)
     * @param restrictedToGroups
     */
    public void setRestrictedToGroups(boolean restrictedToGroups) {
        this.restrictedToGroups = restrictedToGroups;
    }
    
    /**
     * @return Returns true if this assignment will be submitted by a "group", not the
     * individual students in the group
     */
    public boolean getGroupSubmission() {
        return groupSubmission;
    }
    
    /**
     * true if this assignment will be submitted by a "group", not the
     * individual students in the group
     * @param groupSubmission
     */
    public void setGroupSubmission(boolean groupSubmission) {
        this.groupSubmission = groupSubmission;
    }
    
    /**
     * @return Returns true if this assignment requires an honor pledge
     */
    public boolean getHonorPledge() {
        return honorPledge;
    }
    
    /**
     * true if this assignment requires an honor pledge
     * @param honorPledge
     */
    public void setHonorPledge(boolean honorPledge) {
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
     * @return Returns id of the announcement announcing the open date of this assignment.
     * If null, the announcement option was not selected
     */
    public String getAnnouncementId() {
        return announcementId;
    }
    
    /**
     * id of the announcement announcing the open date of this assignment.
     * If null, the announcement option was not selected
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
    public boolean getAllowResubmitUntilDue() {
        return allowResubmitUntilDue;
    }
    
    /**
     * If true, will allow students to resubmit an unlimited number of times
     * until the due date
     * @param allowResubmitUntilDue
     */
    public void setAllowResubmitUntilDue(boolean allowResubmitUntilDue) {
        this.allowResubmitUntilDue = allowResubmitUntilDue;
    }
    
    /**
     * @return If true, this assignment allows a review service (ie TurnItIn)
     */
    public boolean getAllowReviewService() {
        return allowReviewService;
    }

    /**
     * If true, this assignment allows a review service (ie TurnItIn)
     * @param allowReviewService
     */
    public void setAllowReviewService(boolean allowReviewService) {
        this.allowReviewService = allowReviewService;
    }
    
    /**
     * @return If true, students are allowed to view the review service report
     */
    public boolean getAllowStudentViewReport() {
        return allowStudentViewReport;
    }

    /**
     * If true, students are allowed to view the review service report
     * @param allowStudentViewReport
     */
    public void setAllStudentViewReport(boolean allowStudentViewReport) {
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
}
