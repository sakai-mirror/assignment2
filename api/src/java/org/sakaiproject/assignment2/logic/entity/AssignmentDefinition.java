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
package org.sakaiproject.assignment2.logic.entity;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * Object to model Assignment2 data that will be used for
 * import/export
 */
public class AssignmentDefinition implements Serializable {
    private static final long serialVersionUID = 1L;

    private String title;
    private int sortIndex;
    private Date openDate;
    private Date acceptUntilDate;
    private String associatedGbItemName;
    private Double associatedGbItemPtsPossible;
    private boolean graded;
    private boolean draft;
    private Date dueDate;
    private boolean honorPledge;
    private String instructions;
    private boolean hasAnnouncement;
    private boolean addedToSchedule;
    private int numSubmissionsAllowed;
    private boolean sendSubmissionNotifications;
    private int submissionType;
    private boolean requiresSubmission;

    private List<String> groupRestrictionGroupTitles;
    private List<String> attachmentReferences;


    public AssignmentDefinition() {
    }


    public String getTitle() {
        return title;
    }


    public void setTitle(String title) {
        this.title = title;
    }


    public int getSortIndex() {
        return sortIndex;
    }


    public void setSortIndex(int sortIndex) {
        this.sortIndex = sortIndex;
    }


    public Date getOpenDate() {
        return openDate;
    }


    public void setOpenDate(Date openDate) {
        this.openDate = openDate;
    }


    public Date getAcceptUntilDate() {
        return acceptUntilDate;
    }


    public void setAcceptUntilDate(Date acceptUntilDate) {
        this.acceptUntilDate = acceptUntilDate;
    }


    public String getAssociatedGbItemName() {
        return associatedGbItemName;
    }


    public void setAssociatedGbItemName(String associatedGbItemName) {
        this.associatedGbItemName = associatedGbItemName;
    }



    public Double getAssociatedGbItemPtsPossible() {
        return associatedGbItemPtsPossible;
    }


    public void setAssociatedGbItemPtsPossible(Double associatedGbItemPtsPossible) {
        this.associatedGbItemPtsPossible = associatedGbItemPtsPossible;
    }


    public boolean isGraded() {
        return graded;
    }


    public void setGraded(boolean graded) {
        this.graded = graded;
    }


    public boolean isDraft() {
        return draft;
    }


    public void setDraft(boolean draft) {
        this.draft = draft;
    }


    public Date getDueDate() {
        return dueDate;
    }


    public void setDueDate(Date dueDate) {
        this.dueDate = dueDate;
    }


    public boolean isHonorPledge() {
        return honorPledge;
    }


    public void setHonorPledge(boolean honorPledge) {
        this.honorPledge = honorPledge;
    }


    public String getInstructions() {
        return instructions;
    }


    public void setInstructions(String instructions) {
        this.instructions = instructions;
    }


    public boolean isHasAnnouncement() {
        return hasAnnouncement;
    }


    public void setHasAnnouncement(boolean hasAnnouncement) {
        this.hasAnnouncement = hasAnnouncement;
    }


    public boolean isAddedToSchedule()
    {
        return addedToSchedule;
    }


    public void setAddedToSchedule(boolean addedToSchedule)
    {
        this.addedToSchedule = addedToSchedule;
    }


    public int getNumSubmissionsAllowed() {
        return numSubmissionsAllowed;
    }


    public void setNumSubmissionsAllowed(int numSubmissionsAllowed) {
        this.numSubmissionsAllowed = numSubmissionsAllowed;
    }


    public boolean isSendSubmissionNotifications() {
        return sendSubmissionNotifications;
    }


    public void setSendSubmissionNotifications(boolean sendSubmissionNotifications) {
        this.sendSubmissionNotifications = sendSubmissionNotifications;
    }


    public int getSubmissionType() {
        return submissionType;
    }


    public void setSubmissionType(int submissionType) {
        this.submissionType = submissionType;
    }

    public boolean isRequiresSubmission()
    {
        return requiresSubmission;
    }


    public void setRequiresSubmission(boolean requiresSubmission)
    {
        this.requiresSubmission = requiresSubmission;
    }


    public List<String> getGroupRestrictionGroupTitles() {
        return groupRestrictionGroupTitles;
    }


    public void setGroupRestrictionGroupTitles(
            List<String> groupRestrictionGroupTitles) {
        this.groupRestrictionGroupTitles = groupRestrictionGroupTitles;
    }


    public List<String> getAttachmentReferences() {
        return attachmentReferences;
    }


    public void setAttachmentReferences(List<String> attachmentReferences) {
        this.attachmentReferences = attachmentReferences;
    }

}
