/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/api/logic/src/java/org/sakaiproject/assignment2/logic/ExternalGradebookLogic.java $
 * $Id: ExternalGradebookLogic.java 12544 2006-05-03 15:06:26Z wagnermr@iupui.edu $
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
    private Integer sortIndex;
    private Date openDate;
    private Date acceptUntilDate;
    private String associatedGbItemName;
    private Double associatedGbItemPtsPossible;
    private boolean ungraded;
    private boolean draft;
    private Date dueDate;
    private boolean honorPledge;
    private String instructions;
    private boolean hasAnnouncement;
    private Integer numSubmissionsAllowed;
    private Integer notificationType;
    private Integer submissionType;
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


	public Integer getSortIndex() {
		return sortIndex;
	}


	public void setSortIndex(Integer sortIndex) {
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


	public boolean isUngraded() {
		return ungraded;
	}


	public void setUngraded(boolean ungraded) {
		this.ungraded = ungraded;
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


	public Integer getNumSubmissionsAllowed() {
		return numSubmissionsAllowed;
	}


	public void setNumSubmissionsAllowed(Integer numSubmissionsAllowed) {
		this.numSubmissionsAllowed = numSubmissionsAllowed;
	}


	public Integer getNotificationType() {
		return notificationType;
	}


	public void setNotificationType(Integer notificationType) {
		this.notificationType = notificationType;
	}


	public Integer getSubmissionType() {
		return submissionType;
	}


	public void setSubmissionType(Integer submissionType) {
		this.submissionType = submissionType;
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
