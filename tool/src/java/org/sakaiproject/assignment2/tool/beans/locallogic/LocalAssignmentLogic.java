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

package org.sakaiproject.assignment2.tool.beans.locallogic;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.exception.AnnouncementPermissionException;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.utils.ComparatorsUtils;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.tool.beans.Assignment2Bean;

import uk.org.ponder.messageutil.MessageLocator;

/**
 * Contains logic methods that are used by the ui.
 */
public class LocalAssignmentLogic {
	
	private static final Log LOG = LogFactory.getLog(LocalAssignmentLogic.class);
	
	private ExternalLogic externalLogic;
	public void setExternalLogic(ExternalLogic externalLogic) {
		this.externalLogic = externalLogic;
	}
	
	private AssignmentLogic assignmentLogic;
	public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
		this.assignmentLogic = assignmentLogic;
	}
	
	private MessageLocator messageLocator;
	public void setMessageLocator (MessageLocator messageLocator) {
		this.messageLocator = messageLocator;
	}
	
	/**
	 * 
	 * @param groups
	 * @return a comma-delimited String representation of the given list of
	 * groups/section. 
	 */
	public String getListOfGroupRestrictionsAsString(List<AssignmentGroup> restrictedGroups, Map<String, String> siteGroupIdNameMap) {
		StringBuilder sb = new StringBuilder();
		
		if (restrictedGroups != null) {
			List<String> groupNameList = new ArrayList();
			
			for (Iterator groupIter = restrictedGroups.iterator(); groupIter.hasNext();) {
				AssignmentGroup group = (AssignmentGroup) groupIter.next();
				if (group != null) {
					if (siteGroupIdNameMap.containsKey(group.getGroupId())) {
						String groupName = (String)siteGroupIdNameMap.get(group.getGroupId());
						groupNameList.add(groupName);
					}
				}
			}
			
			Collections.sort(groupNameList);
			
			for (int i=0; i < groupNameList.size(); i++) {
				
				String groupName = (String) groupNameList.get(i);
				if (groupName != null) {
					if (i != 0) {
						sb.append(", ");
					}

					sb.append(groupName);
				}
			}	
		}
		
		return sb.toString();
	}
	
	public void handleAnnouncement(Assignment2 newAssignment, Assignment2 oldAssignment) {
		String newAnncSubject = messageLocator.getMessage("assignment2.assignment_annc_subject", new Object[] {newAssignment.getTitle()});
		String newAnncBody = messageLocator.getMessage("assignment2.assignment_annc_body", new Object[] {newAssignment.getOpenTime()});
		String revAnncSubject = messageLocator.getMessage("assignment2.assignment_annc_subject_edited", new Object[] {newAssignment.getTitle()});
		String revAnncBody = messageLocator.getMessage("assignment2.assignment_annc_subject_edited", new Object[] {newAssignment.getOpenTime()});

		try {
			assignmentLogic.saveAssignmentAnnouncement(oldAssignment, newAssignment, newAnncSubject, 
					newAnncBody, revAnncSubject, revAnncBody);
		} catch (AnnouncementPermissionException ape) {
			LOG.error(ape.getMessage(), ape);
			// TODO do something since the assignment was saved but
			// the announcement was not added b/c user doesn't have
			// perm in the announcements tool
		}
	}
	
	public void populateNonPersistedFieldsForAssignments(List<Assignment2> assignmentList) {
		if (assignmentList == null || assignmentList.isEmpty())
			return;
		
		// Now, iterate through the viewable assignments and set the not persisted fields 
		// that aren't related to the gradebook
		
		// create a map of group id to name for all of the groups in this site
		Map groupIdToNameMap = externalLogic.getGroupIdToNameMapForSite(externalLogic.getCurrentContextId());
		
		for (Iterator assignIter = assignmentList.iterator(); assignIter.hasNext();) {
			Assignment2 assign = (Assignment2) assignIter.next();
			if (assign != null) {

				// first, populate the text for the "For" column based upon group restrictions
				if (assign.getAssignmentGroupSet() != null && !assign.getAssignmentGroupSet().isEmpty()) {
					String groupListAsString = getListOfGroupRestrictionsAsString(
							new ArrayList(assign.getAssignmentGroupSet()), groupIdToNameMap);
					assign.setRestrictedToText(groupListAsString);
				} 
				else {
					assign.setRestrictedToText(messageLocator.getMessage("assignment2.assignment_restrict_to_site"));
				}

				// set the status for this assignment: "Open, Due, etc"
				int status = assignmentLogic.getStatusForAssignment(assign);
				assign.setAssignmentStatus(messageLocator.getMessage("assignment2.status." + status));
				
				if (assign.getSubmissionStatusConstant() != null) {
					assign.setSubmissionStatus(messageLocator.getMessage("assignment2.submission_status." + assign.getSubmissionStatusConstant().intValue()));
				}
			}
		}
	}
	
	public List filterListForPaging(List myList, int begIndex, int numItemsToDisplay) {
        if (myList == null || myList.isEmpty())
        	return myList;
        
        int endIndex = begIndex + numItemsToDisplay;
        if (endIndex > myList.size()) {
        	endIndex = myList.size();
        }

		return myList.subList(begIndex, endIndex);
	}
	
	/**
	 * Will apply paging and sorting to the given list and populate any non-persisted
	 * fields that need to be populated from the UI (ie fields that require access
	 * to the bundle)
	 * @param assignmentList
	 * @param currentStart
	 * @param currentCount
	 * @param sortBy
	 * @param sortDir
	 */
	public void filterPopulateAndSortAssignmentList(List<Assignment2> assignmentList, int currentStart, int currentCount, String sortBy, boolean sortDir) {
		assignmentList = filterListForPaging(assignmentList, currentStart, currentCount);
        populateNonPersistedFieldsForAssignments(assignmentList);
        sortAssignments(assignmentList, sortBy, sortDir);
	}
	
	/**
	 * We cannot rely on db sorting because we must sort by several properties that
	 * are not persisted in the A2 tables (ie status, due date, for, etc)
	 * @param assignmentList
	 * @param sortBy
	 * @param ascending
	 */
	public void sortAssignments(List<Assignment2> assignmentList, String sortBy, boolean ascending) {
		Comparator<Assignment2> comp;
		if(AssignmentLogic.SORT_BY_TITLE.equals(sortBy)) {
			comp = new ComparatorsUtils.Assignment2TitleComparator();
		} else if(AssignmentLogic.SORT_BY_DUE.equals(sortBy)) {
			comp = new ComparatorsUtils.Assignment2DueDateComparator();
		} else if(AssignmentLogic.SORT_BY_FOR.equals(sortBy)) {
			comp = new ComparatorsUtils.Assignment2ForComparator();
		} else if(AssignmentLogic.SORT_BY_OPEN.equals(sortBy)){
			comp = new ComparatorsUtils.Assignment2OpenDateComparator();
		} else if(AssignmentLogic.SORT_BY_STATUS.equals(sortBy)){
			comp = new ComparatorsUtils.Assignment2StatusComparator();
		} else {
			comp = new ComparatorsUtils.Assignment2SortIndexComparator();
		}

		Collections.sort(assignmentList, comp);
		if(!ascending) {
			Collections.reverse(assignmentList);
		}
	}
}