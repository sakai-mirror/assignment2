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

package org.sakaiproject.assignment2.logic.utils;

import java.util.Comparator;
import java.util.Date;

import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;


/**
 * Utilities for sorting Assignment2 and AssignmentSubmission objects
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class ComparatorsUtils {

	/**
	 * static class to sort Assignment2 objects by due date
	 */
	public static class Assignment2DueDateComparator implements Comparator<Assignment2>  {
		public int compare(Assignment2 assign1, Assignment2 assign2) {
			Date dueDate1 = assign1.isUngraded() ? assign1.getDueDateForUngraded() : assign1.getDueDate();
			Date dueDate2 = assign2.isUngraded() ? assign2.getDueDateForUngraded() : assign2.getDueDate();

			int value = dueDate1.compareTo(dueDate2);
			if (value == 0) {
				value = sortByTitle(assign1, assign2);
			}
			return value;
		}
	}

	/**
	 * static class to sort Assignment2 objects by title
	 */
	public static class Assignment2TitleComparator implements Comparator<Assignment2>  {
		public int compare(Assignment2 assign1, Assignment2 assign2) {
			String title1 = assign1.getTitle() != null ? assign1.getTitle().toLowerCase() : null;
			String title2 = assign2.getTitle() != null ? assign2.getTitle().toLowerCase() : null;
			return title1.compareTo(title2);
		}
	}

	/**
	 * static class to sort Assignment2 objects by open date
	 */
	public static class Assignment2OpenDateComparator implements Comparator<Assignment2> {
		public int compare(Assignment2 assign1, Assignment2 assign2) {
			int value = assign1.getOpenTime().compareTo(assign2.getOpenTime());
			if (value == 0) {
				value = sortByTitle(assign1, assign2);
			}
			return value;
		}
	}

	/**
	 * static class to sort Assignment2 objects by status
	 */
	public static class Assignment2StatusComparator implements Comparator<Assignment2> {
		public int compare(Assignment2 assign1, Assignment2 assign2) {
			int value = assign1.getAssignmentStatus().compareTo(assign2.getAssignmentStatus());
			if (value == 0) {
				value = sortByTitle(assign1, assign2);
			}
			return value;
		}
	}

	/**
	 * static class to sort Assignment2 objects by sort index
	 */
	public static class Assignment2SortIndexComparator implements Comparator<Assignment2> {
		public int compare(Assignment2 assign1, Assignment2 assign2) {
			int value = 0;

			if (assign1.getSortIndex() > assign2.getSortIndex()) {
				value = 1;
			} else if (assign1.getSortIndex() < assign2.getSortIndex()) {
				value = -1;
			} else {
				value = sortByTitle(assign1, assign2);
			}

			return value;
		}
	}

	/**
	 * static class to sort Assignment2 objects by "for" column - this will be
	 * Site or a list of the group restrictions
	 */
	public static class Assignment2ForComparator implements Comparator<Assignment2> {
		public int compare(Assignment2 assign1, Assignment2 assign2) {
			int value = assign1.getRestrictedToText().compareTo(assign2.getRestrictedToText());
			if (value == 0) {
				value = sortByTitle(assign1, assign2);
			}
			return value;
		}
	}

	/**
	 * static class to sort Assignment2 objects by num ungraded submissions
	 */
	public static class Assignment2NumUngradedComparator implements Comparator<Assignment2> {
		public int compare(Assignment2 assign1, Assignment2 assign2) {
			int value = 0;

			if (assign1.getNumberOfUngradedSubmissions() > assign2.getNumberOfUngradedSubmissions()) {
				value = 1;
			} else if (assign1.getNumberOfUngradedSubmissions() < assign2.getNumberOfUngradedSubmissions()) {
				value = -1;
			} else {
				value = sortByTitle(assign1, assign2);
			}

			return value;
		}
	}
	
	private static int sortByTitle(Assignment2 assign1, Assignment2 assign2) {
		String title1 = assign1.getTitle() != null ? assign1.getTitle().toLowerCase() : null;
		String title2 = assign2.getTitle() != null ? assign2.getTitle().toLowerCase() : null;
		return title1.compareTo(title2);
	}

}
