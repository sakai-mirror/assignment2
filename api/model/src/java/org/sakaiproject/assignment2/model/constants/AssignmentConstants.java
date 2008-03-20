/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/api/model/src/java/org/sakaiproject/assignment2/model/constants/AssignmentConstants.java $
 * $Id: AssignmentConstants.java 8802 2006-05-03 15:06:26Z wagnermr@iupui.edu $
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

package org.sakaiproject.assignment2.model.constants;

/**
* @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
*/
public class AssignmentConstants {
	
	/**
	 * Used to indicate that the assignment may only be viewed, not graded by
	 * the specified user
	 */
	public final static String VIEW = "view";
	
	/**
	 * Used to indicate that the assignment may be viewed and graded by the
	 * specified user
	 */
	public final static String GRADE = "grade";

	// Notification Types
	/**
	 * Do not send notification emails for any submissions
	 */
	public final static int NOTIFY_NONE = 0;
	/**
	 * Send a notification email for each submission
	 */
	public final static int NOTIFY_FOR_EACH = 1;
	/**
	 * Send me one email per day summarizing notifications for submissions
	 */
	public final static int NOTIFY_DAILY_SUMMARY = 2;
	
	// Submission Types
	/**
	 * Submissions may only be inline
	 */
	public final static int SUBMIT_INLINE_ONLY = 0;
	/**
	 * Submissions may only be attachments
	 */
	public final static int SUBMIT_ATTACH_ONLY = 1;
	/**
	 * Submissions may be inline and/or attachments
	 */
	public final static int SUBMIT_INLINE_AND_ATTACH = 2;
	/**
	 * Submissions will be non-electronic
	 */
	public final static int SUBMIT_NON_ELECTRONIC = 3;
	
	// Assignment status
	/**
	 * This assignment is in draft status
	 */
	public final static int STATUS_DRAFT = 0;
	/**
	 * The assignment is not draft status but the current date is prior to
	 * the open date
	 */
	public final static int STATUS_NOT_OPEN = 1;
	/**
	 * The assignment is not draft. The current date is after the open date
	 * but prior to the "accept until" date and due date.
	 */
	public final static int STATUS_OPEN = 2;
	/**
	 * The assignment is not draft. The current date is after the "accept
	 * until" date.
	 */
	public final static int STATUS_CLOSED = 3;
	/**
	 * The assignment is not draft. The current date is after the open and
	 * due dates but before the "accept until" date.
	 */
	public final static int STATUS_DUE = 4;
	
	// Submission status
	/**
	 * Submission has not been started
	 */
	public final static int SUBMISSION_NOT_STARTED = 0;
	
	/**
	 * Submission has been saved (draft) but not submitted
	 */
	public final static int SUBMISSION_IN_PROGRESS = 1;
	
	/**
	 * Student has made a submission
	 */
	public final static int SUBMISSION_SUBMITTED = 2;
	
	/**
	 * Student made a submission but it was submitted after the due date
	 */
	public final static int SUBMISSION_LATE = 3;

	// Security functions & locks
	/** Security function giving the user permission to receive assignment submission email */
	public static final String SECURE_ASSIGNMENT_RECEIVE_NOTIFICATIONS = "asn.receive.notifications";

	/** Security lock for adding an assignment. */
	public static final String SECURE_ADD_ASSIGNMENT = "asn.new";

	/** Security lock for adding an assignment. */
	public static final String SECURE_ADD_ASSIGNMENT_CONTENT = "asn.new";

	/** Security lock for adding an assignment submission. */
	public static final String SECURE_ADD_ASSIGNMENT_SUBMISSION = "asn.submit";

	/** Security lock for removing an assignment. */
	public static final String SECURE_REMOVE_ASSIGNMENT = "asn.delete";

	/** Security lock for removing an assignment content. */
	public static final String SECURE_REMOVE_ASSIGNMENT_CONTENT = "asn.delete";

	/** Security lock for removing an assignment submission. */
	public static final String SECURE_REMOVE_ASSIGNMENT_SUBMISSION = "asn.delete";

	/** Security lock for accessing an assignment. */
	public static final String SECURE_ACCESS_ASSIGNMENT = "asn.read";

	/** Security lock for accessing an assignment content. */
	public static final String SECURE_ACCESS_ASSIGNMENT_CONTENT = "asn.read";

	/** Security lock for accessing an assignment submission. */
	public static final String SECURE_ACCESS_ASSIGNMENT_SUBMISSION = "asn.submit";

	/** Security lock for updating an assignment. */
	public static final String SECURE_UPDATE_ASSIGNMENT = "asn.revise";

	/** Security lock for updating an assignment submission. */
	public static final String SECURE_UPDATE_ASSIGNMENT_SUBMISSION = "asn.submit";

	/** Security lock for grading submission */
	public static final String SECURE_GRADE_ASSIGNMENT_SUBMISSION = "asn.grade";

	/** Security function giving the user permission to all groups, if granted to at the site level. */
	public static final String SECURE_ALL_GROUPS = "asn.all.groups";

	/** Grade type not set */
	public static final int GRADE_TYPE_NOT_SET = -1;

	/** Ungraded grade type */
	public static final int GRADE_TYPE_UNGRADED = 1;

	/** Letter grade type */
	public static final int GRADE_TYPE_LETTER = 2;

	/** Score based grade type */
	public static final int GRADE_TYPE_SCORE = 3;

	/** Pass/fail grade type */
	public static final int GRADE_TYPE_PASS_FAIL = 4;

	/** Grade type that only requires a check */
	public static final int GRADE_TYPE_CHECK = 5;

	public enum GradeType
	{
		NOT_SET(-1), UNGRADED(1), LETTER(2), SCORE(3), PASS_FAIL(4), CHECK(5);

		private int value = -1;
		GradeType(int value)
		{
			this.value = value;
		}

		public int getValue()
		{
			return value;
		}
	}
	
	
	public static final String REFERENCE_ROOT = "asgn2";
	public static final String ASSIGNMENT_TYPE = "a";
    public static final String SUBMISSION_TYPE = "s";
}