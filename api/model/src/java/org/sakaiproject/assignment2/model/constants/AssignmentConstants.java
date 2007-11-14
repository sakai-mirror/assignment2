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
	public final static Integer STATUS_DRAFT = 0;
	/**
	 * The assignment is not draft status but the current date is prior to
	 * the open date
	 */
	public final static Integer STATUS_NOT_OPEN = 1;
	/**
	 * The assignment is not draft. The current date is after the open date
	 * but prior to the "accept until" date and due date.
	 */
	public final static Integer STATUS_OPEN = 2;
	/**
	 * The assignment is not draft. The current date is after the "accept
	 * until" date.
	 */
	public final static Integer STATUS_CLOSED = 3;
	/**
	 * The assignment is not draft. The current date is after the open and
	 * due dates but before the "accept until" date.
	 */
	public final static Integer STATUS_DUE = 4;
	
}
