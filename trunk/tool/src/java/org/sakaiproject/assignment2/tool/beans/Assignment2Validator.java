/**********************************************************************************
 * $URL$
 * $Id$
 ***********************************************************************************
 *
 * Copyright (c) 2007, 2008 The Sakai Foundation.
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

package org.sakaiproject.assignment2.tool.beans;

import org.sakaiproject.assignment2.model.Assignment2;

import uk.org.ponder.messageutil.TargettedMessage;
import uk.org.ponder.messageutil.TargettedMessageList;

/**
 * This holds Validation utilities for the Assignments objects, so the checks
 * are in one place. This is mostly used for form verification in the GUI,
 * so an error message can be thrown up if the user forgets something like
 * the assignment title.
 * 
 * This uses the standard RSF error handling, so any errors it finds are added
 * to the targetted message list which includes the human readable message
 * keys to present to the user.
 * 
 * @author rjlowe
 * @author sgithens
 *
 */
public class Assignment2Validator  {

    /**
     * Validates the Assignment2 object. Currently checks to make sure there
     * is a title, some grading constraints, and some due date and accept date
     * constraints.
     * 
     * @param assignment The assignment to validate.
     * @param messages The message list to add any error/success messages to.
     * @return Whether or not this assignment object passed validation.
     */
    public boolean validate(Assignment2 assignment, TargettedMessageList messages) {

        boolean valid = true;
        String key = "";
        if (assignment.getId() == null){
            key = "new 1";
        } else {
            key = assignment.getId().toString();
        }


        //check for empty title
        if (assignment.getTitle() == null || assignment.getTitle().equals("")) {
            messages.addMessage(new TargettedMessage("assignment2.assignment_title_empty",
                    new Object[] { assignment.getTitle() }, "Assignment2." + key + ".title"));
            valid = false;
        }

        //check for graded but no gradebookItemId
        if (assignment.isGraded() && (assignment.getGradebookItemId() == null || 
                assignment.getGradebookItemId().longValue() < 1)) {
            messages.addMessage(new TargettedMessage("assignment2.assignment_graded_no_gb_item", 
                    new Object[] {}, "Assignment2."+ key + ".gradebookItemId"));
            valid = false;
        }

        // check for due date after open date
        if (assignment.getDueDate() != null && assignment.getOpenDate() != null
                && assignment.getDueDate().before(assignment.getOpenDate())) {
            messages.addMessage(new TargettedMessage("assignment2.assignment_due_before_open"));
            valid = false;
        }

        if (assignment.getAcceptUntilDate() != null && assignment.getOpenDate() != null
                && assignment.getAcceptUntilDate().before(assignment.getOpenDate())) {
            messages.addMessage(new TargettedMessage("assignment2.assignment_accept_before_open"));
            valid = false;
        }

        // check for due date before or equal to accept until
        if (assignment.getDueDate() != null && assignment.getAcceptUntilDate() != null
                && assignment.getAcceptUntilDate().before(assignment.getDueDate())) {
            messages.addMessage(new TargettedMessage("assignment2.assignment_accept_before_due"));
            valid = false;
        }

        return valid;
    }

}