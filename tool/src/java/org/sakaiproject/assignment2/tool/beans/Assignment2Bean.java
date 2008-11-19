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

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.tool.WorkFlowResult;
import org.sakaiproject.assignment2.tool.beans.Assignment2Validator;
import org.sakaiproject.user.api.UserNotDefinedException;
import org.sakaiproject.assignment2.exception.AnnouncementPermissionException;
import org.sakaiproject.assignment2.exception.StaleObjectModificationException;
import org.sakaiproject.exception.IdUnusedException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.messageutil.TargettedMessage;
import uk.org.ponder.messageutil.TargettedMessageList;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;

public class Assignment2Bean {

    public Assignment2 assignment = new Assignment2();
    public Date openDate;

    private static final Log LOG = LogFactory.getLog(Assignment2Bean.class);

    private static final String REMOVE = "remove";
    private static final String SAVE_REORDER = "save";

    public Long currentAssignmentId;

    private Boolean requireAcceptUntil;
    public void setRequireAcceptUntil(Boolean requireAcceptUntil) {
        this.requireAcceptUntil = requireAcceptUntil;
    }

    public Boolean requireDueDate;
    public void setRequireDueDate(Boolean requireDueDate) {
        this.requireDueDate = requireDueDate;
    }

    public Map<String, Boolean> selectedIds = new HashMap<String, Boolean>();
    public String restrictedToGroups;

    private TargettedMessageList messages;
    public void setMessages(TargettedMessageList messages) {
        this.messages = messages;
    }

    private AssignmentLogic logic;
    public void setLogic(AssignmentLogic logic) {
        this.logic = logic;
    }

    private Map<String, Assignment2> OTPMap;
    @SuppressWarnings("unchecked")
    public void setAssignment2EntityBeanLocator(EntityBeanLocator entityBeanLocator) {
        this.OTPMap = entityBeanLocator.getDeliveredBeans();
    }

    private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }

    private MessageLocator messageLocator;
    public void setMessageLocator (MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }

    private NotificationBean notificationBean;
    public void setNotificationBean (NotificationBean notificationBean) {
        this.notificationBean = notificationBean;
    }

    public WorkFlowResult processActionPost() {
        WorkFlowResult result = WorkFlowResult.INSTRUCTOR_POST_ASSIGNMENT;
        for (String key : OTPMap.keySet()) {
            Assignment2 assignment = OTPMap.get(key);
            result = internalProcessPost(assignment, key, Boolean.FALSE);

            // Notify students
            if (result.equals(WorkFlowResult.INSTRUCTOR_POST_ASSIGNMENT))
            {
                try
                {
                    notificationBean.notifyStudentsOfNewAssignment(assignment);
                }catch (IdUnusedException e)
                {
                    messages.addMessage(new TargettedMessage("assignment2.student-submit.error.unexpected",
                            new Object[] {e.getLocalizedMessage()}, TargettedMessage.SEVERITY_ERROR));
                }catch (UserNotDefinedException e)
                {
                    messages.addMessage(new TargettedMessage("assignment2.student-submit.error.unexpected",
                            new Object[] {e.getLocalizedMessage()}, TargettedMessage.SEVERITY_ERROR));
                }
            }
        }
        return result;
    }

    private WorkFlowResult internalProcessPost(Assignment2 assignment, String key, Boolean draft){
        Boolean errorFound = false;

        assignment.setDraft(draft);

        if (this.requireAcceptUntil == null || this.requireAcceptUntil == Boolean.FALSE) {
            assignment.setAcceptUntilDate(null);
        }

        if (this.requireDueDate == null || this.requireDueDate == Boolean.FALSE) {
            assignment.setDueDate(null);
        }

        //Since in the UI, the select box bound to the gradebookItemId is always present
        // we need to manually remove this value if the assignment is ungraded
        if (!assignment.isGraded()) {
            assignment.setGradebookItemId(null);
        }

        //do groups
        Set<AssignmentGroup> newGroups = new HashSet<AssignmentGroup>();
        if (this.restrictedToGroups != null && restrictedToGroups.equals(Boolean.TRUE.toString())){

            List<String> existingGroups = assignment.getListOfAssociatedGroupReferences();

            //now add any new groups
            if (assignment.getAssignmentGroupSet() != null) {
                newGroups.addAll(assignment.getAssignmentGroupSet());
            } 

            Set<AssignmentGroup> remGroups = new HashSet<AssignmentGroup>();
            for (String selectedId : selectedIds.keySet()) {
                if (selectedIds.get(selectedId) == Boolean.TRUE) {
                    // if it isn't already associated with this assignment, add it
                    if (existingGroups == null || (existingGroups != null && !existingGroups.contains(selectedId))) {
                        AssignmentGroup newGroup = new AssignmentGroup();
                        newGroup.setAssignment(assignment);
                        newGroup.setGroupId(selectedId);
                        newGroups.add(newGroup);
                    }
                } else if (selectedIds.get(selectedId) == Boolean.FALSE) {
                    //then remove the group
                    for (AssignmentGroup ag : newGroups) {
                        if (ag.getGroupId().equals(selectedId)) {
                            remGroups.add(ag);
                        }
                    }
                }
            }
            newGroups.removeAll(remGroups);
        }

        if (this.restrictedToGroups != null && restrictedToGroups.equals(Boolean.TRUE.toString()) && newGroups.size() < 1){
            messages.addMessage(new TargettedMessage("assignment2.assignment_post.no_groups"));
            errorFound = true;
        }
        assignment.setAssignmentGroupSet(newGroups);

        //start the validator
        Assignment2Validator validator = new Assignment2Validator();
        if (validator.validate(assignment, messages) && !errorFound){
            //Validation Passed!
            try {

                logic.saveAssignment(assignment);

            } catch (AnnouncementPermissionException ape) {
                if (LOG.isDebugEnabled()) LOG.debug("Announcement could not " +
                "be updated b/c user does not have perm in annc tool");
                //TODO display to user?
            }

            //set Messages
            if (draft) {
                messages.addMessage(new TargettedMessage("assignment2.assignment_save_draft",
                        new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_INFO));
            } else 	if (key.startsWith(EntityBeanLocator.NEW_PREFIX)) {
                messages.addMessage(new TargettedMessage("assignment2.assignment_post",
                        new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_INFO));
            }
            else {
                messages.addMessage(new TargettedMessage("assignment2.assignment_save",
                        new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_INFO));
            }
        } else {
            if (draft) {
                //messages.addMessage(new TargettedMessage("assignment2.assignment_save_draft_error"));
            } else {
                //messages.addMessage(new TargettedMessage("assignment2.assignment_post_error"));
            }
            return WorkFlowResult.INSTRUCTOR_ASSIGNMENT_FAILURE;
        }
        return WorkFlowResult.INSTRUCTOR_POST_ASSIGNMENT;
    }


    public WorkFlowResult processActionPreview() {
        WorkFlowResult returnCode = WorkFlowResult.INSTRUCTOR_ASSIGNMENT_FAILURE;
        
        for (String key : OTPMap.keySet()) {
            Assignment2 assignment = OTPMap.get(key);
            if (this.requireAcceptUntil == null || Boolean.FALSE.equals(requireAcceptUntil)) {
                assignment.setAcceptUntilDate(null);
            }
            if (this.requireDueDate == null || this.requireDueDate == Boolean.FALSE) {
                assignment.setDueDate(null);
            }
            // TODO FIXME ASNN-295
            // previewAssignmentBean.setAssignment(assignment);
            // reviewAssignmentBean.setOTPKey(key);
        }
        
        // Validation
       
        // SWG Put this back in.
        // Assignment2Validator validator = new Assignment2Validator();
       // if (validator.validate(assignment, messages)) {
       //     returnCode = WorkFlowResult.INSTRUCTOR_PREVIEW_ASSIGNMENT;
       // }
        
        //return returnCode;
        return WorkFlowResult.INSTRUCTOR_PREVIEW_ASSIGNMENT;
    }

    public WorkFlowResult processActionEdit() {
        return WorkFlowResult.INSTRUCTOR_CONTINUE_EDITING_ASSIGNMENT;
    }

    public WorkFlowResult processActionSaveDraft() {
        WorkFlowResult result = WorkFlowResult.INSTRUCTOR_SAVE_DRAFT_ASSIGNMENT;
        for (String key : OTPMap.keySet()) {
            Assignment2 assignment = OTPMap.get(key);
            result = internalProcessPost(assignment, key, Boolean.TRUE);
        }
        return result;
    }

    public WorkFlowResult processActionCancel() {
        return WorkFlowResult.INSTRUCTOR_CANCEL_ASSIGNMENT;
    }

    public void createDuplicate(Long assignmentId) {
        Assignment2Creator creator = new Assignment2Creator();
        creator.setExternalLogic(externalLogic);
        creator.setMessageLocator(messageLocator);
        Assignment2 duplicate = creator.createDuplicate(logic.getAssignmentByIdWithGroupsAndAttachments(assignmentId));
        try {
            logic.saveAssignment(duplicate);

        } catch (SecurityException e) {
            LOG.error(e.getMessage(), e);
            messages.addMessage(new TargettedMessage("assignment2.assignment_post.security_exception"));
            return;
        } catch (AnnouncementPermissionException ape) {
            if (LOG.isDebugEnabled()) LOG.debug("Announcement could not " +
            "be updated b/c user does not have perm in annc tool");
            //TODO display to user?
        }
        messages.addMessage(new TargettedMessage("assignment2.assignment_post.duplicate",
                new Object[] {duplicate.getTitle() }, TargettedMessage.SEVERITY_INFO));
    }

    public String processSaveReorder() {
        //all action is done via an ajax request actually
        messages.addMessage(new TargettedMessage("assignment2.assignment_reorder.saved", new Object[]{}, TargettedMessage.SEVERITY_INFO));
        return SAVE_REORDER;
    }

    public String processCancelReorder() {
        return "cancel";
    }


}
