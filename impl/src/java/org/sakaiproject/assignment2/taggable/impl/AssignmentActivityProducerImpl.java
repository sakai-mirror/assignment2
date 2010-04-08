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

package org.sakaiproject.assignment2.taggable.impl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.taggable.api.TaggableActivity;
import org.sakaiproject.taggable.api.TaggableItem;
import org.sakaiproject.taggable.api.TaggingManager;
import org.sakaiproject.taggable.api.TaggingProvider;
import org.sakaiproject.assignment2.dao.AssignmentDao;
import org.sakaiproject.assignment2.logic.AssignmentBundleLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.taggable.api.AssignmentActivityProducer;
import org.sakaiproject.authz.api.SecurityService;
import org.sakaiproject.entity.api.Entity;
import org.sakaiproject.entity.api.EntityManager;
import org.sakaiproject.entity.api.Reference;
import org.sakaiproject.site.api.SiteService;
import org.sakaiproject.user.api.UserDirectoryService;

public class AssignmentActivityProducerImpl implements
AssignmentActivityProducer {

    private static final Log logger = LogFactory
    .getLog(AssignmentActivityProducerImpl.class);

    protected AssignmentBundleLogic assignmentBundleLogic;

    protected AssignmentDao assignmentDao;

    protected EntityManager entityManager;

    protected TaggingManager taggingManager;

    protected SiteService siteService;

    protected SecurityService securityService;

    protected UserDirectoryService userDirectoryService;

    protected AssignmentPermissionLogic assignmentPermissionLogic;

    protected AssignmentSubmissionLogic assignmentSubmissionLogic;


    public boolean allowGetItems(TaggableActivity activity,
            TaggingProvider provider) {
        // We aren't picky about the provider, so ignore that argument.
        // Only allow this if the user can grade submissions
        //return assignmentDao.allowGradeSubmission(activity.getReference());
        //return assignmentPermissionLogic.isUserAbleToProvideFeedbackForSubmission(submissionId);
        Assignment2 assignment = (Assignment2) activity.getObject();
        return assignmentPermissionLogic.isUserAllowedToManageSubmissionsForAssignment(null, assignment);
    }

    public boolean allowRemoveTags(TaggableActivity activity) {
        Assignment2 assignment = (Assignment2) activity.getObject();
        return assignmentPermissionLogic.isUserAllowedToDeleteAssignment(null, assignment);
    }

    public boolean allowRemoveTags(TaggableItem item) {
        AssignmentSubmission subm = (AssignmentSubmission)item.getObject();
        return assignmentPermissionLogic.isUserAllowedToManageSubmission(null, subm.getId());
    }

    public boolean allowTransferCopyTags(TaggableActivity activity) {
        return securityService.unlock(SiteService.SECURE_UPDATE_SITE,
                siteService.siteReference(activity.getContext()));
    }

    public boolean checkReference(String ref) {
        return ref.startsWith(Entity.SEPARATOR + AssignmentConstants.REFERENCE_ROOT);
    }

    public List<TaggableActivity> getActivities(String context,
            TaggingProvider provider) {
        // We aren't picky about the provider, so ignore that argument.
        List<TaggableActivity> activities = new ArrayList<TaggableActivity>();
        List<Assignment2> assignments = assignmentDao.getAssignmentsWithGroupsAndAttachments(context);
        for (Assignment2 assignment : assignments) {
            activities.add(getActivity(assignment));
        }
        return activities;
    }

    public TaggableActivity getActivity(Assignment2 assignment) {
        return new AssignmentActivityImpl(assignment, this);
    }

    public TaggableActivity getActivity(String activityRef,
            TaggingProvider provider) {
        // We aren't picky about the provider, so ignore that argument.
        TaggableActivity activity = null;
        if (checkReference(activityRef)) {
            Reference ref = entityManager.newReference(activityRef);
            Assignment2 assignment = assignmentDao.getAssignmentByIdWithGroupsAndAttachments(Long.valueOf(ref.getId()));
            if (assignment != null) 
                activity = new AssignmentActivityImpl(assignment, this);
        }
        return activity;
    }

    public String getContext(String ref) {
        return entityManager.newReference(ref).getContext();
    }

    public String getId() {
        return PRODUCER_ID;
    }

    public TaggableItem getItem(AssignmentSubmission assignmentSubmission,
            String userId) {
        return new AssignmentItemImpl(assignmentSubmission, userId,
                new AssignmentActivityImpl(
                        assignmentSubmission.getAssignment(), this));
    }

    public String getName() {
        return assignmentBundleLogic.getString("service_name");
    }

    public void init() {
        logger.info("init()");

        taggingManager.registerProducer(this);
    }

    protected String parseAuthor(String itemRef) {
        return itemRef.split(AssignmentItemImpl.ITEM_REF_SEPARATOR)[1];
    }

    protected Long parseSubmissionRef(String itemRef) {
        return Long.valueOf(itemRef.split(AssignmentItemImpl.ITEM_REF_SEPARATOR)[0]);
    }

    public void setAssignmentDao(AssignmentDao assignmentDao)
    {
        this.assignmentDao = assignmentDao;
    }

    public void setEntityManager(EntityManager entityManager) {
        this.entityManager = entityManager;
    }

    public void setSecurityService(SecurityService securityService) {
        this.securityService = securityService;
    }

    public void setSiteService(SiteService siteService) {
        this.siteService = siteService;
    }

    public void setTaggingManager(TaggingManager taggingManager) {
        this.taggingManager = taggingManager;
    }

    public void setUserDirectoryService(
            UserDirectoryService userDirectoryService) {
        this.userDirectoryService = userDirectoryService;
    }

    public void setAssignmentPermissionLogic(
            AssignmentPermissionLogic assignmentPermissionLogic)
    {
        this.assignmentPermissionLogic = assignmentPermissionLogic;
    }

    public void setAssignmentSubmissionLogic(
			AssignmentSubmissionLogic assignmentSubmissionLogic) {
		this.assignmentSubmissionLogic = assignmentSubmissionLogic;
	}

	public void setAssignmentBundleLogic(AssignmentBundleLogic assignmentBundleLogic) {
        this.assignmentBundleLogic = assignmentBundleLogic;
    }

    public boolean allowGetItems(TaggableActivity activity, TaggingProvider provider, boolean getMyItemsOnly, String taggedItem)
    {
    	// We aren't picky about the provider, so ignore that argument.
        // Only allow this if the user can grade submissions
        //return assignmentDao.allowGradeSubmission(activity.getReference());
        //return assignmentPermissionLogic.isUserAbleToProvideFeedbackForSubmission(submissionId);
        Assignment2 assignment = (Assignment2) activity.getObject();
        return assignmentPermissionLogic.isUserAllowedToManageSubmissionsForAssignment(null, assignment);
    }

    public TaggableItem getItem(String itemRef, TaggingProvider provider, boolean getMyItemsOnly, String taggedItem)
    {
        TaggableItem item = null;
        if (checkReference(itemRef)) {
        	AssignmentSubmission submission = assignmentDao.getSubmissionWithVersionHistoryById(parseSubmissionRef(itemRef));
        	boolean allowed = provider.allowGetItem(submission.getAssignment().getReference(), 
        			itemRef, userDirectoryService.getCurrentUser().getId(), taggedItem);
        	if (allowed) {
        		item = new AssignmentItemImpl(submission, parseAuthor(itemRef),
        				new AssignmentActivityImpl(submission.getAssignment(),
        						this));
        	}
        }
        return item;
    }

    public String getItemPermissionOverride()
    {
        // TODO Auto-generated method stub
        return null;
    }
    
    public List<TaggableItem> getItems(TaggableActivity activity, TaggingProvider provider,
          boolean getMyItemsOnly, String taggedItem)
    {
       return getItems(activity, provider, getMyItemsOnly, taggedItem, true);
    }

    private List<TaggableItem> getItems(TaggableActivity activity, TaggingProvider provider,
            boolean getMyItemsOnly, String taggedItem, boolean checkPerms)
    {
    	// We aren't picky about the provider, so ignore that argument.
        List<TaggableItem> items = new ArrayList<TaggableItem>();
        Assignment2 assignment = (Assignment2) activity.getObject();
        /*
         * If you're not allowed to grade submissions, you shouldn't be able to
         * look at submission items. It seems that anybody is allowed to get any
         * submissions.
         */
        boolean allowed = false;
        if (checkPerms) {
           allowed = provider.allowGetItems(activity.getReference(), new String[]{}, userDirectoryService.getCurrentUser().getId(), taggedItem);
        }
        else {
           allowed = true;
        }
        
        if (allowed) {
            for (Iterator<AssignmentSubmission> i = assignmentSubmissionLogic.getViewableSubmissionsForAssignmentId(assignment.getId(), null).iterator(); i.hasNext();) {
                AssignmentSubmission submission = i.next();
                items.add(new AssignmentItemImpl(submission, submission.getUserId(), activity));
            }
        }
        return items;
    }

    public List<TaggableItem> getItems(TaggableActivity activity, String userId,
          TaggingProvider provider, boolean getMyItemsOnly, String taggedItem)
    {
       return getItems(activity, userId, provider, getMyItemsOnly, taggedItem, true);
    }
    
    private List<TaggableItem> getItems(TaggableActivity activity, String userId,
            TaggingProvider provider, boolean getMyItemsOnly, String taggedItem, boolean checkPerms)
    {
    	// We aren't picky about the provider, so ignore that argument.
        List<TaggableItem> returned = new ArrayList<TaggableItem>();
        Assignment2 assignment = (Assignment2) activity.getObject();
        
        boolean allowed = false;
        if (checkPerms) {
           allowed =provider.allowGetItems(activity.getReference(), new String[]{}, userDirectoryService.getCurrentUser().getId(), taggedItem);
        }
        else {
           allowed = true;
        }
        
        if (allowed) {
        	AssignmentSubmission submission = assignmentDao.getSubmissionWithVersionHistoryForStudentAndAssignment(
        			userId, assignment);
        	if (submission != null) {
        		TaggableItem item = new AssignmentItemImpl(submission, userId,
        				activity);
        		returned.add(item);
        	}
        }
        return returned;
    }
    
    public boolean hasSubmissions(TaggableActivity activity, TaggingProvider provider,
            boolean getMyItemsOnly, String taggedItem)
    {
       List<TaggableItem> items = new ArrayList<TaggableItem>();
       if (taggedItem == null)
          items = getItems(activity, provider, getMyItemsOnly, taggedItem, false);
       else
          items = getItems(activity, provider, getMyItemsOnly, taggedItem);
       
		return items.size() > 0;
    }

    public boolean hasSubmissions(TaggableActivity activity, String userId,
            TaggingProvider provider, boolean getMyItemsOnly, String taggedItem)
    {
       List<TaggableItem> items = new ArrayList<TaggableItem>();
       if (taggedItem == null)
          items = getItems(activity, userId, provider, getMyItemsOnly, taggedItem, false);
       else
          items = getItems(activity, userId, provider, getMyItemsOnly, taggedItem);
		return items.size() > 0;
    }
}
