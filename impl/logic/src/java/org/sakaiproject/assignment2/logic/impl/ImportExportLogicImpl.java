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

package org.sakaiproject.assignment2.logic.impl;

import java.util.Calendar;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment.api.Assignment;
import org.sakaiproject.assignment.api.AssignmentContent;
import org.sakaiproject.assignment.api.AssignmentService;
import org.sakaiproject.assignment.api.Assignment.AssignmentAccess;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.GradebookItem;
import org.sakaiproject.assignment2.logic.ImportExportLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalAnnouncementLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.dao.AssignmentDao;
import org.sakaiproject.assignment2.exception.AnnouncementPermissionException;
import org.sakaiproject.assignment2.exception.ConflictingAssignmentNameException;
import org.sakaiproject.assignment2.exception.NoGradebookItemForGradedAssignmentException;
import org.sakaiproject.entity.api.Reference;
import org.sakaiproject.entity.api.ResourceProperties;
import org.sakaiproject.exception.IdInvalidException;
import org.sakaiproject.exception.IdUnusedException;
import org.sakaiproject.exception.IdUsedException;
import org.sakaiproject.exception.InconsistentException;
import org.sakaiproject.exception.OverQuotaException;
import org.sakaiproject.exception.PermissionException;
import org.sakaiproject.exception.ServerOverloadException;
import org.sakaiproject.exception.TypeException;
import org.sakaiproject.genericdao.api.finders.ByPropsFinder;
import org.sakaiproject.service.gradebook.shared.StaleObjectModificationException;
import org.sakaiproject.site.api.Group;
import org.sakaiproject.time.api.TimeBreakdown;
import org.sakaiproject.assignment2.logic.entity.AssignmentDefinition;
import org.sakaiproject.assignment2.logic.utils.ComparatorsUtils;
import org.sakaiproject.content.api.ContentHostingService;
import org.sakaiproject.content.api.ContentResource;
import org.springframework.orm.hibernate3.HibernateOptimisticLockingFailureException;



/**
 * This is the implementation of methods related to the import/export of
 * Assignment2 data
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class ImportExportLogicImpl implements ImportExportLogic {

	private static Log log = LogFactory.getLog(ImportExportLogicImpl.class);

	private ExternalLogic externalLogic;
	public void setExternalLogic(ExternalLogic externalLogic) {
		this.externalLogic = externalLogic;
	}

	private ExternalGradebookLogic gradebookLogic;
	public void setExternalGradebookLogic(ExternalGradebookLogic gradebookLogic) {
		this.gradebookLogic = gradebookLogic;
	}

	private ExternalAnnouncementLogic announcementLogic;
	public void setExternalAnnouncementLogic(ExternalAnnouncementLogic announcementLogic) {
		this.announcementLogic = announcementLogic;
	}

	private AssignmentLogic assignmentLogic;
	public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
		this.assignmentLogic = assignmentLogic;
	}

	private AssignmentDao dao;
	public void setDao(AssignmentDao dao) {
		this.dao = dao;
	}
	
	private AssignmentService assignmentService;
	public void setAssignmentService(AssignmentService assignmentService) {
		this.assignmentService = assignmentService;
	}
	
	private ContentHostingService contentHostingService;
	public void setContentHostingService(ContentHostingService contentHostingService) {
		this.contentHostingService = contentHostingService;
	}

	public void init(){
		if(log.isDebugEnabled()) log.debug("init");
	}

	public String getAssignmentToolDefinitionXML(String contextId) {
		AssignmentToolDefinition toolDefinition = new AssignmentToolDefinition();
		toolDefinition.setAssignments(getAssignmentDefinitionsInContext(contextId));

		return VersionedExternalizable.toXml(toolDefinition);
	}

	public List<AssignmentDefinition> getAssignmentDefinitionsInContext(String contextId) {

		if (contextId == null) {
			throw new IllegalArgumentException("Null contextId passed to getAssignmentDefinitionsInContext");
		}
		List<AssignmentDefinition> assignList = new ArrayList();

		Set<Assignment2> allAssignments = dao.getAssignmentsWithGroupsAndAttachments(contextId);
		if (allAssignments != null && !allAssignments.isEmpty()) {
			List allGbItems = gradebookLogic.getAllGradebookItems(contextId);
			Map<Long, GradebookItem> gbIdItemMap = new HashMap();
			if (allGbItems != null) {
				for (Iterator gbIter = allGbItems.iterator(); gbIter.hasNext();) {
					GradebookItem item = (GradebookItem) gbIter.next();
					gbIdItemMap.put(item.getGradableObjectId(), item);
				}
			}

			Map<String, String> groupIdToTitleMap = new HashMap();
			Collection groups = externalLogic.getSiteGroups(contextId);
			if (groups != null) {
				for (Iterator gIter = groups.iterator(); gIter.hasNext();) {
					Group group = (Group) gIter.next();
					if (group != null) {
						groupIdToTitleMap.put(group.getId(), group.getTitle());
					}
				}
			}

			for (Iterator assignIter = allAssignments.iterator(); assignIter.hasNext();) {
				Assignment2 assign = (Assignment2) assignIter.next();
				if (assign != null) {
					AssignmentDefinition assignDef = getAssignmentDefinition(assign, gbIdItemMap, groupIdToTitleMap);
					assignList.add(assignDef);
				}
			}
		}

		return assignList;
	}

	private AssignmentDefinition getAssignmentDefinition(Assignment2 assignment, Map gbIdItemMap,
			Map groupIdToTitleMap) {
		if (assignment == null) {
			throw new IllegalArgumentException("Null assignment passed to getAssignmentDefinition");
		}

		AssignmentDefinition assignDef = new AssignmentDefinition();
		assignDef.setAcceptUntilDate(assignment.getAcceptUntilTime());
		assignDef.setDraft(assignment.isDraft());
		assignDef.setDueDateForUngraded(assignment.getDueDateForUngraded());
		assignDef.setHasAnnouncement(assignment.getHasAnnouncement());
		assignDef.setHonorPledge(assignment.isHonorPledge());
		assignDef.setInstructions(assignment.getInstructions());
		assignDef.setNotificationType(assignment.getNotificationType());
		assignDef.setNumSubmissionsAllowed(assignment.getNumSubmissionsAllowed());
		assignDef.setOpenDate(assignment.getOpenTime());
		assignDef.setSortIndex(assignment.getSortIndex());
		assignDef.setSubmissionType(assignment.getSubmissionType());
		assignDef.setTitle(assignment.getTitle());
		assignDef.setUngraded(assignment.isUngraded());

		// if it is graded, we need to retrieve the name of the associated gb item
		if (!assignment.isUngraded() && assignment.getGradableObjectId() != null &&
				gbIdItemMap != null) {
			GradebookItem gbItem = (GradebookItem)gbIdItemMap.get(assignment.getGradableObjectId());
			if (gbItem != null) {
				assignDef.setAssociatedGbItemName(gbItem.getTitle());
				assignDef.setAssociatedGbItemDueDate(gbItem.getDueDate());
				assignDef.setAssociatedGbItemPtsPossible(gbItem.getPointsPossible());
			}
		}

		// we need to make a list of the attachment references
		List attachRefList = new ArrayList();
		if (assignment.getAttachmentSet() != null) {
			for (Iterator attachIter = assignment.getAttachmentSet().iterator(); attachIter.hasNext();) {
				AssignmentAttachment attach = (AssignmentAttachment) attachIter.next();
				if (attach != null) {
					attachRefList.add(attach.getAttachmentReference());
				}
			}
		}
		assignDef.setAttachmentReferences(attachRefList);

		// we need to make a list of the group names
		List associatedGroupNames = new ArrayList();
		if (assignment.getAssignmentGroupSet() != null && groupIdToTitleMap != null) {
			for (Iterator agIter = assignment.getAssignmentGroupSet().iterator(); agIter.hasNext();) {
				AssignmentGroup aGroup = (AssignmentGroup) agIter.next();
				if (aGroup != null) {
					String groupName = (String)groupIdToTitleMap.get(aGroup.getGroupId());
					if (groupName != null) {
						associatedGroupNames.add(groupName);
					}
				}
			}
		}
		assignDef.setGroupRestrictionGroupTitles(associatedGroupNames);

		return assignDef;
	}

	public void mergeAssignmentToolDefinitionXml(String toContext, String fromAssignmentToolXml) {
		AssignmentToolDefinition toolDefinition = 
			(AssignmentToolDefinition)VersionedExternalizable.fromXml(fromAssignmentToolXml);

		if (toolDefinition != null) {
			if(toolDefinition.getAssignments() != null) {
				
				// let's retrieve the existing assignments in this site so we can
				// compare the assignment titles
				Set<Assignment2> currAssignments = dao.getAssignmentsWithGroupsAndAttachments(toContext);
				List currTitles = new ArrayList();
				if (currAssignments != null) {
					for (Iterator aIter = currAssignments.iterator(); aIter.hasNext();) {
						Assignment2 assign = (Assignment2) aIter.next();
						if (assign != null) {
							currTitles.add(assign.getTitle());
						}
					}
				}

				// now retrieve a list of all of the gb items in this site
				List<GradebookItem> currGbItems = gradebookLogic.getAllGradebookItems(toContext);
				// make a map of item title to item
				Map<String, GradebookItem> gbTitleToItemMap = new HashMap();
				if (currGbItems != null) {
					for (Iterator gbIter = currGbItems.iterator(); gbIter.hasNext();) {
						GradebookItem item = (GradebookItem) gbIter.next();
						if (item != null) {
							gbTitleToItemMap.put(item.getTitle(), item);
						}
					}
				}

				// make a map of all of the titles of groups in the new site
				Collection currGroups = externalLogic.getSiteGroups(toContext);
				Map<String, Group> groupTitleGroupMap = new HashMap();
				for (Iterator groupIter = currGroups.iterator(); groupIter.hasNext();) {
					Group group = (Group) groupIter.next();
					if (group != null) {
						groupTitleGroupMap.put(group.getTitle(), group);
					}
				}

				// we will iterate through all of the assignments to be imported
				for (Iterator aIter = toolDefinition.getAssignments().iterator(); aIter.hasNext();) {
					AssignmentDefinition assignDef = (AssignmentDefinition) aIter.next();
					if (assignDef != null) {
						Assignment2 newAssignment = new Assignment2();
						newAssignment.setAcceptUntilTime(assignDef.getAcceptUntilDate());
						newAssignment.setContextId(toContext);
						newAssignment.setDraft(assignDef.isDraft());
						newAssignment.setHonorPledge(assignDef.isHonorPledge());
						newAssignment.setInstructions(assignDef.getInstructions());
						newAssignment.setNotificationType(assignDef.getNotificationType());
						newAssignment.setNumSubmissionsAllowed(assignDef.getNumSubmissionsAllowed());
						newAssignment.setOpenTime(assignDef.getOpenDate());
						
						if (assignDef.getSortIndex() == null) {
							int index = dao.getHighestSortIndexInSite(toContext);
							newAssignment.setSortIndex(index);
						} else {
							newAssignment.setSortIndex(assignDef.getSortIndex());
						}

						newAssignment.setSubmissionType(assignDef.getSubmissionType());
						newAssignment.setUngraded(assignDef.isUngraded());
						newAssignment.setHasAnnouncement(assignDef.isHasAnnouncement());
						
						// if title already exists, we need to append "_1" or "_2" etc - whatever it takes to make it unique
						String title = assignDef.getTitle();
						if (currTitles.contains(assignDef.getTitle())) {
							title = getNewTitle(assignDef.getTitle(), currTitles);
						}
						newAssignment.setTitle(title);

						// if this item is graded, we need to link it up to a 
						// corresponding gb item. first, we will check to see if
						// the gb item already exists. if there is an item with
						// the same name and points possible value, we will link
						// our assignment to this item. Otherwise, we will create
						// a new gradebook item
						if (!assignDef.isUngraded() && assignDef.getAssociatedGbItemName() != null) {
							Long associatedGbItemId = null;
							GradebookItem existingItem = gbTitleToItemMap.get(assignDef.getAssociatedGbItemName());
							if (existingItem != null) {
								// an item exists with this title already - check the points possible
								if (existingItem.getPointsPossible().equals(assignDef.getAssociatedGbItemPtsPossible())) {
									associatedGbItemId = existingItem.getGradableObjectId();
								} else {
									// we need to create a new item
									// be careful b/c multiple assignments may be associated with
									// the same gb item. we don't want to try to create it more than once
									String gbItemTitle = getNewTitle(assignDef.getAssociatedGbItemName(), 
											new ArrayList(gbTitleToItemMap.keySet()));
									associatedGbItemId = gradebookLogic.createGbItemInGradebook(toContext, 
											gbItemTitle, assignDef.getAssociatedGbItemPtsPossible(), 
											assignDef.getAssociatedGbItemDueDate(), false, false);
									if (log.isDebugEnabled()) log.debug("New gb item created via import!");
									
									// now let's retrieve it and add it to our map of existing gb items
									// so we don't try to create it again
									GradebookItem newItem = gradebookLogic.getGradebookItemById(toContext, associatedGbItemId);
									if (newItem != null) {
										gbTitleToItemMap.put(newItem.getTitle(), newItem);
									}
								}
							} else {
								// this is a new item
								associatedGbItemId = gradebookLogic.createGbItemInGradebook(toContext, 
										assignDef.getAssociatedGbItemName(), assignDef.getAssociatedGbItemPtsPossible(), 
										assignDef.getAssociatedGbItemDueDate(), false, false);
								if (log.isDebugEnabled()) log.debug("New gb item created via import!");
								// now let's retrieve it and add it to our map of existing gb items
								// so we don't try to create it again
								GradebookItem newItem = gradebookLogic.getGradebookItemById(toContext, associatedGbItemId);
								if (newItem != null) {
									gbTitleToItemMap.put(newItem.getTitle(), newItem);
								}
							}

							newAssignment.setGradableObjectId(associatedGbItemId);
						}

						// we need to copy any associated attachments 
						if (assignDef.getAttachmentReferences() != null && 
								!assignDef.getAttachmentReferences().isEmpty()) {
							
							Set<AssignmentAttachment> attachSet = new HashSet();
							for (Iterator attachIter = assignDef.getAttachmentReferences().iterator(); attachIter.hasNext();) {
								String attRef = (String) attachIter.next();
								String newAttId = copyAttachment(attRef, toContext);
								AssignmentAttachment newAA = new AssignmentAttachment(newAssignment, newAttId);
								attachSet.add(newAA);
							}
							newAssignment.setAttachmentSet(attachSet);
						}

						// if a group with the same name exists in the new site,
						// associate the assignment with that group
						if (assignDef.getGroupRestrictionGroupTitles() != null &&
								!assignDef.getGroupRestrictionGroupTitles().isEmpty() &&
								groupTitleGroupMap != null) {

							Set assignGroupSet = new HashSet();
							// now iterate through the groups from the old site
							// to see if a group with that name exists in the
							// new site
							for (Iterator agIter = assignDef.getGroupRestrictionGroupTitles().iterator(); agIter.hasNext();) {
								String groupTitle = (String) agIter.next();
								Group group = groupTitleGroupMap.get(groupTitle);
								if (group != null) {
									// the group exists, so create AssignmentGroup
									AssignmentGroup ag = new AssignmentGroup(newAssignment, group.getId());
									assignGroupSet.add(ag);
								}
							}
							
							newAssignment.setAssignmentGroupSet(assignGroupSet);

						}
						
						// if this assignment has an announcement and the new
						// site has an announcements tool, add one in the new site
						if (assignDef.isHasAnnouncement()) {
							if (externalLogic.siteHasTool(toContext, ExternalLogic.TOOL_ID_ANNC)) {
								// we need to add an announcement in the new site
								try {
									//TODO we need access to the bundle!
									String newAnncId = announcementLogic.addOpenDateAnnouncement(
											newAssignment.getListOfAssociatedGroupReferences(), 
											toContext, "imported subject", "imported body");
									newAssignment.setAnnouncementId(newAnncId);
								} catch (AnnouncementPermissionException ape) {
									newAssignment.setHasAnnouncement(false);
									log.info("No announcement added for imported assignment b/c no permission in annc tool");
								}
							} else {
								// this new assignment won't have an annc b/c no tool in site
								newAssignment.setHasAnnouncement(false);
							}
						}
						
						try {
							assignmentLogic.saveAssignment(newAssignment, toContext);
							if (log.isDebugEnabled()) log.debug("New assignment " + 
									newAssignment.getTitle() + " added in site " + toContext);
						} catch (ConflictingAssignmentNameException cane) {
							if (log.isInfoEnabled()) log.info("Assignment with title " +
									newAssignment.getTitle() + " already exists so was not added to site " + toContext);
						}
					}
				}
				
			}
		}

	}
	
	public String getAssignmentToolDefinitionXmlFromOriginalAssignmentsTool(String fromContext, String toContext) {
		
		List<AssignmentDefinition> assignmentDefs = new ArrayList();	
		Collection siteGroups = externalLogic.getSiteGroups(fromContext);
		
		Iterator origAssignIter = assignmentService.getAssignmentsForContext(fromContext);
		while (origAssignIter.hasNext()) {
			Assignment oAssignment = (Assignment)origAssignIter.next();
			AssignmentContent oContent = oAssignment.getContent();
			ResourceProperties oProperties = oAssignment.getProperties();
			
			AssignmentDefinition newAssnDef = new AssignmentDefinition();
			
			Calendar cal = Calendar.getInstance();
			TimeBreakdown openTime = oAssignment.getOpenTime().breakdownLocal();
			cal.set(openTime.getYear(), openTime.getMonth(), openTime.getDay(), 
					openTime.getHour(), openTime.getMin(), openTime.getSec());
			newAssnDef.setOpenDate(cal.getTime());
			
			if (oAssignment.getCloseTime() != null) {
				TimeBreakdown closeTime = oAssignment.getCloseTime().breakdownLocal();
				cal.set(closeTime.getYear(), closeTime.getMonth(), closeTime.getDay(), 
						closeTime.getHour(), closeTime.getMin(), closeTime.getSec());
				newAssnDef.setAcceptUntilDate(cal.getTime());
			}
			
			newAssnDef.setTitle(oAssignment.getTitle());
			newAssnDef.setDraft(oAssignment.getDraft());
			newAssnDef.setInstructions(oContent.getInstructions());
			newAssnDef.setHonorPledge(oContent.getHonorPledge() == Assignment.HONOR_PLEDGE_ENGINEERING);
			// set submission type
			if (oContent.getTypeOfSubmission() == Assignment.TEXT_AND_ATTACHMENT_ASSIGNMENT_SUBMISSION) {
				newAssnDef.setSubmissionType(AssignmentConstants.SUBMIT_INLINE_AND_ATTACH);
			} else if (oContent.getTypeOfSubmission() == Assignment.ATTACHMENT_ONLY_ASSIGNMENT_SUBMISSION) {
				newAssnDef.setSubmissionType(AssignmentConstants.SUBMIT_ATTACH_ONLY);
			} else if (oContent.getTypeOfSubmission() == Assignment.NON_ELECTRONIC_ASSIGNMENT_SUBMISSION) {
				newAssnDef.setSubmissionType(AssignmentConstants.SUBMIT_NON_ELECTRONIC);
			} else if (oContent.getTypeOfSubmission() == Assignment.TEXT_ONLY_ASSIGNMENT_SUBMISSION) {
				newAssnDef.setSubmissionType(AssignmentConstants.SUBMIT_INLINE_ONLY);
			} else {
				// default to text and attachments
				newAssnDef.setSubmissionType(AssignmentConstants.SUBMIT_INLINE_AND_ATTACH);
			}
			
			// retrieve the notification setting
			String notifProperty = oProperties.getProperty(Assignment.ASSIGNMENT_INSTRUCTOR_NOTIFICATIONS_VALUE);
			if (notifProperty == null) {
				newAssnDef.setNotificationType(AssignmentConstants.NOTIFY_NONE);
			} else if (notifProperty.equals(Assignment.ASSIGNMENT_INSTRUCTOR_NOTIFICATIONS_DIGEST)) {
				newAssnDef.setNotificationType(AssignmentConstants.NOTIFY_DAILY_SUMMARY);
			} else if (notifProperty.equals(Assignment.ASSIGNMENT_INSTRUCTOR_NOTIFICATIONS_EACH)) {
				newAssnDef.setNotificationType(AssignmentConstants.NOTIFY_FOR_EACH);
			} else if (notifProperty.equals(Assignment.ASSIGNMENT_INSTRUCTOR_NOTIFICATIONS_NONE)) {
				newAssnDef.setNotificationType(AssignmentConstants.NOTIFY_NONE);
			} else {
				newAssnDef.setNotificationType(AssignmentConstants.NOTIFY_NONE); // default
			}
			
			// is there an announcement?
			String openDateAnnc = oProperties.getProperty(ResourceProperties.PROP_ASSIGNMENT_OPENDATE_ANNOUNCEMENT_MESSAGE_ID);
			if (openDateAnnc != null) {
				newAssnDef.setHasAnnouncement(true);
			} else {
				newAssnDef.setHasAnnouncement(false);
			}
			
			// the old tool didn't support a resubmission option on the assignment level,
			// so just allow 1 submission
			newAssnDef.setNumSubmissionsAllowed(1);
			
			// handle attachments
			List oAttachments = oContent.getAttachments();
			List attachRefList = new ArrayList();
			if (oAttachments != null && !oAttachments.isEmpty()) {
				for (Iterator attachIter = oAttachments.iterator(); attachIter.hasNext();) {
					Reference attach = (Reference) attachIter.next();
					if (attach != null) {
						attachRefList.add(attach.getId());
					}
				}
			}
			newAssnDef.setAttachmentReferences(attachRefList);
			
			// handle any group restrictions
			List groupTitleList = new ArrayList();
			if (oAssignment.getAccess() == Assignment.AssignmentAccess.GROUPED &&
					oAssignment.getGroups() != null && !oAssignment.getGroups().isEmpty()) {
				if (siteGroups != null) {
					// iterate through this assignment's groups and find the name
					for (Iterator gIter = siteGroups.iterator(); gIter.hasNext();) {
						Group group = (Group)gIter.next();
						if (group != null) {
							if (oAssignment.getGroups().contains(group.getReference())) {
								groupTitleList.add(group.getTitle());
							}
						}
					}
				}
			}
			newAssnDef.setGroupRestrictionGroupTitles(groupTitleList);
			
			// now let's handle the graded/ungraded stuff
			if (oContent.getTypeOfGrade() == Assignment.UNGRADED_GRADE_TYPE) {
				newAssnDef.setUngraded(true);
				if (oAssignment.getDueTime() != null) {
					TimeBreakdown dueTime = oAssignment.getOpenTime().breakdownLocal();
					cal.set(dueTime.getYear(), dueTime.getMonth(), dueTime.getDay(), 
							dueTime.getHour(), dueTime.getMin(), dueTime.getSec());
					newAssnDef.setDueDateForUngraded(cal.getTime());
				}
			} else {
				// TODO - we need to figure out how to handle this!!
				newAssnDef.setUngraded(true);
				
			}
			
			assignmentDefs.add(newAssnDef);
		}
		
		AssignmentToolDefinition assignmentToolDef = new AssignmentToolDefinition();
		assignmentToolDef.setAssignments(assignmentDefs);
		
		return VersionedExternalizable.toXml(assignmentToolDef);
	}
	
	private String getNewTitle(String originalTitle, List existingTitles) {
		int increment = 1;
		String newTitle = originalTitle + "_" + increment;
		while (existingTitles.contains(newTitle)) {
			increment++;
			newTitle = originalTitle + "_" + increment;
		}
		
		return newTitle;
	}
	
	private String copyAttachment(String attId, String contextId) {
		String newAttId = null;
		if (attId != null) {
			try {
				ContentResource oldAttachment = contentHostingService.getResource(attId);
				String toolTitle = externalLogic.getToolTitle();
				String name = oldAttachment.getProperties().getProperty(
						ResourceProperties.PROP_DISPLAY_NAME);
				String type = oldAttachment.getContentType();
				byte[] content = oldAttachment.getContent();
				ResourceProperties properties = oldAttachment.getProperties();
				
				ContentResource newResource = contentHostingService.addAttachmentResource(name, 
						contextId, toolTitle, type, content, properties);
				newAttId = newResource.getId();
				
			} catch (TypeException te) {
				log.warn("TypeException thrown while attempting to retrieve resource with" +
						" id " + attId + ". Attachment was not copied.");
			} catch (PermissionException pe) {
				log.warn("PermissionException thrown while attempting to retrieve resource with" +
						" id " + attId + ". Attachment was not copied.");
			} catch (IdUnusedException iue) {
				log.warn("IdUnusedException thrown while attempting to retrieve resource with" +
						" id " + attId + ". Attachment was not copied.");
			} catch (IdInvalidException iie) {
				log.warn("IdInvalidException thrown while attempting to copy resource with" +
						" id " + attId + ". Attachment was not copied.");
			} catch (ServerOverloadException soe) {
				log.warn("ServerOverloadException thrown while attempting to copy resource with" +
						" id " + attId + ". Attachment was not copied.");
			} catch (IdUsedException iue) {
				log.warn("IdUsedException thrown while attempting to copy resource with" +
						" id " + attId + ". Attachment was not copied.");
			} catch (OverQuotaException oqe) {
				log.warn("OverQuotaException thrown while attempting to copy resource with" +
						" id " + attId + ". Attachment was not copied.");
			} catch (InconsistentException ie) {
				log.warn("InconsistentException thrown while attempting to copy resource with" +
						" id " + attId + ". Attachment was not copied.");
			}
		}
		
		return newAttId;
	}
}
