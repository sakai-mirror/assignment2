/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/api/logic/src/java/org/sakaiproject/assignment2/logic/ExternalGradebookLogicImpl.java $
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

package org.sakaiproject.assignment2.logic.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.GradebookItem;
import org.sakaiproject.component.cover.ComponentManager;
import org.sakaiproject.component.cover.ServerConfigurationService;
import org.sakaiproject.exception.IdUnusedException;
import org.sakaiproject.service.gradebook.shared.GradebookFrameworkService;
import org.sakaiproject.service.gradebook.shared.GradebookService;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.site.api.Site;
import org.sakaiproject.site.api.ToolConfiguration;
import org.sakaiproject.site.cover.SiteService;
import org.sakaiproject.service.gradebook.shared.AssessmentNotFoundException;
import org.sakaiproject.service.gradebook.shared.Assignment;
import org.sakaiproject.service.gradebook.shared.CommentDefinition;
import org.sakaiproject.service.gradebook.shared.GradeDefinition;
import org.sakaiproject.tool.gradebook.Gradebook;


/**
 * This is the implementation for logic to interact with the Sakai
 * Gradebook tool
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class ExternalGradebookLogicImpl implements ExternalGradebookLogic {

    private static Log log = LogFactory.getLog(ExternalGradebookLogicImpl.class);

    public void init() {
    	if (log.isDebugEnabled()) log.debug("init");
    }
    
    private GradebookService gradebookService;
    public void setGradebookService(GradebookService gradebookService) {
    	this.gradebookService = gradebookService;
    }

    public List<Assignment2> getViewableAssignmentsWithGbData(List<Assignment2> gradedAssignments, String contextId) {
    	if (contextId == null) {
    		throw new IllegalArgumentException("contextId is null in getViewableAssignmentsWithGbData");
    	}
    	
    	List<Assignment2> viewableAssignmentsWithGbData = new ArrayList();
    	if (gradedAssignments == null || gradedAssignments.isEmpty()) {
    		return viewableAssignmentsWithGbData;
    	}
    	
    	List<Assignment> gbAssignments = gradebookService.getViewableAssignmentsForCurrentUser(contextId);
		
		Map goIdGbAssignmentMap = new HashMap();
		if (gbAssignments != null) {
			for (Iterator gbIter = gbAssignments.iterator(); gbIter.hasNext();) {
				Assignment gbObject = (Assignment) gbIter.next();
				if (gbObject != null) {
					goIdGbAssignmentMap.put(gbObject.getId(), gbObject);
				}
			}
		}
		
		boolean userIsStudent = isCurrentUserAStudentInGb(contextId);
		List allGbItems = new ArrayList();
		
		// there are 2 situations in which the gradable object associated with the assignment
		// is not included in the list returned from the gradebook:
		// 1) the user does not have permission to view that GO - if the user is a student,
		//		we still want to display the assignment but don't want to display the
		//		associated grade info b/c not released to students yet
		// 2) the GO was deleted from the gb
		
		for (Iterator assignIter = gradedAssignments.iterator(); assignIter.hasNext();) {
			Assignment2 gradedAssignment = (Assignment2) assignIter.next();
			if (gradedAssignment != null && !gradedAssignment.isUngraded()) {
				Long goId = gradedAssignment.getGradableObjectId();
				if (goId != null) {
					Assignment gbItem =	(Assignment)goIdGbAssignmentMap.get(goId);
					if (gbItem != null) {
						gradedAssignment.setDueDate(gbItem.getDueDate());
						gradedAssignment.setPointsPossible(gbItem.getPoints());
						viewableAssignmentsWithGbData.add(gradedAssignment);
					} else {
						// check to see if this gradable object exists anymore
						if (!gradebookService.isGradableObjectDefined(goId)) {
							// then the GO was deleted -- let the user know
							gradedAssignment.setNeedsUserAttention(true);
							viewableAssignmentsWithGbData.add(gradedAssignment);
						} else {
							// if it exists, then this user does not have perm to view it in the gb
							if (userIsStudent) {
								// if a student, we still need to return an assignment with gb item info
								// this just means the grade info has not been released yet
								Assignment unreleasedAssign = gradebookService.getAssignment(contextId, goId);
								gradedAssignment.setDueDate(unreleasedAssign.getDueDate());
								gradedAssignment.setPointsPossible(unreleasedAssign.getPoints());
								viewableAssignmentsWithGbData.add(gradedAssignment);
							}
						}
					}
				} else {
					// there is no gradableObjectId set for this assignment!
					gradedAssignment.setNeedsUserAttention(true);
					viewableAssignmentsWithGbData.add(gradedAssignment);
				}
			}
		}
		
		return viewableAssignmentsWithGbData;
    }

    public void createGradebookDataIfNecessary(String contextId) {
    	// we need to check if there is gradebook data defined for this site. if not,
        // create it (but will not actually add the tool, just the backend)
    	
    	GradebookFrameworkService frameworkService = (org.sakaiproject.service.gradebook.shared.GradebookFrameworkService) 
        ComponentManager.get("org.sakaiproject.service.gradebook.GradebookFrameworkService");
        if (!frameworkService.isGradebookDefined(contextId)) {
        	if (log.isInfoEnabled()) 
        		log.info("Gradebook data being added to context " + contextId + " by Assignment2 tool");
        	frameworkService.addGradebook(contextId, contextId);
        }
    }

    public Map getViewableGradableObjectIdTitleMap(String contextId) {
    	if (contextId == null) {
    		throw new IllegalArgumentException("Null contextId passed to getViewableGradableObjectIdTitleMap");
    	}
    	
    	List<Assignment> viewableGbItems = gradebookService.getViewableAssignmentsForCurrentUser(contextId);
    	
    	Map<Long, String> idTitleMap = new HashMap();
    	if (viewableGbItems == null || viewableGbItems.isEmpty()) {
    		return idTitleMap;
    	}
    	
    	for (Iterator itemIter = viewableGbItems.iterator(); itemIter.hasNext();) {
    		Assignment assign =	(Assignment)itemIter.next();
    		
    		if (assign != null) {
    			idTitleMap.put(assign.getId(), assign.getName());
    		}
    	}
    	
    	return idTitleMap;
    }

    public List<GradebookItem> getAllGradebookItems(String contextId) {
    	if (contextId == null) {
    		throw new IllegalArgumentException("null contextId passed to getAllGradebookItems");
    	}

    	List<GradebookItem> gradebookItems = new ArrayList<GradebookItem>();

    	try {
    		List<Assignment> allGbItems = gradebookService.getAssignments(contextId);

    		if (allGbItems != null) {

    			for (Iterator itemIter = allGbItems.iterator(); itemIter.hasNext();) {
    				Assignment assign =	(Assignment)itemIter.next();

    				if (assign != null) {
    					GradebookItem item = 
    						new GradebookItem(assign.getId(), assign.getName(), assign.getPoints(), assign.getDueDate());
    					gradebookItems.add(item);
    				}
    			}
    		}
    	} catch (SecurityException se) {
    		throw new SecurityException("User without edit or grade perm attempted to access the list of all gb items");
    	}

    	return gradebookItems;
    }

    public Map<String, String> getViewableGroupIdToTitleMap(String contextId) {
    	if (contextId == null) {
    		throw new IllegalArgumentException("Null contextId passed to getViewableGroupIdToTitleMap");
    	}
    	
    	return gradebookService.getViewableSectionUuidToNameMap(contextId);
    }

    public Map<String, String> getViewableStudentsForGradedItemMap(String contextId, Long gradableObjectId) {
    	if (contextId == null) {
    		throw new IllegalArgumentException("Null contextId passed to getViewableGroupIdToTitleMap");
    	}
    	
    	Map<String, String> studentIdGbFunctionMap = gradebookService.getViewableStudentsForItemForCurrentUser(contextId, gradableObjectId);
    	Map<String, String> studentIdAssnFunctionMap = new HashMap();
    	
    	if (studentIdGbFunctionMap != null) {
    		for (Iterator stIter = studentIdGbFunctionMap.keySet().iterator(); stIter.hasNext();) {
    			String studentId = (String) stIter.next();
    			if (studentId != null && studentIdGbFunctionMap.get(studentId) != null) {
    				String function = studentIdGbFunctionMap.get(studentId);
    				if (function != null) {
    					if (function.equals(GradebookService.gradePermission)) {
    						studentIdAssnFunctionMap.put(studentId, AssignmentConstants.GRADE);
    					} else {
    						studentIdAssnFunctionMap.put(studentId, AssignmentConstants.VIEW);
    					}
    				}
    			}
    		}
    	}
    	
    	return studentIdAssnFunctionMap;
    }
    
    public boolean isCurrentUserAbleToEdit(String contextId) {
    	return gradebookService.currentUserHasEditPerm(contextId);
    }
	
	public boolean isCurrentUserAbleToGradeAll(String contextId) {
    	return gradebookService.currentUserHasGradeAllPerm(contextId);
	}
	
	public boolean isCurrentUserAbleToGrade(String contextId) {
    	return gradebookService.currentUserHasGradingPerm(contextId);
	}
	
	public boolean isCurrentUserAbleToViewOwnGrades(String contextId) {
    	return gradebookService.currentUserHasViewOwnGradesPerm(contextId);
	}
	
	public boolean isCurrentUserAStudentInGb(String contextId) {
		boolean userIsAStudentInGb = false;
		
		if (isCurrentUserAbleToViewOwnGrades(contextId) &&
				!isCurrentUserAbleToGrade(contextId) &&
				!isCurrentUserAbleToEdit(contextId)) {
			userIsAStudentInGb = true;
		}
		
		return userIsAStudentInGb;
	}
    
    public String getGradeViewPermissionForCurrentUserForStudentForItem(String contextId, String studentId, Long gbItemId) {
    	if (contextId == null || studentId == null || gbItemId == null) {
    		throw new IllegalArgumentException("Null contextId or studentId or itemId passed to getGradeViewPermissionForCurrentUserForStudentForItem");
    	}
    	
    	String viewOrGrade = null;
    	
    	String function =
    		gradebookService.getGradeViewFunctionForUserForStudentForItem(contextId, gbItemId, studentId);
    	
    	if (function == null) {
    		viewOrGrade = null;
    	} else if (function.equals(GradebookService.gradePermission)) {
    		viewOrGrade = AssignmentConstants.GRADE;
    	} else if (function.equals(GradebookService.viewPermission)) {
    		viewOrGrade = AssignmentConstants.VIEW;
    	}
    	
    	return viewOrGrade;
    }
    
    public String getStudentGradeForItem(String contextId, String studentId, Long gbItemId) {
    	if (contextId == null || studentId == null || gbItemId == null) {
    		throw new IllegalArgumentException("Null contextId or studentId or gbItemId passed to getStudentGradeForItem");
    	}
    	
    	String grade = null;
    	
    	try {
    		GradeDefinition gradeDef = gradebookService.getGradeDefinitionForStudentForItem(contextId, gbItemId, studentId);
    		if (gradeDef != null) {
    			grade = gradeDef.getGrade();
    		}
    	} catch (AssessmentNotFoundException anfe) {
    		// this gradebook item no longer exists, so return a null grade
    		grade = null;
    	}
    	
    	return grade;
    }
    
    public String getStudentGradeCommentForItem(String contextId, String studentId, Long gbItemId) {
    	if (contextId == null || studentId == null || gbItemId == null) {
    		throw new IllegalArgumentException("Null contextId or studentId or gbItemId passed to getStudentGradeCommentForItem");
    	}
    	
    	String comment = null;
    	
    	try {
    		CommentDefinition commentDef = gradebookService.getAssignmentScoreComment(contextId, gbItemId, studentId);
    		if (commentDef != null) {
    			comment = commentDef.getCommentText();
    		}
    	} catch (AssessmentNotFoundException anfe) {
    		// this gradebook item no longer exists, so return a null comment
    		comment = null;
    	}
    	
    	return comment;
    }
    
    public void populateGradesForSubmissions(String contextId, List<AssignmentSubmission> submissionList, Assignment2 assignment) {
    	if (contextId == null || assignment == null) {
    		throw new IllegalArgumentException("null contextId or assignment passed to populateStudentGradeInformation");
    	}
    	
    	if (!assignment.isUngraded() && assignment.getGradableObjectId() != null && 
    			submissionList != null && !submissionList.isEmpty()) {
    		Map<String, AssignmentSubmission> studentIdSubmissionMap = new HashMap();
    		for (Iterator subIter = submissionList.iterator(); subIter.hasNext();) {
    			AssignmentSubmission submission = (AssignmentSubmission) subIter.next();
    			if (submission != null) {
    				studentIdSubmissionMap.put(submission.getUserId(), submission);
    			}
    		}
    		
    		List<String> studentIdList = new ArrayList(studentIdSubmissionMap.keySet());
    		List<GradeDefinition> gradeDefs =
    			gradebookService.getGradesForStudentsForItem(contextId, assignment.getGradableObjectId(), studentIdList);
    		
    		if (gradeDefs != null) {
    			for (Iterator gradeIter = gradeDefs.iterator(); gradeIter.hasNext();) {
    				GradeDefinition gradeDef = (GradeDefinition) gradeIter.next();
    				if (gradeDef != null) {
    					StringBuilder sb = new StringBuilder();
    					sb.append(gradeDef.getGrade());
    					if (gradeDef.getGradeEntryType() == GradebookService.GRADE_TYPE_PERCENTAGE) {
    						sb.append("%");
    					}
    					
    					AssignmentSubmission thisSubmission = (AssignmentSubmission) studentIdSubmissionMap.get(gradeDef.getStudentUid());
    					if (thisSubmission != null) {
    						thisSubmission.setGradebookGrade(sb.toString());
    						thisSubmission.setGradebookGradeReleased(gradeDef.isGradeReleased());
    					}
    				}
    			}
    		}
    	}
    }
    
    public boolean isCurrentUserAbleToGradeStudentForItem(String contextId, String studentId, Long gradableObjectId) {
    	if (studentId == null || gradableObjectId == null) {
    		throw new IllegalArgumentException("null studentId or gradableObjectId passed to isCurrentUserAbleToGradeStudentForItem");
    	}
    	
    	return gradebookService.isUserAbleToGradeItemForStudent(contextId, gradableObjectId, studentId);
    }
    
    public void populateAllGradeInfoForSubmission(String contextId, String currUserId, AssignmentSubmission submission) {
    	if (contextId == null || currUserId == null) {
    		throw new IllegalArgumentException("null contextId or currUserId passed to populateAllGradeInfoForSubmission");
    	}
    	
    	if (submission != null) {
    		Assignment2 assignment = submission.getAssignment();
    		if (assignment != null && !assignment.isUngraded() && assignment.getGradableObjectId() != null) {
    			Long gbItemId = assignment.getGradableObjectId();
    			
    			try {
    				GradeDefinition gradeDef = gradebookService.getGradeDefinitionForStudentForItem(contextId, gbItemId, submission.getUserId());
    				CommentDefinition commentDef = gradebookService.getAssignmentScoreComment(contextId, gbItemId, submission.getUserId());
    				
    				if (gradeDef != null) {
    					submission.setGradebookGrade(gradeDef.getGrade());
        				submission.setGradebookGradeReleased(gradeDef.isGradeReleased());
    				}
    				if (commentDef != null) {
    					submission.setGradebookComment(commentDef.getCommentText());
    				}
    				
    				// if the submission is for the current user and grade info
    				// has not been released, do not return grade info
    				if (gradeDef != null && !gradeDef.isGradeReleased() && 
    						currUserId.equals(submission.getUserId())) {
    					submission.setGradebookComment(null);
    					submission.setGradebookGrade(null);
    				}
    				
    			} catch (AssessmentNotFoundException anfe) {
    				// this gb item no longer exists, so there is no information to populate
    				if (log.isDebugEnabled()) log.debug("gb item with id " + gbItemId + " no longer exists, so returning null grade info");
    			}
    		}
    	}
    }
    
    public void populateGradebookItemDetailsForAssignment(String contextId, Assignment2 assignment) {
    	if (contextId == null || assignment == null) {
    		throw new IllegalArgumentException("null contextId or assignment passed to populateGradebookItemDetailsForAssignment");
    	}
    	
    	if (!assignment.isUngraded() && assignment.getGradableObjectId() != null) {
    		try {
    			Assignment gbItem = gradebookService.getAssignment(contextId, assignment.getGradableObjectId());
    			assignment.setDueDate(gbItem.getDueDate());
    			assignment.setPointsPossible(gbItem.getPoints());
    		} catch (AssessmentNotFoundException e) {
    			if (log.isDebugEnabled()) log.debug("Gradebook item that assignment " + assignment.getId() + " with associated with no longer exists");
    			assignment.setNeedsUserAttention(true);
    		}
    	}
    }

	public String getGradeType(String contextId)
	{
		Gradebook gb = (Gradebook) gradebookService.getGradebook(contextId);
		Integer i = new Integer(gb.getGrade_type());
		return i.toString();
	}
	
	public Long createGbItemInGradebook(String contextId, String title, Double pointsPossible, Date dueDate,
			boolean releasedToStudents, boolean countedInCourseGrade) {
		if (contextId == null || title == null) {
			throw new IllegalArgumentException("Null contextId or title passed to createGbItemInGradebook");
		}
		
		if (countedInCourseGrade && !releasedToStudents) {
			throw new IllegalArgumentException("You may not count an item in course grade without releasing to students.");
		}
		
		Long gradableObjectId = null;
		Assignment newItem = new Assignment();
		newItem.setCounted(countedInCourseGrade);
		newItem.setDueDate(dueDate);
		newItem.setName(title);
		newItem.setPoints(pointsPossible);
		newItem.setReleased(releasedToStudents);
		
		gradebookService.addAssignment(contextId, newItem);
		if (log.isDebugEnabled()) log.debug("New gradebook item added to gb via assignment2 tool");
		
		// now let's retrieve the id of this newly created item
		Assignment newlyCreatedAssign = gradebookService.getAssignment(contextId, title);
		if (newlyCreatedAssign != null) {
			gradableObjectId = newlyCreatedAssign.getId();
		}
		
		return gradableObjectId;
	}
	
	public GradebookItem getGradebookItemById(String contextId, Long gradableObjectId) {
		if (contextId == null || gradableObjectId == null) {
			throw new IllegalArgumentException ("Null contextId or gradableObjectId passed to getGradebookItemById");
		}
		
		GradebookItem gradebookItem = new GradebookItem();
		
		try {
			Assignment assign = gradebookService.getAssignment(contextId, gradableObjectId);
			gradebookItem = new GradebookItem(assign.getId(), assign.getName(), assign.getPoints(), assign.getDueDate());
		} catch (AssessmentNotFoundException anfe) {
			throw new IllegalArgumentException ("No gradebook item exists with gradableObjectId " + gradableObjectId + " in context " + contextId);
		}
		
		return gradebookItem;
	}

}
