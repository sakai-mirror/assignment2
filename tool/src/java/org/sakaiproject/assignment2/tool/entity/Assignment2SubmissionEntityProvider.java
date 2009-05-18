package org.sakaiproject.assignment2.tool.entity;

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.sakaiproject.assignment2.logic.AssignmentBundleLogic;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.GradeInformation;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.tool.params.GradeViewParams;
import org.sakaiproject.assignment2.tool.producers.GradeProducer;
import org.sakaiproject.entitybroker.EntityReference;
import org.sakaiproject.entitybroker.entityprovider.CoreEntityProvider;
import org.sakaiproject.entitybroker.entityprovider.capabilities.RESTful;
import org.sakaiproject.entitybroker.entityprovider.capabilities.RequestAware;
import org.sakaiproject.entitybroker.entityprovider.capabilities.RequestStorable;
import org.sakaiproject.entitybroker.entityprovider.extension.RequestGetter;
import org.sakaiproject.entitybroker.entityprovider.extension.RequestStorage;
import org.sakaiproject.entitybroker.entityprovider.search.Search;
import org.sakaiproject.entitybroker.util.AbstractEntityProvider;

import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIOutput;

public class Assignment2SubmissionEntityProvider extends AbstractEntityProvider implements
CoreEntityProvider, RESTful, RequestStorable, RequestAware{

    /**
     * Dependency
     */
    private AssignmentSubmissionLogic submissionLogic;
    public void setSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
        this.submissionLogic = submissionLogic;
    }
    
    /**
     * Dependency
     */
    private AssignmentLogic assignmentLogic;
    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
        this.assignmentLogic = assignmentLogic;
    }
    
    /**
     * Dependency
     */
    private ExternalGradebookLogic externalGradebookLogic;
    public void setExternalGradebookLogic(ExternalGradebookLogic externalGradebookLogic) {
        this.externalGradebookLogic = externalGradebookLogic;
    }
    
    /**
     * Dependency
     */
    private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }
    
    /**
     * Dependency
     */
    private AssignmentPermissionLogic permissionLogic;
    public void setPermissionLogic(AssignmentPermissionLogic permissionLogic) {
        this.permissionLogic = permissionLogic;
    }
    
    /**
     * Dependency
     */
    private RequestStorage requestStorage;
    public void setRequestStorage(RequestStorage requestStorage) {
        this.requestStorage = requestStorage;
    }
    
    /**
     * Dependency
     */
    private RequestGetter requestGetter;
    public void setRequestGetter(RequestGetter requestGetter) {
        this.requestGetter = requestGetter;
    }
    
    /**
     * Dependency
     */
    private AssignmentBundleLogic assignmentBundleLogic;
    public void setAssignmentBundleLogic(AssignmentBundleLogic assignmentBundleLogic) {
        this.assignmentBundleLogic = assignmentBundleLogic;
    }
    
    public boolean entityExists(String id) {
        // TODO Auto-generated method stub
        return false;
    }

    public final static String PREFIX = "assignment2submission";
    public String getEntityPrefix() {
        return PREFIX;
    }

    public String createEntity(EntityReference ref, Object entity,
            Map<String, Object> params) {
        // TODO Auto-generated method stub
        return null;
    }

    public Object getSampleEntity() {
        // TODO Auto-generated method stub
        return null;
    }

    public void updateEntity(EntityReference ref, Object entity,
            Map<String, Object> params) {
        // TODO Auto-generated method stub
        
    }

    public Object getEntity(EntityReference ref) {
        // TODO Auto-generated method stub
        return null;
    }

    public void deleteEntity(EntityReference ref, Map<String, Object> params) {
        // TODO Auto-generated method stub
        
    }

    public List<?> getEntities(EntityReference ref, Search search) {
        Long assignmentId = requestStorage.getStoredValueAsType(Long.class, "asnnid");
        
        DateFormat df = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT, assignmentBundleLogic.getLocale());
        
        List togo = new ArrayList();
        
        List<AssignmentSubmission> submissions = submissionLogic.getViewableSubmissionsWithHistoryForAssignmentId(assignmentId, null);
        
        if (submissions == null) {
            return togo;
        }
        
        // put studentIds in a list
        List<String> studentIdList = new ArrayList<String>();
        for (AssignmentSubmission submission : submissions) {
            studentIdList.add(submission.getUserId());
        }
        
        Assignment2 assignment = assignmentLogic.getAssignmentById(assignmentId);
        
        Map<String, GradeInformation> studentIdGradeInfoMap = new HashMap<String, GradeInformation>();
        if (submissions != null && assignment.isGraded() && assignment.getGradebookItemId() != null) {
            // now retrieve all of the GradeInformation
            studentIdGradeInfoMap = externalGradebookLogic.getGradeInformationForStudents(
                    studentIdList, assignment.getContextId(), assignment.getGradebookItemId());
        }

        Map<String, String> studentIdSortNameMap = externalLogic.getUserIdToSortNameMap(studentIdList);
        
        for (AssignmentSubmission as : submissions) {
            Map submap = new HashMap();
            submap.put("studentName", studentIdSortNameMap.get(as.getUserId()));

            // submission info columns are not displayed for non-electronic assignments
            if (assignment.getSubmissionType() != AssignmentConstants.SUBMIT_NON_ELECTRONIC) {
                if (as.getCurrentSubmissionVersion() != null && as.getCurrentSubmissionVersion().getSubmittedDate() != null){
                    submap.put("submittedDate", as.getCurrentSubmissionVersion().getSubmittedDate());
                    submap.put("submittedDateFormat", df.format(as.getCurrentSubmissionVersion().getSubmittedDate()));
                } else {
                    // We're not addign this to the subject.
                }
                
                // set the textual representation of the submission status
                String status = "";
                int statusConstant = AssignmentConstants.SUBMISSION_NOT_STARTED;
                if (as != null) {
                    statusConstant = submissionLogic.getSubmissionStatusConstantForCurrentVersion(
                            as.getCurrentSubmissionVersion(), assignment.getDueDate());
                    status = assignmentBundleLogic.getString(
                            "assignment2.assignment_grade-assignment.submission_status." + 
                            statusConstant);
                    submap.put("submissionStatus", status);
                }

            }

            if (assignment.isGraded()) {
                String grade = "";
                GradeInformation gradeInfo = studentIdGradeInfoMap.get(as.getUserId());
                if (gradeInfo != null) {
                    grade = gradeInfo.getGradebookGrade();
                }
                submap.put("grade", grade);
            }

            if (as.getCurrentSubmissionVersion() != null)  {
                submap.put("feedbackReleased",as.getCurrentSubmissionVersion().isFeedbackReleased());
            }
            else {
                submap.put("feedbackReleased", false);
            }

            togo.add(submap);
        }

        return togo;
    }

    public String[] getHandledOutputFormats() {
        // TODO Auto-generated method stub
        return null;
    }

    public String[] getHandledInputFormats() {
        // TODO Auto-generated method stub
        return null;
    }

}
