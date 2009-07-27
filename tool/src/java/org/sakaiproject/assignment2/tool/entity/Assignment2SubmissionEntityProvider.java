package org.sakaiproject.assignment2.tool.entity;

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sakaiproject.assignment2.logic.AssignmentBundleLogic;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalContentReviewLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.GradeInformation;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
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
import org.sakaiproject.entitybroker.entityprovider.search.Order;
import org.sakaiproject.entitybroker.entityprovider.search.Search;
import org.sakaiproject.entitybroker.util.AbstractEntityProvider;

import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIOutput;

public class Assignment2SubmissionEntityProvider extends AbstractEntityProvider implements
CoreEntityProvider, RESTful, RequestStorable, RequestAware{
    public static final String STUDENT_NAME_PROP = "studentName";
    public static final String STUDENT_ID_PROP = "studentId";
    public static final String SUBMITTED_DATE = "submittedDate";
    public static final String SUBMITTED_DATE_FORMATTED = "submittedDateFormat";
    public static final String SUBMISSION_STATUS = "submissionStatus";
    public static final String SUBMISSION_GRADE = "grade";
    public static final String SUBMISSION_FEEDBACK_RELEASED = "feedbackReleased";

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
    
    private ExternalContentReviewLogic contentReviewLogic;
    public void setExternalContentReviewLogic(ExternalContentReviewLogic contentReviewLogic) {
        this.contentReviewLogic = contentReviewLogic;
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

    @SuppressWarnings("unchecked")
    public List<?> getEntities(EntityReference ref, Search search) {
        Long assignmentId = requestStorage.getStoredValueAsType(Long.class, "asnnid");

        DateFormat df = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT, assignmentBundleLogic.getLocale());

        List togo = new ArrayList();

        String filterGroupId = requestStorage.getStoredValueAsType(String.class, "groupId");
        List<AssignmentSubmission> submissions = submissionLogic.getViewableSubmissionsWithHistoryForAssignmentId(assignmentId, filterGroupId);

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
        
        if (assignment.isContentReviewEnabled()) {
            populateReviewProperties(assignment, submissions);
        }

        for (AssignmentSubmission as : submissions) {
            Map submap = new HashMap();
            submap.put("studentName", studentIdSortNameMap.get(as.getUserId()));
            submap.put("studentId", as.getUserId());

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
            
            if (assignment.isContentReviewEnabled()) {
                // content review is enabled, so we need to figure out what to display for the 
                // review column
                AssignmentSubmissionVersion currVersion = as.getCurrentSubmissionVersion();
                Set<SubmissionAttachment> submittedAttachSet = 
                    currVersion != null ? currVersion.getSubmissionAttachSet() : null;
                if (submittedAttachSet != null && submittedAttachSet.size() > 0) {
                    // iterate through the attachments to see if any were reviewed
                    List<SubmissionAttachment> reviewedAttach = new ArrayList<SubmissionAttachment>();
                    for (SubmissionAttachment attach : submittedAttachSet) {
                        if (attach.getProperties() != null) {
                            String status = (String)attach.getProperties().get(AssignmentConstants.PROP_REVIEW_STATUS);
                            if (status != null && !status.equals(AssignmentConstants.REVIEW_STATUS_NONE)) {
                                reviewedAttach.add(attach);
                            }
                        }
                    }
                    
                    if (reviewedAttach.size() > 1) {
                        submap.put("reviewMultiple", true);
                        submap.put("reviewError", false);
                        submap.put("reviewStatus", false);
                        submap.put("reviewScore", false);
                    } else if (reviewedAttach.size() == 1) {
                        // we need to either display an error indicator
                        // or a score
                        SubmissionAttachment attach = reviewedAttach.get(0);
                        Map properties = attach.getProperties();
                        String status = (String)attach.getProperties().get(AssignmentConstants.PROP_REVIEW_STATUS);
                        if (status.equals(AssignmentConstants.REVIEW_STATUS_ERROR)) {
                            submap.put("reviewError", true);
                            Long statusErrorCode = (Long)attach.getProperties().get(AssignmentConstants.PROP_REVIEW_ERROR_CODE);
                            String errorMsg = contentReviewLogic.getErrorMessage(statusErrorCode);
                            submap.put("reviewErrorMsg", errorMsg);
                        } else if (status.equals(AssignmentConstants.REVIEW_STATUS_SUCCESS)) {
                            String score = (String)properties.get(AssignmentConstants.PROP_REVIEW_SCORE_DISPLAY);
                            String link = (String)properties.get(AssignmentConstants.PROP_REVIEW_URL);
                            submap.put("reviewStatus", true);
                            submap.put("reviewScore", score);
                            submap.put("reviewScoreLink", link);
                        }
                    } 
                }
            }

            togo.add(submap);
        }

        if (search.getOrders() != null && search.getOrders().length > 0) {
            Order order = search.getOrders()[0];
            // We can sort by:
            // studentName, submittedDate, submissionStatus, grade, and feedbackReleased
            final String orderBy = order.getProperty();
            final boolean ascending = order.ascending;
            if (orderBy.equals(STUDENT_NAME_PROP) || orderBy.equals(SUBMITTED_DATE) ||
                    orderBy.equals(SUBMISSION_STATUS) || 
                    (orderBy.equals(SUBMISSION_GRADE) && assignment.isGraded()) ||
                    orderBy.equals(SUBMISSION_FEEDBACK_RELEASED)) {

                Collections.sort(togo, new Comparator() {
                    public int compare(Object o1, Object o2) {
                        Map m1, m2;
                        if (ascending) {
                            m1 = (Map) o1;
                            m2 = (Map) o2;
                        }
                        else {
                            m2 = (Map) o1;
                            m1 = (Map) o2;
                        }
                        if (m1.get(orderBy) instanceof Date) {
                            return ((Date)m1.get(orderBy)).compareTo(((Date)m2.get(orderBy)));
                        } 
                        else if (m1.get(orderBy) instanceof Boolean) {
                            return ((Boolean)m1.get(orderBy)).compareTo(((Boolean)m2.get(orderBy)));
                        }
                        else {
                            return m1.get(orderBy).toString().compareTo(m2.get(orderBy).toString());
                        }
                    }});
            }
        }

        long start = (int) search.getStart();
        if (togo.size() == 0) {
            start = 0;
        }
        else if (start >= togo.size()) {
            start = togo.size() - 1;
        }

        long end = togo.size();

        if (search.getLimit() > 0) {
            end = start + search.getLimit();
            if (end > togo.size()) {
                end = togo.size();
            }
        }

        return togo.subList((int)start, (int)end);
    }
    
    private void populateReviewProperties(Assignment2 assign, List<AssignmentSubmission> submissions) {
        if (assign == null) {
            throw new IllegalArgumentException("Null assign passed to populateReviewProperties");
        }
        Map<String, Set<SubmissionAttachment>> studentIdAttachListMap = new HashMap<String, Set<SubmissionAttachment>>();
        
        if (submissions != null && !submissions.isEmpty()) {
            // collect all of the attachments for review
            List<SubmissionAttachment> attachToReview = new ArrayList<SubmissionAttachment>();
            for (AssignmentSubmission submission : submissions) {
                AssignmentSubmissionVersion currVersion = submission.getCurrentSubmissionVersion();
                if (currVersion != null && currVersion.getSubmissionAttachSet() != null) {
                    attachToReview.addAll(currVersion.getSubmissionAttachSet());
                }
            }
            
            // populate the review properties on the attachments
            contentReviewLogic.populateReviewProperties(assign, attachToReview, true);
        }
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
