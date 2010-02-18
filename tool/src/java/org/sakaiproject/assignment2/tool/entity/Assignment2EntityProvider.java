package org.sakaiproject.assignment2.tool.entity;

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.azeckoski.reflectutils.DeepUtils;
import org.sakaiproject.assignment2.exception.AssignmentNotFoundException;
import org.sakaiproject.assignment2.logic.AssignmentBundleLogic;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.ExternalContentReviewLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.ExternalTaggableLogic;
import org.sakaiproject.assignment2.logic.GradebookItem;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.tool.DisplayUtil;
import org.sakaiproject.entitybroker.EntityReference;
import org.sakaiproject.entitybroker.EntityView;
import org.sakaiproject.entitybroker.entityprovider.CoreEntityProvider;
import org.sakaiproject.entitybroker.entityprovider.capabilities.RESTful;
import org.sakaiproject.entitybroker.entityprovider.capabilities.RequestAware;
import org.sakaiproject.entitybroker.entityprovider.capabilities.RequestStorable;
import org.sakaiproject.entitybroker.entityprovider.extension.Formats;
import org.sakaiproject.entitybroker.entityprovider.extension.RequestGetter;
import org.sakaiproject.entitybroker.entityprovider.extension.RequestStorage;
import org.sakaiproject.entitybroker.entityprovider.search.Search;
import org.sakaiproject.entitybroker.util.AbstractEntityProvider;
import org.sakaiproject.entitybroker.entityprovider.annotations.EntityCustomAction;
import org.sakaiproject.site.api.Group;

import sun.util.logging.resources.logging;


/**
 * Entity Provider for Assn2 assignments.
 * 
 * @author sgithens
 *
 */
public class Assignment2EntityProvider extends AbstractEntityProvider implements
CoreEntityProvider, RESTful, RequestStorable, RequestAware {
    private static Log log = LogFactory.getLog(Assignment2EntityProvider.class);

    // Dependency
    private AssignmentLogic assignmentLogic;
    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
        this.assignmentLogic = assignmentLogic;
    }

    // Dependency
    private AssignmentPermissionLogic permissionLogic;
    public void setPermissionLogic(AssignmentPermissionLogic permissionLogic) {
        this.permissionLogic = permissionLogic;
    }

    // Dependency
    private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }
    
    // Dependency
    private ExternalGradebookLogic gradebookLogic;
    public void setExternalGradebookLogic(ExternalGradebookLogic gradebookLogic) {
        this.gradebookLogic = gradebookLogic;
    }

    // Dependency
    private DisplayUtil displayUtil;
    public void setDisplayUtil(DisplayUtil displayUtil) {
        this.displayUtil = displayUtil;
    }
    
    // Dependency
    private ExternalTaggableLogic taggableLogic;
    public void setExternalTaggableLogic(ExternalTaggableLogic taggableLogic) {
    	this.taggableLogic = taggableLogic;
    }

    private RequestStorage requestStorage;
    public void setRequestStorage(RequestStorage requestStorage) {
        this.requestStorage = requestStorage;
    }

    private RequestGetter requestGetter;
    public void setRequestGetter(RequestGetter requestGetter) {
        this.requestGetter = requestGetter;
    }

    private AssignmentBundleLogic assignmentBundleLogic;
    public void setAssignmentBundleLogic(AssignmentBundleLogic assignmentBundleLogic) {
        this.assignmentBundleLogic = assignmentBundleLogic;
    }
    
    private ExternalContentReviewLogic contentReviewLogic;
    public void setExternalContentReviewLogic(ExternalContentReviewLogic contentReviewLogic) {
        this.contentReviewLogic = contentReviewLogic;
    }

    public static String PREFIX = "assignment2";
    public String getEntityPrefix() {
        return PREFIX;
    }

    /**
     * TODO: Change this so it's not a GET
     * 
     * @param view
     */
    @EntityCustomAction(action="reorder", viewKey=EntityView.VIEW_LIST)
    public void reorderAssignments(EntityView view) {
        String context = (String) requestStorage.getStoredValue("siteid");
        String order = (String) requestStorage.getStoredValue("order");

        String[] stringAssignIds = order.split(",");
        try {
            // convert the strings to longs
            List<Long> longAssignmentIds = new ArrayList<Long>();
            for (int i=0; i < stringAssignIds.length; i++){
                String idAsString = stringAssignIds[i];
                if (idAsString != null && idAsString.trim().length() > 0) { 
                    longAssignmentIds.add(Long.valueOf(stringAssignIds[i]));
                }
            }
            assignmentLogic.reorderAssignments(longAssignmentIds, context);

            if (log.isDebugEnabled()) log.debug("Assignments reordered via Entity Feed");
        } catch (NumberFormatException nfe) {
            log.error("Non-numeric value passed to ReorderAssignmentsCommand. No reordering was saved.");
        }
    }

    /**
     * This is a custom action for retrieving the Assignment Data we need to 
     * render the list of assignments for landing pages. Currently this does
     * require a 'context' or 'siteid', but we should move towards this not
     * requiring that so it can be used for newer age 3akai things.
     * 
     * It's likely this will just be moved to the getEntities method after 
     * prototyping.
     * 
     * @param view
     * @return
     */
    @SuppressWarnings("unchecked")
    @EntityCustomAction(action="sitelist", viewKey=EntityView.VIEW_LIST)
    public List getAssignmentListForSite(EntityView view) {        
        String context = (String) requestStorage.getStoredValue("siteid");

        DateFormat df = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT, assignmentBundleLogic.getLocale());

        if (context == null) {
            return new ArrayList();
        }

        List<Assignment2> viewable = assignmentLogic.getViewableAssignments(context);
        
        // let's grab all of the gradebook items to see if we need to flag any
        // graded assignments b/c their associated gb item was deleted
        List<GradebookItem> existingGbItems = gradebookLogic.getAllGradebookItems(context, false);
        List<Long> existingGbItemIds = new ArrayList<Long>();
        if (existingGbItems != null) {
            for (GradebookItem gbItem : existingGbItems) {
                existingGbItemIds.add(gbItem.getGradebookItemId());
            }
        }

        List togo = new ArrayList();

        Map<Assignment2, List<String>> assignmentViewableStudentsMap = 
            permissionLogic.getViewableStudentsForUserForAssignments(externalLogic.getCurrentUserId(), context, viewable);

        Collection<Group> groups = externalLogic.getSiteGroups(context);
        Map<String,Group> groupmap = new HashMap<String,Group>();

        for (Group group: groups) {
            groupmap.put(group.getId(), group);
        }
        
        boolean contentReviewAvailable = contentReviewLogic.isContentReviewAvailable(context); 
        boolean canEdit = permissionLogic.isCurrentUserAbleToEditAssignments(context);
        boolean canMatrixLink = taggableLogic.isSiteAssociated(context);
        
        // TODO - use the service for entity work
       filterRestrictedAssignmentInfo(viewable, context);

        for (Assignment2 asnn: viewable) {
            Map asnnmap = new HashMap();
            asnnmap.put("id", asnn.getId());
            asnnmap.put("title", asnn.getTitle());
            asnnmap.put("openDate", asnn.getOpenDate());
            if (asnn.getOpenDate() != null) {
                asnnmap.put("openDateFormatted", df.format(asnn.getOpenDate()));
            }
            asnnmap.put("dueDate", asnn.getDueDate());
            if (asnn.getDueDate() != null) {
                asnnmap.put("dueDateFormatted", df.format(asnn.getDueDate()));
            }
            asnnmap.put("graded", asnn.isGraded());
            asnnmap.put("sortIndex", asnn.getSortIndex());
            asnnmap.put("requiresSubmission", asnn.isRequiresSubmission());
            asnnmap.put("draft", asnn.isDraft());

            // In case assignment has a gradebook item, but that gradebook item
            // no longer exists.
            if (asnn.isGraded() && (asnn.getGradebookItemId() == null || 
                    !existingGbItemIds.contains(asnn.getGradebookItemId()))) {
                asnnmap.put("gbItemMissing", true);
            }

            // Can the current user edit this particular assignment. Does not 
            // include grading. If a user can see this assignment they can grade
            // it.
            asnnmap.put("canEdit", canEdit);
            
            // Create/Edit Matrix Links
            asnnmap.put("canMatrixLink", canMatrixLink);

            List<String> viewableStudents = assignmentViewableStudentsMap.get(asnn);

            Map<String, String> subStatusMap = displayUtil.getSubmissionStatusForAssignment(asnn, viewableStudents);
            String inAndNewText = subStatusMap.get(DisplayUtil.IN_NEW_DISPLAY);
            String numSubmissions = subStatusMap.get(DisplayUtil.NUM_SUB);

            asnnmap.put("inAndNew", inAndNewText);
            asnnmap.put("numSubmissions", numSubmissions);
            
            asnnmap.put("reviewEnabled", contentReviewAvailable && asnn.isContentReviewEnabled());

            List groupstogo = new ArrayList();
            // we need to double check that all of the associated groups still exist.
            // if they don't, we will display an indicator that this assignment needs attention
            for (AssignmentGroup group: asnn.getAssignmentGroupSet()) {
                if (groupmap.containsKey(group.getGroupId())) {
                    Map groupprops = new HashMap();
                    groupprops.put("groupId", group.getGroupId());
                    groupprops.put("id", group.getId());

                    Group g = groupmap.get(group.getGroupId());
                    groupprops.put("title",g.getTitle());
                    groupprops.put("description", g.getDescription());
                    groupstogo.add(groupprops);
                } else {
                    // group was probably deleted, so signal a problem to user
                    asnnmap.put("groupMissing", true);
                }
            }
            asnnmap.put("groups", groupstogo);

            List attachstogo = new ArrayList();
            for (AssignmentAttachment attach: asnn.getAttachmentSet()) {
                Map attachprops = new HashMap();
                attachprops.put("id", attach.getId());
                attachprops.put("attachmentReference", attach.getAttachmentReference());
                attachstogo.add(attachprops);
            }
            asnnmap.put("attachments", attachstogo);

            togo.add(asnnmap);
        }

        // IE Won't stop caching even with the no-cache.
        HttpServletResponse httpServletResponse = requestGetter.getResponse();
        httpServletResponse.setHeader("Pragma", "no-cache");
        httpServletResponse.setHeader("Cache-Control", "max-age=0,no-cache,no-store,must-revalidate,private,post-check=0,pre-check=0,s-max-age=0");
        httpServletResponse.setDateHeader("Expires", 0 );

        httpServletResponse.setHeader("x-asnn2-canEdit", canEdit+"");

        return togo;
    }

    public boolean entityExists(String id) {
        boolean exists;
        try {
            assignmentLogic.getAssignmentById(new Long(id));
            exists = true;
        }
        catch (AssignmentNotFoundException anfe) {
            exists = false;
        }
        return exists;
    }

    public String createEntity(EntityReference ref, Object entity,
            Map<String, Object> params) {
        Assignment2 assignment = (Assignment2) entity;
        assignmentLogic.saveAssignment(assignment);
        return assignment.getId().toString();
    }

    public Object getSampleEntity() {
        return new Assignment2();
    }

    public void updateEntity(EntityReference ref, Object entity,
            Map<String, Object> params) {
        Assignment2 assignment = (Assignment2) entity;

        Assignment2 tosave = assignmentLogic.getAssignmentByIdWithAssociatedData(assignment.getId());

        /*
         * This is going to be obtuse.  Because Hibernate Model objects are so
         * different that the models we really want available to RESTful feeds,
         * and because of the wierd cascade populating, we're going to have to 
         * have a custom list of things that can be updated by the regular 
         * REST PUT update operation. The problem right now is that we can't use
         * the assignment object passed in to the method, because we deepCloned
         * it without stuff that couldn't be serialized in getEntity.
         * 
         * This is not a huge deal necessarily, but something we have to
         * remember about for now, until we make new model objects for REST
         * or start using some other metaprogramming paradigm.
         * 
         */
        tosave.setTitle(assignment.getTitle());

        assignmentLogic.saveAssignment(tosave);
    }

    public Object getEntity(EntityReference ref) {
        Assignment2 asnn = assignmentLogic.getAssignmentByIdWithAssociatedData(new Long(ref.getId()));
        
        // TODO use the service methods
        List<Assignment2> assignList = new ArrayList<Assignment2>();
        assignList.add(asnn);
        filterRestrictedAssignmentInfo(assignList, asnn.getContextId());

        DeepUtils deep = DeepUtils.getInstance();

        return deep.deepClone(asnn, 3, new String[] {"submissionsSet",
                "ListOfAssociatedGroupReferences","assignmentGroupSet",
                "attachmentSet","assignmentAttachmentRefs"});
    }

    public void deleteEntity(EntityReference ref, Map<String, Object> params) {
        Assignment2 asnn = assignmentLogic.getAssignmentById(new Long(ref.getId()));
        assignmentLogic.deleteAssignment(asnn);
    }

    public List<?> getEntities(EntityReference ref, Search search) {
        // TODO Auto-generated method stub
        return null;
    }

    public String[] getHandledOutputFormats() {
        return new String[] {Formats.XML, Formats.JSON, Formats.HTML };
    }

    public String[] getHandledInputFormats() {
        return new String[] {Formats.XML, Formats.JSON, Formats.HTML };
    }
    
    /**
     * Until we have time to rework the EntityProviders to utilize the service AssignmentDefinition,
     * we will manually filter the assignments here
     * @param assignList
     */
    private void filterRestrictedAssignmentInfo(List<Assignment2> assignList, String context) {
        if (assignList != null) {
            boolean filterRestrictedInfo = !permissionLogic.isUserAbleToAccessInstructorView(context);
            if (filterRestrictedInfo) {
                // non-instructors cannot view the accept until date or properties
                for (Assignment2 assign : assignList) {
                    assign.setProperties(null);
                    assign.setAcceptUntilDate(null);
                }
            }
        }
    }

}
