package org.sakaiproject.assignment2.tool.entity;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.azeckoski.reflectutils.DeepUtils;
import org.sakaiproject.assignment2.exception.AssignmentNotFoundException;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.entitybroker.EntityReference;
import org.sakaiproject.entitybroker.EntityView;
import org.sakaiproject.entitybroker.entityprovider.CoreEntityProvider;
import org.sakaiproject.entitybroker.entityprovider.capabilities.RESTful;
import org.sakaiproject.entitybroker.entityprovider.capabilities.RequestStorable;
import org.sakaiproject.entitybroker.entityprovider.extension.Formats;
import org.sakaiproject.entitybroker.entityprovider.extension.RequestStorage;
import org.sakaiproject.entitybroker.entityprovider.search.Search;
import org.sakaiproject.entitybroker.util.AbstractEntityProvider;
import org.sakaiproject.entitybroker.entityprovider.annotations.EntityCustomAction;


/**
 * Entity Provider for Assn2 assignments.
 * 
 * @author sgithens
 *
 */
public class Assignment2EntityProvider extends AbstractEntityProvider implements
CoreEntityProvider, RESTful, RequestStorable {

    // Dependency
    private AssignmentLogic assignmentLogic;
    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
        this.assignmentLogic = assignmentLogic;
    }
    
    public static String PREFIX = "assignment2";
    public String getEntityPrefix() {
        return PREFIX;
    }
    
    @EntityCustomAction(action="sitelist", viewKey=EntityView.VIEW_LIST)
    public List getAssignmentListForSite(EntityView view) {
        List<Assignment2> viewable = assignmentLogic.getViewableAssignments("48ff42c3-9da3-4340-a8b8-5c5ad183b1d4");
        
        List togo = new ArrayList();
        
        for (Assignment2 asnn: viewable) {
            Map asnnmap = new HashMap();
            asnnmap.put("id", asnn.getId());
            asnnmap.put("title", asnn.getTitle());
            asnnmap.put("openDate", asnn.getOpenDate());
            asnnmap.put("dueDate", asnn.getDueDate());
            togo.add(asnnmap);
        }
        
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
        assignmentLogic.saveAssignment(assignment);
    }

    public Object getEntity(EntityReference ref) {
        Assignment2 asnn = assignmentLogic.getAssignmentByIdWithAssociatedData(new Long(ref.getId()));
        
        DeepUtils deep = DeepUtils.getInstance();
        
        return deep.deepClone(asnn, 3, new String[] {"submissionsSet",
                "ListOfAssociatedGroupReferences","assignmentGroupSet",
                "attachmentSet","assignmentAttachmentRefs"});
    }

    public void deleteEntity(EntityReference ref, Map<String, Object> params) {
        // TODO Auto-generated method stub
        
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

    
    RequestStorage requestStorage = null;
    public void setRequestStorage(RequestStorage requestStorage) {
        this.requestStorage = requestStorage;
    }

}
