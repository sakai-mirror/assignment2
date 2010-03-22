package org.sakaiproject.assignment2.tool.beans;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.assignment2.tool.WorkFlowResult;


public class PermissionsBean {

    // Property Binding
    private Map<String, Boolean> permissionsMap = new HashMap<String, Boolean>();
    public void setPermissionsMap(Map<String, Boolean> permissionsMap) {
        this.permissionsMap = permissionsMap;
    }
    
    public Map<String, Boolean> getPermissionsMap() {
        return this.permissionsMap;
    }
    
    private AssignmentPermissionLogic permissionLogic;
    public void setAssignmentPermissionLogic(AssignmentPermissionLogic permissionLogic) {
        this.permissionLogic = permissionLogic;
    }
    
    private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }

    /**
     * We will use this separator to build the role/function key
     * ex instructor + {@link #ROLE_FUNCTION_SEPARATOR} + {@link AssignmentConstants#PERMISSION_ADD_ASSIGNMENTS}
     */
    public static final String ROLE_FUNCTION_SEPARATOR = "%%%";

    /*
     * PERMISSIONS FUNCTIONS
     */
    public WorkFlowResult savePermissions(){
        // we need to parse the permissionsMap into roles and permissions
        Map<String, Map<String, Boolean>> roleIdPermMap = new HashMap<String, Map<String,Boolean>>();
        for (Map.Entry<String, Boolean> entry : permissionsMap.entrySet()) {
            String roleIdFunction = entry.getKey();
            boolean hasPerm = entry.getValue();
            
            // we need to split the roleIdFunction
            String[] split = roleIdFunction.split(ROLE_FUNCTION_SEPARATOR);
            String roleId = split[0];
            String function = split[1];
            
            Map<String, Boolean> permMap = new HashMap<String, Boolean>();
            if (roleIdPermMap.containsKey(roleId)) {
                permMap = roleIdPermMap.get(roleId);
            }
            
            permMap.put(function, hasPerm);
            
            roleIdPermMap.put(roleId, permMap);
        }
        
        permissionLogic.savePermissions(externalLogic.getCurrentContextId(), roleIdPermMap);

        return WorkFlowResult.PERMISSIONS_SAVE;
    }

    public WorkFlowResult processActionCancel() {
        return WorkFlowResult.PERMISSIONS_CANCEL;
    }

}
