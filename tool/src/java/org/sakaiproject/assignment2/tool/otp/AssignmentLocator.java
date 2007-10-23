package org.sakaiproject.assignment2.tool.otp;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;

import uk.org.ponder.beanutil.BeanLocator;

public class AssignmentLocator implements BeanLocator {

    public static final String NEW_PREFIX = "new ";
    public static String NEW_1 = NEW_PREFIX + "1";

	private AssignmentLogic assignmentLogic;
    //private ExternalLogic externalLogic;

	private Map<String, Assignment2> delivered = new HashMap<String, Assignment2>();

	public Object locateBean(String name) {
        Assignment2 togo = delivered.get(name);
        if (togo == null) {
            if (name.startsWith(NEW_PREFIX)) {
                // create the new object
                //String currentUserId = externalLogic.getCurrentUserId();
                togo = new Assignment2(); //currentUserId, externalLogic.getCurrentLocationId(), null );
            } else {
                togo = assignmentLogic.getAssignmentById(new Long(name));
            }
            delivered.put(name, togo);
        }
        return togo;
	}

	public String saveAll() {
        for (Iterator it = delivered.keySet().iterator(); it.hasNext();) {
            String key = (String) it.next();
            Assignment2 assignment = delivered.get(key);
            if (key.startsWith(NEW_PREFIX)) {
                // could do stuff here
            }
            assignmentLogic.saveAssignment(assignment);
        }
		return "saved";
	}


	public void setBlogLogic(AssignmentLogic assignmentLogic) {
		this.assignmentLogic = assignmentLogic;
	}

	/**
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }
    **/
}
