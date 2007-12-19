package org.sakaiproject.assignment2.tool.producers;

import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class NavBarRenderer {
	
    public void makeNavBar(UIContainer tofill, String divID, String currentViewID) {
        UIJointContainer joint = new UIJointContainer(tofill, divID, "assignment_navigation:", ""+1);
        
        UIBranchContainer cell;
        
        
        //Assignment List Link
        cell = UIBranchContainer.make(joint, "navigation-li:", "2");
        if (currentViewID.equals(AssignmentListSortViewProducer.VIEW_ID) 
        		|| currentViewID.equals(AssignmentListReorderProducer.VIEW_ID)) {
            UIMessage.make(cell, "navigation-text", "assignment2.navbar.assignment_list");
        } else {
            UIInternalLink.make(cell, "navigation-link", UIMessage.make("assignment2.navbar.assignment_list"), 
                    new SimpleViewParameters(AssignmentListSortViewProducer.VIEW_ID));               
        }
/**        
        UIOutput.make(cell, "navigation-separator");
        
        //Add Link
        cell = UIBranchContainer.make(joint, "navigation-li:", "1");
        if (currentViewID.equals(AssignmentAddProducer.VIEW_ID)) {
            UIMessage.make(cell, "navigation-text", "assignment2.navbar.add");
        } else {
            UIInternalLink.make(cell, "navigation-link", UIMessage.make("assignment2.navbar.add"), 
                    new SimpleViewParameters(AssignmentAddProducer.VIEW_ID));               
        }
**/
/**        
        UIOutput.make(cell, "navigation-separator");

        //Grade Report Link
        cell = UIBranchContainer.make(joint, "navigation-li:", "2");
        if (currentViewID.equals(AssignmentGradeReportProducer.VIEW_ID)) {
            UIMessage.make(cell, "navigation-text", "assignment2.navbar.grade_report");
        } else {
            UIInternalLink.make(cell, "navigation-link", UIMessage.make("assignment2.navbar.grade_report"), 
                    new SimpleViewParameters(AssignmentGradeReportProducer.VIEW_ID));               
        }
        ***/
        
    }
   
}
