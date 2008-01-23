package org.sakaiproject.assignment2.tool.producers.renderers;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.tool.producers.FinishedHelperProducer;
import org.sakaiproject.entitybroker.EntityBroker;
import org.sakaiproject.entitybroker.IdEntityReference;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UILink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;


public class GradebookDetailsRenderer {
	
    
	private EntityBroker entityBroker;
	public void setEntityBroker(EntityBroker entityBroker) {
		this.entityBroker = entityBroker;
	}
	
	private ExternalLogic externalLogic;
	public void setExternalLogic(ExternalLogic externalLogic) {
		this.externalLogic = externalLogic;
	}
	
	private AssignmentLogic assignmentLogic;
	public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
		this.assignmentLogic = assignmentLogic;
	}
	
	public void makeGradebookDetails(UIContainer tofill, String divID, AssignmentSubmission as, Long assignmentId, String userId){
		
		UIJointContainer joint = new UIJointContainer(tofill, divID, "gradebook_details:", ""+1);
		
		Assignment2 assignment = assignmentLogic.getAssignmentByIdWithAssociatedData(assignmentId);
    	//Grading Helper Link
		String url = externalLogic.getUrlForGradeGradebookItemHelper(assignment.getGradableObjectId(), userId, FinishedHelperProducer.VIEWID);
                
        UILink.make(joint, "gradebook_grading_helper",
        		UIMessage.make("assignment2.assignment_grade.gradebook_grade"),
        		url);
     
        UIOutput.make(joint, "gradebook_grade", (as!= null && as.getGradebookGrade() != null ? as.getGradebookGrade() : ""));
        UIOutput.make(joint, "gradebook_comment", (as != null && as.getGradebookComment() != null ? as.getGradebookComment() : ""));
	}
}