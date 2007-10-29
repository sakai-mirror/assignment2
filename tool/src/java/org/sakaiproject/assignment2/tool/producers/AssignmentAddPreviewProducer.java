package org.sakaiproject.assignment2.tool.producers;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.tool.beans.Assignment2Creator;
import org.sakaiproject.assignment2.tool.params.AssignmentAddViewParams;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UICommand;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIForm;
import uk.org.ponder.rsf.components.UIInput;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UISelect;
import uk.org.ponder.rsf.components.UISelectChoice;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.components.decorators.DecoratorList;
import uk.org.ponder.rsf.components.decorators.UILabelTargetDecorator;
import uk.org.ponder.rsf.evolvers.TextInputEvolver;
import uk.org.ponder.rsf.flow.ARIResult;
import uk.org.ponder.rsf.flow.ActionResultInterceptor;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCase;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCaseReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class AssignmentAddPreviewProducer implements ViewComponentProducer, ViewParamsReporter {

    public static final String VIEW_ID = "assignment_add-preview";
    public String getViewID() {
        return VIEW_ID;
    }


    private NavBarRenderer navBarRenderer;
    private MessageLocator messageLocator;
    private AssignmentLogic assignmentLogic;
    private ExternalLogic externalLogic;


    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	AssignmentAddViewParams params = (AssignmentAddViewParams) viewparams;
    	
    	Long assignmentId = params.assignmentId;
    	
    	UIMessage.make(tofill, "page-title", "assignment2.assignment_add-preview.title");
        navBarRenderer.makeNavBar(tofill, "navIntraTool:", VIEW_ID);

        Date openTime = new Date();
        Date dueDate = new Date();
        Date acceptUntilDate = new Date();
        String assignment2OTP = "Assignment2.";
        Assignment2 assignment;
        if (assignmentId != null) {
        	assignment2OTP += assignmentId; 
        	//get Dates
        	assignment = assignmentLogic.getAssignmentById(assignmentId);
        }else {
        	Assignment2Creator creator = new Assignment2Creator();
        	creator.setExternalLogic(externalLogic);
        	assignment = creator.create();
        	
        	assignment2OTP += EntityBeanLocator.NEW_PREFIX + "1";
        }
    	openTime = assignment.getOpenTime();
    	dueDate = assignment.getDueDateForUngraded();			//change here
    	acceptUntilDate = assignment.getCloseTime();
    	
    	UIMessage.make(tofill, "heading", "assignment2.assignment_add-preview.heading", new Object[]{ assignment.getTitle()});
    }
    
    public ViewParameters getViewParameters() {
        return new AssignmentAddViewParams();
    }

    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }

    public void setNavBarRenderer(NavBarRenderer navBarRenderer) {
        this.navBarRenderer = navBarRenderer;
    }
    
    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
    	this.assignmentLogic = assignmentLogic;
    }
    
    public void setExternalLogic(ExternalLogic externalLogic) {
    	this.externalLogic = externalLogic;
    }
}