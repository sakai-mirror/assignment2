package org.sakaiproject.assignment2.tool.producers;

import org.sakaiproject.assignment2.tool.beans.Assignment2Creator;
import org.sakaiproject.assignment2.tool.params.AssignmentAddViewParams;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.org.ponder.arrayutil.ListUtil;
import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UICommand;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIELBinding;
import uk.org.ponder.rsf.components.UIForm;
import uk.org.ponder.rsf.components.UIInput;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UISelect;
import uk.org.ponder.rsf.components.UISelectChoice;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.components.decorators.DecoratorList;
import uk.org.ponder.rsf.components.decorators.UIFreeAttributeDecorator;
import uk.org.ponder.rsf.components.decorators.UILabelTargetDecorator;
import uk.org.ponder.rsf.evolvers.TextInputEvolver;
import uk.org.ponder.rsf.evolvers.FormatAwareDateInputEvolver;
import uk.org.ponder.rsf.flow.ARIResult;
import uk.org.ponder.rsf.flow.ActionResultInterceptor;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCase;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCaseReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class AssignmentAddProducer implements ViewComponentProducer, NavigationCaseReporter, ViewParamsReporter {

    public static final String VIEW_ID = "assignment_add";
    public String getViewID() {
        return VIEW_ID;
    }


    private NavBarRenderer navBarRenderer;
    private TextInputEvolver richTextEvolver;
    private MessageLocator messageLocator;
    private AssignmentLogic assignmentLogic;
    private ExternalLogic externalLogic;
    
	/*
	 * You can change the date input to accept time as well by uncommenting the lines like this:
	 * dateevolver.setStyle(FormatAwareDateInputEvolver.DATE_TIME_INPUT);
	 * and commenting out lines like this:
	 * dateevolver.setStyle(FormatAwareDateInputEvolver.DATE_INPUT);
	 * -AZ
	 * And vice versa - RWE
	 */
	private FormatAwareDateInputEvolver dateEvolver;
	public void setDateEvolver(FormatAwareDateInputEvolver dateEvolver) {
		this.dateEvolver = dateEvolver;
	}

    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	AssignmentAddViewParams params = (AssignmentAddViewParams) viewparams;
    	
    	Long assignmentId = params.assignmentId;
    	
        UIMessage.make(tofill, "page-title", "assignment2.assignment_add.title");
        navBarRenderer.makeNavBar(tofill, "navIntraTool:", VIEW_ID);
        UIMessage.make(tofill, "heading", "assignment2.assignment_add.heading");

        Date openTime = new Date();
        Date dueDate = new Date();
        Date acceptUntilDate = new Date();
        String assignment2OTP = "Assignment2.";
        if (assignmentId != null) {
        	assignment2OTP += assignmentId; 
        	//get Dates
        	Assignment2 assignment = assignmentLogic.getAssignmentById(assignmentId);
        	openTime = assignment.getOpenTime();
        	dueDate = assignment.getDueDateForUngraded();			//change here
        	acceptUntilDate = assignment.getCloseTime();
        }else {
        	Assignment2Creator creator = new Assignment2Creator();
        	creator.setExternalLogic(externalLogic);
        	Assignment2 assignment = creator.create();
        	openTime = assignment.getOpenTime();
        	dueDate = assignment.getDueDateForUngraded();			//change here
        	acceptUntilDate = assignment.getCloseTime();
        	
        	assignment2OTP += EntityBeanLocator.NEW_PREFIX + "1";
        }
        
        UIForm form = UIForm.make(tofill, "assignment_form");
        
        //set dateEvolver
        dateEvolver.setStyle(FormatAwareDateInputEvolver.DATE_TIME_INPUT);
        
        UIInput.make(form, "title", assignment2OTP + ".title");
        UIInput openDateField = UIInput.make(form, "open_date:", assignment2OTP + ".openTime");
		dateEvolver.evolveDateInput(openDateField, openTime);
        
		UIInput closeTimeField = UIInput.make(form, "due_date:", assignment2OTP + ".dueDateForUngraded");
		dateEvolver.evolveDateInput(closeTimeField, dueDate);
		
        UIInput dropDeadTimeField = UIInput.make(form, "accept_until:", assignment2OTP + ".closeTime");
        dateEvolver.evolveDateInput(dropDeadTimeField, acceptUntilDate);
        
        //Rich Text Input
        UIInput instructions = UIInput.make(form, "instructions:", assignment2OTP + ".instructions");
        richTextEvolver.evolveTextInput(instructions);
        
        //Post Buttons
        UICommand postCmd = UICommand.make(form, "post_assignment", UIMessage.make("assignment2.assignment_add.post"), "#{Assignment2Bean.processActionPost}");
        //postCmd.parameters.add(new UIELBinding("#{Assignment2.assignmentId}",
        //        assignmentId));

    }

    public List reportNavigationCases() {
        return ListUtil.instance(new NavigationCase("post", new SimpleViewParameters(
            AssignmentListSortViewProducer.VIEW_ID)));
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
    
    public void setRichTextEvolver(TextInputEvolver richTextEvolver) {
        this.richTextEvolver = richTextEvolver;
    }
    
    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
    	this.assignmentLogic = assignmentLogic;
    }
    
    public void setExternalLogic(ExternalLogic externalLogic) {
    	this.externalLogic = externalLogic;
    }
}