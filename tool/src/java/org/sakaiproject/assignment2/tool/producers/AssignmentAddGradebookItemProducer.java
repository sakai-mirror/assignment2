package org.sakaiproject.assignment2.tool.producers;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIBoundBoolean;
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
import uk.org.ponder.rsf.evolvers.FormatAwareDateInputEvolver;
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

public class AssignmentAddGradebookItemProducer implements ViewComponentProducer {

    public static final String VIEW_ID = "assignment_add-gradebook-item";
    public String getViewID() {
        return VIEW_ID;
    }

    private String reqStar = "<span class=\"reqStar\">*</span>";

    private NavBarRenderer navBarRenderer;
    private MessageLocator messageLocator;
    
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

        //set dateEvolver
        dateEvolver.setStyle(FormatAwareDateInputEvolver.DATE_INPUT);
        Date date = new Date();
        
    	
        UIMessage.make(tofill, "page-title", "assignment2.assignment_add-gradebook-item.title");
        UIMessage.make(tofill, "heading", "assignment2.assignment_add-gradebook-item.heading");
        UIVerbatim.make(tofill, "instructions", messageLocator.getMessage("assignment2.assignment_add-gradebook-item.instructions",
        		new Object[]{ reqStar }));
        
        //Start Form
        UIForm form = UIForm.make(tofill, "form");
        UIMessage.make(form, "gradebook_item_legend", "assignment2.assignment_add-gradebook-item.gradebook_item_legend");
        
        UIVerbatim.make(form, "title_label", messageLocator.getMessage("assignment2.assignment_add-gradebook-item.title_label",
        		new Object[]{ reqStar }));
        UIInput.make(form, "title", "Title Here");
        
        UIVerbatim.make(form, "point_label", messageLocator.getMessage("assignment2.assignment_add-gradebook-item.point_label",
        		new Object[]{ reqStar }));
        UIInput.make(form, "point", "POINTS HERE");
        
        UIVerbatim.make(form, "due_date_label", messageLocator.getMessage("assignment2.assignment_add-gradebook-item.due_date_label",
        		new Object[]{ reqStar }));
        UIInput due_date = UIInput.make(form, "due_date:", "");
        dateEvolver.evolveDateInput(due_date, date);
        
        UIMessage.make(form, "category_label", "assignment2.assignment_add-gradebook-item.category_label");
        UISelect category_select = UISelect.make(form, "category", new String[] {}, new String[] {}, "");
        UIMessage.make(form, "category_instruction", "assignment2.assignment_add-gradebook-item.category_instruction");
        
        UIMessage.make(form, "release_label", "assignment2.assignment_add-gradebook-item.release_label");
        UIBoundBoolean.make(form, "release");
        
        UIMessage.make(form, "course_grade_label", "assignment2.assignment_add-gradebook-item.course_grade_label");
        UIBoundBoolean.make(form, "course_grade");
        
        
        //Action Buttons
        UICommand.make(form, "add_item", "assignment2.assignment_add-gradebook-item.add_item", "#{GradebookItemBean.processActionAddItem}");
        UICommand.make(form, "cancel", "assignment2.assignment_add-gradebook-item.cancel", "#{GradebookItemBean.processActionCancel}");
    }

    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }


    public void setNavBarRenderer(NavBarRenderer navBarRenderer) {
        this.navBarRenderer = navBarRenderer;
    }
    
}