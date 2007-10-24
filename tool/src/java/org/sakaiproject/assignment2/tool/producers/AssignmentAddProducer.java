package org.sakaiproject.assignment2.tool.producers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.org.ponder.arrayutil.ListUtil;
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
import uk.org.ponder.rsf.components.decorators.UIFreeAttributeDecorator;
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

public class AssignmentAddProducer implements ViewComponentProducer, NavigationCaseReporter {

    public static final String VIEW_ID = "assignment_add";
    public String getViewID() {
        return VIEW_ID;
    }


    private NavBarRenderer navBarRenderer;
    private TextInputEvolver richTextEvolver;
    private MessageLocator messageLocator;


    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {

        UIMessage.make(tofill, "page-title", "assignment2.assignment_add.title");
        navBarRenderer.makeNavBar(tofill, "navIntraTool:", VIEW_ID);
        UIMessage.make(tofill, "heading", "assignment2.assignment_add.heading");

        String assignment2OTP = "Assignment2." + EntityBeanLocator.NEW_PREFIX + "1";
        
        UIForm form = UIForm.make(tofill, "assignment_form");
                
        UIInput.make(form, "new_assignment_title", assignment2OTP + ".title");
        UIInput field = UIInput.make(form, "new_assignment_open_date", assignment2OTP + ".openTime");
        Map attrmap = new HashMap(); 
		attrmap.put("id", "new_assignment_date");
		field.decorators = new DecoratorList(new UIFreeAttributeDecorator(attrmap));
        
		field = UIInput.make(form, "new_assignment_due_date", assignment2OTP + ".closeTime");
        attrmap = new HashMap(); 
		attrmap.put("id", "assignment_due_date");
		field.decorators = new DecoratorList(new UIFreeAttributeDecorator(attrmap));
		
        field = UIInput.make(form, "new_assignment_accept_until", assignment2OTP + ".dropDeadTime");
        attrmap = new HashMap(); 
		attrmap.put("id", "accept_until");
		field.decorators = new DecoratorList(new UIFreeAttributeDecorator(attrmap));
        
        //Rich Text Input
        UIInput instructions = UIInput.make(form, "new_assignment_instructions:", assignment2OTP + ".instructions");
        richTextEvolver.evolveTextInput(instructions);
        
        //Post Buttons
        UICommand.make(form, "post_assignment", UIMessage.make("assignment2.assignment_add.post"), "#{Assignment2Bean.processActionPost}");
    }

    public List reportNavigationCases() {
        return ListUtil.instance(new NavigationCase("post", new SimpleViewParameters(
            AssignmentListSortViewProducer.VIEW_ID)));
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
    
}