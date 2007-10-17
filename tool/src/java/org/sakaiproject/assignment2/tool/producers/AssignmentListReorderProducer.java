package org.sakaiproject.assignment2.tool.producers;

import java.util.ArrayList;
import java.util.List;

import org.sakaiproject.assignment2.tool.producers.AssignmentListSortViewProducer;
import org.sakaiproject.assignment2.tool.producers.NavBarRenderer;
import org.sakaiproject.assignment2.tool.producers.PagerRenderer;
import org.sakaiproject.assignment2.tool.beans.PagerBean;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIBranchContainer;
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

public class AssignmentListReorderProducer implements ViewComponentProducer {

    public static final String VIEW_ID = "assignment_list-reorder";
    public String getViewID() {
        return VIEW_ID;
    }

    private NavBarRenderer navBarRenderer;
    private PagerBean pagerBean;
    private PagerRenderer pagerRenderer;
    private MessageLocator messageLocator;


    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {

    	//get paging data
    	int total_count = 17;
    	pagerBean.setTotalCount(total_count);
    	
        UIMessage.make(tofill, "page-title", "assignment2.assignment_list-reorder.title");
        navBarRenderer.makeNavBar(tofill, "navIntraTool:", VIEW_ID);
        pagerRenderer.makePager(tofill, "pagerDiv:", VIEW_ID);
        UIMessage.make(tofill, "heading", "assignment2.assignment_list-sortview.heading");

        //Links
        UIInternalLink.make(tofill, "assignment_list-sortview-link",
					UIMessage.make("assignment2.assignment_list-sortview.title"),
				new SimpleViewParameters(AssignmentListSortViewProducer.VIEW_ID));
        
        //Fill out UL
        for (int i=0; i < 4; i ++){
        	UIBranchContainer row = UIBranchContainer.make(tofill, "assignment_li:");
        	UIOutput.make(row, "assignment_title", "Homework Example 2");
        	UIMessage.make(row, "assignment_due_on", "assignment2.assignment_list-reorder.due_on");
        	UIOutput.make(row, "assignment_duedate", "Oct 1, 2007 6:00 pm");
        }
        
    }

    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }

    
    public void setNavBarRenderer(NavBarRenderer navBarRenderer) {
        this.navBarRenderer = navBarRenderer;
    }
    
    public void setPagerBean(PagerBean pagerBean){
    	this.pagerBean = pagerBean;
    }
    
    public void setPagerRenderer(PagerRenderer pagerRenderer){
    	this.pagerRenderer = pagerRenderer;
    }
}