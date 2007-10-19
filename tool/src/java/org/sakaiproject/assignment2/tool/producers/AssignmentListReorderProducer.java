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
import uk.org.ponder.rsf.components.UIComponent;
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
import uk.org.ponder.rsf.components.decorators.UIFreeAttributeDecorator;
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
import java.util.Map;
import java.util.HashMap;

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
        pagerRenderer.makePager(tofill, "pagerDiv:", VIEW_ID, viewparams);
        UIMessage.make(tofill, "heading", "assignment2.assignment_list-sortview.heading");

        //Links
        UIInternalLink.make(tofill, "assignment_list-sortview-link",
					UIMessage.make("assignment2.assignment_list-sortview.title"),
				new SimpleViewParameters(AssignmentListSortViewProducer.VIEW_ID));
        
        
        //Fill out Table & UL
        for (int i=0; i < 4; i ++){
        	UIBranchContainer row = UIBranchContainer.make(tofill, "assignment-row:");
        	if (i == 0){
        		UIOutput cell = UIOutput.make(row, "reorder_cell");
        	}
        	
        	//Sorting LI
        	UIBranchContainer li = UIBranchContainer.make(tofill, "assignment_li:");
        	Map attrmap = new HashMap();
        	attrmap.put("id", "li_" + i);
        	li.decorators = new DecoratorList(new UIFreeAttributeDecorator(attrmap));
        	UIOutput.make(li, "assignment_row_title", "Homework Example " + i);
        	
        	//Table Row
        	UIInternalLink.make(row, "assignment_row_link", "Homework Example " + i, new SimpleViewParameters(AssignmentListReorderProducer.VIEW_ID));
        	UIInternalLink.make(row, "assignment_row_edit", "Edit", new SimpleViewParameters(AssignmentListReorderProducer.VIEW_ID));
        	UIInternalLink.make(row, "assignment_row_duplicate", "Duplicate", new SimpleViewParameters(AssignmentListReorderProducer.VIEW_ID));
        	UIInternalLink.make(row, "assignment_row_grade", "Grade Assignment", new SimpleViewParameters(AssignmentListReorderProducer.VIEW_ID));
        	
        	UIOutput.make(row, "assignment_row_for", "Site");
        	UIOutput.make(row, "assignment_row_status", "Open");
        	UIOutput.make(row, "assignment_row_open", "Sep 24, 2007 11:00 am");
        	UIOutput.make(row, "assignment_row_due", "Oct 1, 2007 6:00 pm");
        	UIInternalLink.make(row, "assignment_row_in_new", "2/2", new SimpleViewParameters(AssignmentListReorderProducer.VIEW_ID));
        	UIOutput.make(row, "assignment_row_scale", "0-100.0");
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