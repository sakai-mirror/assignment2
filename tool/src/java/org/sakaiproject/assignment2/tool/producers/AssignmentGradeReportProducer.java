package org.sakaiproject.assignment2.tool.producers;

import org.sakaiproject.assignment2.tool.params.PagerViewParams;
import org.sakaiproject.assignment2.tool.producers.renderers.PagerRenderer;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class AssignmentGradeReportProducer implements ViewComponentProducer, ViewParamsReporter {

    public static final String VIEW_ID = "assignment_grade-report";
    public String getViewID() {
        return VIEW_ID;
    }


    private MessageLocator messageLocator;
    private PagerRenderer pagerRenderer;


    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	PagerViewParams pagerparams = (PagerViewParams) viewparams;
    	
    	Integer total_count = 0;
    	
        UIMessage.make(tofill, "page-title", "assignment2.assignment_grade-report.title");
        pagerRenderer.makePager(tofill, "pagerDiv:", VIEW_ID, pagerparams, total_count);
        UIMessage.make(tofill, "heading", "assignment2.assignment_grade-report.heading");
        
    }

    public ViewParameters getViewParameters(){
    	return new PagerViewParams();
    }
    
    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }
    
    public void setPagerRenderer(PagerRenderer pagerRenderer) {
    	this.pagerRenderer = pagerRenderer;
    }
}