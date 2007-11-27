package org.sakaiproject.assignment2.tool.producers;

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.tool.beans.Assignment2Creator;
import org.sakaiproject.assignment2.tool.beans.Assignment2Bean;
import org.sakaiproject.assignment2.tool.beans.PreviewAssignmentBean;
import org.sakaiproject.assignment2.tool.params.AssignmentAddViewParams;
import org.sakaiproject.assignment2.tool.params.SimpleAssignmentViewParams;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIBoundString;
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

public class AssignmentPreviewProducer implements ViewComponentProducer, NavigationCaseReporter, ViewParamsReporter, ActionResultInterceptor {

    public static final String VIEW_ID = "assignment_preview";
    public String getViewID() {
        return VIEW_ID;
    }


    private NavBarRenderer navBarRenderer;
    private MessageLocator messageLocator;
    private AssignmentLogic assignmentLogic;
    private ExternalLogic externalLogic;
	private PreviewAssignmentBean previewAssignmentBean;
	private Locale locale;

    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	AssignmentAddViewParams params = (AssignmentAddViewParams) viewparams;
    	
    	//use a date which is related to the current users locale
        DateFormat df = DateFormat.getDateInstance(DateFormat.MEDIUM, locale);
    	
    	UIMessage.make(tofill, "page-title", "assignment2.assignment_preview.title");
        navBarRenderer.makeNavBar(tofill, "navIntraTool:", VIEW_ID);

        Assignment2 assignment;
        if (params.fromViewId != null && params.assignmentId != null && params.fromViewId.equals(AssignmentListSortViewProducer.VIEW_ID)) {
        	assignment = assignmentLogic.getAssignmentById(params.assignmentId);
        } else {
        	assignment = previewAssignmentBean.getAssignment();
        }
    	UIMessage.make(tofill, "heading", "assignment2.assignment_preview.heading", new Object[]{ assignment.getTitle() });
    	
    	//Assignment Settings Table
    	UIMessage.make(tofill, "created_by_label", "assignment2.assignment_preview.created_by");
    	UIOutput.make(tofill, "created_by", externalLogic.getUserDisplayName(assignment.getCreator()));
    	UIMessage.make(tofill, "modified_label", "assignment2.assignment_preview.modified");
    	UIOutput.make(tofill, "modified", (assignment.getModifiedTime() != null ? df.format(assignment.getModifiedTime()) : ""));
    	UIMessage.make(tofill, "open_date_label", "assignment2.assignment_preview.open_date");
    	UIOutput.make(tofill, "open_date", df.format(assignment.getOpenTime()));
    	UIMessage.make(tofill, "due_date_label", "assignment2.assignment_preview.due_date");
    	UIOutput.make(tofill, "due_date", df.format(assignment.getDueDateForUngraded()));
    	UIMessage.make(tofill, "accept_until_label", "assignment2.assignment_preview.accept_until");
    	UIOutput.make(tofill, "accept_until", df.format(assignment.getAcceptUntilTime()));
    	UIMessage.make(tofill, "submission_type_label", "assignment2.assignment_preview.submission_type");
    	UIMessage.make(tofill, "submission_type", "assignment2.submission_type." + String.valueOf(assignment.getSubmissionType())); 
    	UIMessage.make(tofill, "graded_label", "assignment2.assignment_preview.graded");
    	UIMessage.make(tofill, "graded", (assignment.isUngraded() ? "assignment2.no" : "assignment2.yes")); 		//change here for more details
    	UIMessage.make(tofill, "calendar_label", "assignment2.assignment_preview.calendar");
    	UIMessage.make(tofill, "calendar", (assignment.getCalendarEventId() == null ? "assignment2.no" : "assignment2.yes"));
    	
    	// only display announcement option if the site has the Announcements tool
    	if (externalLogic.siteHasTool(externalLogic.getCurrentContextId(), ExternalLogic.TOOL_ID_ANNC)) {
	    	UIMessage.make(tofill, "announcement_label", "assignment2.assignment_preview.announcement");
	    	UIMessage.make(tofill, "announcement", (assignment.getHasAnnouncement() ? "assignment2.no" : "assignment2.yes"));
    	}
    	
    	UIMessage.make(tofill, "honor_pledge_label", "assignment2.assignment_preview.honor_pledge");
    	UIMessage.make(tofill, "honor_pledge", (assignment.isHonorPledge() ? "assignment2.required" : "assignment2.not_required"));
    	UIMessage.make(tofill, "gradebook_label", "assignment2.assignment_preview.gradebook");
    	if (assignment.getGradableObjectId() != null){
    		UIOutput.make(tofill, "gradebook", "Gradebook Title Here");
    	} else {
    		UIMessage.make(tofill, "gradebook", "assignment2.no");
    	}
    	
    	//Assignment Instructions
    	UIMessage.make(tofill, "assignment_instructions_heading", "assignment2.assignment_preview.assignment_instructions_heading");
    	UIOutput.make(tofill, "instructions", assignment.getInstructions());
    	
    	//Student View
    	UIMessage.make(tofill, "student_view", "assignment2.assignment_preview.student_view");
    	UIOutput.make(tofill, "student_view_title", assignment.getTitle());
    	UIMessage.make(tofill, "student_view_due_label", "assignment2.assignment_preview.student_view.due");
    	UIOutput.make(tofill, "student_view_due_date", df.format(assignment.getDueDateForUngraded()));
    	UIMessage.make(tofill, "student_view_status_label", "assignment2.assignment_preview.student_view.status");
    	Date now = new Date();
    	if (now.after(assignment.getOpenTime()) && now.before(assignment.getAcceptUntilTime())) {
    		UIMessage.make(tofill, "student_view_status", "assignment2.assignment_preview.student_view.open");
    	} else {
    		UIMessage.make(tofill, "student_view_status", "assignment2.assignment_preview.student_view.closed");
    	}
    	UIMessage.make(tofill, "student_view_grade_label", "assignment2.assignment_preview.student_view.grade");
    	UIOutput.make(tofill, "student_view_grade", "Points");   ///fix here
    	UIMessage.make(tofill, "student_view_instructions_label", "assignment2.assignment_preview.student_view.instructions");
    	UIOutput.make(tofill, "student_view_instructions", assignment.getInstructions());
    	UIMessage.make(tofill, "student_view_start", "assignment2.assignment_preview.student_view.start");
    	UIMessage.make(tofill, "student_view_start_instructions", "assignment2.assignment_preview.student_view.start_instructions");
    	UIMessage.make(tofill, "student_view_attachments_legend", "assignment2.assignment_preview.student_view.attachments");
    	UIMessage.make(tofill, "student_view_no_attachments", "assignment2.assignment_preview.student_view.no_attachments");
    	UIMessage.make(tofill, "student_view_add_attachments", "assignment2.assignment_preview.student_view.add_attachments");
    	
    	//Post Buttons
    	UIForm form = UIForm.make(tofill, "form");
    	if (params.fromViewId != null && params.fromViewId.equals(AssignmentListSortViewProducer.VIEW_ID)) {
    		UICommand.make(form, "back_to_list", UIMessage.make("assignment2.assignment_preview.back_to_list"), "#{Assignment2Bean.processActionBackToList}");
    	} else {
	    	UICommand.make(form, "post", UIMessage.make("assignment2.assignment_add.post"), "#{Assignment2Bean.processActionPreviewPost}");
	    	if (assignment == null || assignment.getAssignmentId() == null || assignment.isDraft()){
	    		UICommand.make(form, "save_draft", UIMessage.make("assignment2.assignment_add.save_draft"), "#{Assignment2Bean.processActionSaveDraft}");
	    	}
	        UICommand.make(tofill, "edit", UIMessage.make("assignment2.assignment_preview.edit"), "#{Assignment2Bean.processActionEdit}");
    	}
        
    }
    
    public ViewParameters getViewParameters() {
        return new AssignmentAddViewParams();
    }
    
	public List reportNavigationCases() {
    	List<NavigationCase> nav= new ArrayList<NavigationCase>();
    	nav.add(new NavigationCase("back_to_list", new SimpleViewParameters(
    		AssignmentListSortViewProducer.VIEW_ID)));
        nav.add(new NavigationCase("post", new SimpleViewParameters(
            AssignmentListSortViewProducer.VIEW_ID)));
        nav.add(new NavigationCase("failure", new AssignmentAddViewParams(
        	AssignmentAddProducer.VIEW_ID)));
        nav.add(new NavigationCase("preview", new SimpleAssignmentViewParams(
        	AssignmentPreviewProducer.VIEW_ID, null)));
        nav.add(new NavigationCase("save_draft", new SimpleViewParameters(
        	AssignmentListSortViewProducer.VIEW_ID)));
        nav.add(new NavigationCase("cancel", new SimpleViewParameters(
        	AssignmentListSortViewProducer.VIEW_ID)));
        return nav;
    }
	
	public void interceptActionResult(ARIResult result, ViewParameters incoming, Object actionReturn) {
		if (result.resultingView instanceof AssignmentAddViewParams) {
			AssignmentAddViewParams outgoing = (AssignmentAddViewParams) result.resultingView;
			outgoing.fromViewId = AssignmentPreviewProducer.VIEW_ID;
			outgoing.viewID = AssignmentAddProducer.VIEW_ID;
		}
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
        
    public void setPreviewAssignmentBean(PreviewAssignmentBean previewAssignmentBean) {
    	this.previewAssignmentBean = previewAssignmentBean;
    }
    
    public void setLocale(Locale locale) {
    	this.locale = locale;
    }
}