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
import uk.org.ponder.rsf.content.ContentTypeReporter;
import uk.org.ponder.rsf.content.ContentTypeInfoRegistry;
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

public class FragmentAssignmentPreviewProducer implements ViewComponentProducer, ViewParamsReporter, ContentTypeReporter {

    public static final String VIEW_ID = "fragment-assignment_preview";
    public String getViewID() {
        return VIEW_ID;
    }


    private AssignmentLogic assignmentLogic;
    private ExternalLogic externalLogic;
	private PreviewAssignmentBean previewAssignmentBean;
	private Locale locale;
	private AttachmentListRenderer attachmentListRenderer;

    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	AssignmentAddViewParams params = (AssignmentAddViewParams) viewparams;
    	
    	//use a date which is related to the current users locale
        DateFormat df = DateFormat.getDateInstance(DateFormat.MEDIUM, locale);
    	
    	UIMessage.make(tofill, "page-title", "assignment2.assignment_preview.title");
        //navBarRenderer.makeNavBar(tofill, "navIntraTool:", VIEW_ID);

        Assignment2 assignment;
        String OTPKey = "";
        if (params.fromViewId != null && params.assignmentId != null && params.fromViewId.equals(AssignmentListSortViewProducer.VIEW_ID)) {
        	//we are coming from the main page and are just viewing
        	assignment = assignmentLogic.getAssignmentByIdWithAssociatedData(params.assignmentId);
        	OTPKey = assignment.getAssignmentId().toString();
        } else {
        	//we are coming from the add/edit assignment page
        	assignment = previewAssignmentBean.getAssignment();
        	if (assignment.getAssignmentId() != null) {
        		OTPKey = assignment.getAssignmentId().toString();
        	} else {
        		OTPKey = EntityBeanLocator.NEW_PREFIX + "1";
        	}
        }
        UIMessage.make(tofill, "preview_heading", "assignment2.assignment_preview.heading", new Object[]{ assignment.getTitle() });
    	
    	//Assignment Settings Table
    	UIOutput.make(tofill, "preview_created_by", externalLogic.getUserDisplayName(assignment.getCreator()));
    	UIOutput.make(tofill, "preview_modified", (assignment.getModifiedTime() != null ? df.format(assignment.getModifiedTime()) : ""));
    	UIOutput.make(tofill, "preview_open_date", df.format(assignment.getOpenTime()));
    	UIOutput.make(tofill, "preview_due_date", df.format(assignment.getDueDateForUngraded()));
    	UIOutput.make(tofill, "preview_accept_until", df.format(assignment.getAcceptUntilTime()));
    	UIMessage.make(tofill, "preview_submission_type", "assignment2.submission_type." + String.valueOf(assignment.getSubmissionType())); 
    	UIMessage.make(tofill, "preview_graded", (assignment.isUngraded() ? "assignment2.no" : "assignment2.yes")); 		//change here for more details
    	//UIMessage.make(tofill, "calendar", (assignment.getCalendarEventId() == null ? "assignment2.no" : "assignment2.yes"));
    	
    	// only display announcement option if the site has the Announcements tool
    	if (externalLogic.siteHasTool(externalLogic.getCurrentContextId(), ExternalLogic.TOOL_ID_ANNC)) {
	    	UIMessage.make(tofill, "announcement_label", "assignment2.assignment_preview.announcement");
	    	UIMessage.make(tofill, "preview_announcement", (assignment.getHasAnnouncement() ? "assignment2.no" : "assignment2.yes"));
    	}
    	
    	UIMessage.make(tofill, "preview_honor_pledge", (assignment.isHonorPledge() ? "assignment2.required" : "assignment2.not_required"));
    	if (assignment.getGradableObjectId() != null){
    		UIOutput.make(tofill, "preview_gradebook", "Gradebook Title Here");
    	} else {
    		UIMessage.make(tofill, "preview_gradebook", "assignment2.no");
    	}
    	
    	//Assignment Instructions
    	UIVerbatim.make(tofill, "preview_instructions", assignment.getInstructions());
    	
    	//Student View
    	UIOutput.make(tofill, "student_view_title", assignment.getTitle());
    	UIOutput.make(tofill, "student_view_due_date", df.format(assignment.getDueDateForUngraded()));
    	Date now = new Date();
    	if (now.after(assignment.getOpenTime()) && now.before(assignment.getAcceptUntilTime())) {
    		UIMessage.make(tofill, "student_view_status", "assignment2.assignment_preview.student_view.open");
    	} else {
    		UIMessage.make(tofill, "student_view_status", "assignment2.assignment_preview.student_view.closed");
    	}
    	UIOutput.make(tofill, "student_view_grade", "Points");   ///fix here
    	UIVerbatim.make(tofill, "student_view_instructions", assignment.getInstructions());
    	if (assignment.getAttachmentSet() == null || assignment.getAttachmentSet().isEmpty()){
    		UIMessage.make(tofill, "student_view_no_attachments", "assignment2.assignment_preview.student_view.no_attachments");
    	}
    	
    	attachmentListRenderer.makeAttachmentFromAssignmentAttachmentSet(tofill, "attachment_list:", params.viewID, 
    		assignment.getAttachmentSet(), Boolean.FALSE);

        
    }
    
    public ViewParameters getViewParameters() {
        return new AssignmentAddViewParams();
    }
	
	public String getContentType() {
		return ContentTypeInfoRegistry.HTML_FRAGMENT;
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
    
	public void setAttachmentListRenderer(AttachmentListRenderer attachmentListRenderer){
		this.attachmentListRenderer = attachmentListRenderer;
	}
}