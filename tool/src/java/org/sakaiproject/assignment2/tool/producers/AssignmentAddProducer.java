package org.sakaiproject.assignment2.tool.producers;

import org.sakaiproject.assignment2.tool.beans.Assignment2Creator;
import org.sakaiproject.assignment2.tool.beans.PreviewAssignmentBean;
import org.sakaiproject.assignment2.tool.params.AssignmentAddViewParams;
import org.sakaiproject.assignment2.tool.params.SimpleAssignmentViewParams;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.lang.String;

import uk.org.ponder.arrayutil.ListUtil;
import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIBoundBoolean;
import uk.org.ponder.rsf.components.UIBoundString;
import uk.org.ponder.rsf.components.UIBranchContainer;
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
import uk.org.ponder.rsf.components.UISelectLabel;
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
import org.sakaiproject.site.api.Group;

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
    private PreviewAssignmentBean previewAssignmentBean;
    
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

    @SuppressWarnings("unchecked")
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
        Assignment2 assignment;
        if (assignmentId != null) {
        	assignment2OTP += assignmentId; 
        	//get Dates
        	assignment = assignmentLogic.getAssignmentById(assignmentId);
        } else if (params.fromPreview && previewAssignmentBean.getAssignment() != null) {
        	//from Preview page
        	assignment = previewAssignmentBean.getAssignment();
        	assignment2OTP += EntityBeanLocator.NEW_PREFIX + "1";
        } else {
        	//create new
        	Assignment2Creator creator = new Assignment2Creator();
        	creator.setExternalLogic(externalLogic);
        	assignment = creator.create();
        	
        	assignment2OTP += EntityBeanLocator.NEW_PREFIX + "1";
        }
    	openTime = assignment.getOpenTime();
    	dueDate = assignment.getDueDateForUngraded();			//change here
    	acceptUntilDate = assignment.getCloseTime();
        
        UIForm form = UIForm.make(tofill, "assignment_form");
        
        //set dateEvolver
        dateEvolver.setStyle(FormatAwareDateInputEvolver.DATE_TIME_INPUT);
        
        UIInput.make(form, "title", assignment2OTP + ".title", assignment.getTitle());
        UIInput openDateField = UIInput.make(form, "open_date:", assignment2OTP + ".openTime");
		dateEvolver.evolveDateInput(openDateField, openTime);
        
		UIInput closeTimeField = UIInput.make(form, "due_date:", assignment2OTP + ".dueDateForUngraded");
		dateEvolver.evolveDateInput(closeTimeField, dueDate);
		
        UIInput dropDeadTimeField = UIInput.make(form, "accept_until:", assignment2OTP + ".closeTime");
        dateEvolver.evolveDateInput(dropDeadTimeField, acceptUntilDate);
        
        //Submission Types
        String[] submission_type_values = new String[] {
        		String.valueOf(AssignmentConstants.SUBMIT_INLINE_ONLY),
        		String.valueOf(AssignmentConstants.SUBMIT_ATTACH_ONLY),
        		String.valueOf(AssignmentConstants.SUBMIT_INLINE_AND_ATTACH),
        		String.valueOf(AssignmentConstants.SUBMIT_NON_ELECTRONIC)
        };
        String[] submisison_type_labels = new String[] {
        		"assignment2.submission_type." + String.valueOf(AssignmentConstants.SUBMIT_INLINE_ONLY),
        		"assignment2.submission_type." + String.valueOf(AssignmentConstants.SUBMIT_ATTACH_ONLY),
        		"assignment2.submission_type." + String.valueOf(AssignmentConstants.SUBMIT_INLINE_AND_ATTACH),
        		"assignment2.submission_type." + String.valueOf(AssignmentConstants.SUBMIT_NON_ELECTRONIC)
        };
        UISelect select =UISelect.make(form, "submission_type", submission_type_values,
        		submisison_type_labels, assignment2OTP + ".submissionType").setMessageKeys();
        ((UIBoundString) select.selection).setValue(String.valueOf(assignment.getSubmissionType()));
        
        //Rich Text Input
        UIInput instructions = UIInput.make(form, "instructions:", assignment2OTP + ".instructions", assignment.getInstructions());
        richTextEvolver.evolveTextInput(instructions);
        
        
        //Calendar Due Date
        //Announcement
        UIBoundBoolean.make(form, "honor_pledge", assignment2OTP + ".honorPledge", assignment.isHonorPledge());
        
        //Access
        UIMessage.make(form, "access_legend", "assignment2.assignment_add.access_legend");
        String[] access_values = new String[] {
        		Boolean.FALSE.toString(),
        		Boolean.TRUE.toString()
        };
        String[] access_labels = new String[] {
        		"assignment2.assignment_add.access.not_restricted",
        		"assignment2.assignment_add.access.restricted"
        };
        UISelect access = UISelect.make(form, "access_select", access_values, access_labels,
        		assignment2OTP + ".restrictedToGroups").setMessageKeys();
        ((UIBoundString) select.selection).setValue(assignment.isRestrictedToGroups().toString());
        
        String accessId = access.getFullID();
        for (int i=0; i < access_values.length; i++) {
        	UIBranchContainer access_row = UIBranchContainer.make(form, "access_row:");
        	UISelectChoice checkbox = UISelectChoice.make(access_row, "access_choice", accessId, i);
        	Map attrmap = new HashMap();
        	String id = org.sakaiproject.util.Web.escapeJavascript("Main" + org.sakaiproject.tool.cover.ToolManager.getCurrentPlacement().getId());
        	if (access_values[i].equals(Boolean.FALSE.toString())) {
        		attrmap.put("onclick", "$('li#groups_table_li').hide();a2SetMainFrameHeight('" + id + "');");
        	} else {
        		attrmap.put("onclick", "$('li#groups_table_li').show();a2SetMainFrameHeight('" + id + "');");
        	}
        	checkbox.decorators = new DecoratorList(new UIFreeAttributeDecorator(attrmap));
        	UISelectLabel.make(access_row, "access_label", accessId, i);
        }
        
        //Groups
        UIOutput groups_table_li = UIOutput.make(form, "groups_table_li");
        if (!assignment.isRestrictedToGroups()){
	        Map attrmap = new HashMap(); 
			attrmap.put("style", "display:none");
	        groups_table_li.decorators = new DecoratorList(new UIFreeAttributeDecorator(attrmap));
        }
        Collection<Group> groups = externalLogic.getSiteGroups();
        for (Group g : groups){
	        UIBranchContainer groups_row = UIBranchContainer.make(form, "groups_row:");
	        UIBoundBoolean.make(groups_row, "group_check");
	        UIOutput.make(groups_row, "group_label", g.getTitle());
	        UIOutput.make(groups_row, "group_description", g.getDescription());
        }
        
        
        //Notifications
        UIMessage.make(form, "notification_legend", "assignment2.assignment_add.notification_legend");
        String[] notification_type_values = new String[] {
        		String.valueOf(AssignmentConstants.NOTIFY_NONE),
        		String.valueOf(AssignmentConstants.NOTIFY_FOR_EACH),
        		String.valueOf(AssignmentConstants.NOTIFY_DAILY_SUMMARY)
        };
        String[] notification_type_labels = new String[] {
        		"assignment2.assignment_add.notification_type.notify_none",
        		"assignment2.assignment_add.notification_type.notify_each",
        		"assignment2.assignment_add.notification_type.notify_daily"
        };
        UISelect notifications = UISelect.make(form, "notifications_select", notification_type_values,
        		notification_type_labels, assignment2OTP + ".notificationType").setMessageKeys();
        ((UIBoundString) notifications.selection).setValue(String.valueOf(assignment.getNotificationType()));
        String notificationSelectId = notifications.getFullID();
        for (int i = 0; i < notification_type_values.length; i++){
        	UIBranchContainer notification_row = UIBranchContainer.make(form, "notification_row:");
        	UISelectChoice.make(notification_row, "notification_choice", notificationSelectId, i);
        	UISelectLabel.make(notification_row, "notification_label", notificationSelectId, i);
        }
        
        
        
        //Post Buttons
        UICommand.make(form, "post_assignment", UIMessage.make("assignment2.assignment_add.post"), "#{Assignment2Bean.processActionPost}");
        UICommand.make(form, "preview_assignment", UIMessage.make("assignment2.assignment_add.preview"), "#{Assignment2Bean.processActionPreview}");
        UICommand.make(form, "save_draft", UIMessage.make("assignment2.assignment_add.save_draft"), "#{Assignment2Bean.processActionSaveDraft}");
        UICommand.make(form, "cancel_assignment", UIMessage.make("assignment2.assignment_add.cancel_assignment"), "#{Assignment2Bean.processActionCancel}");
        
    }

	public List reportNavigationCases() {
    	List<NavigationCase> nav= new ArrayList<NavigationCase>();
        nav.add(new NavigationCase("post", new SimpleViewParameters(
            AssignmentListSortViewProducer.VIEW_ID)));
        nav.add(new NavigationCase("preview", new SimpleAssignmentViewParams(
        	AssignmentAddPreviewProducer.VIEW_ID, null)));
        nav.add(new NavigationCase("save_draft", new SimpleViewParameters(
        	AssignmentListSortViewProducer.VIEW_ID)));
        nav.add(new NavigationCase("cancel", new SimpleViewParameters(
        	AssignmentListSortViewProducer.VIEW_ID)));
        return nav;
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
    
    public void setPreviewAssignmentBean(PreviewAssignmentBean previewAssignmentBean) {
    	this.previewAssignmentBean = previewAssignmentBean;
    }
}