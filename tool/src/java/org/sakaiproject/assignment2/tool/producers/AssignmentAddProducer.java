package org.sakaiproject.assignment2.tool.producers;

import org.sakaiproject.assignment2.tool.beans.Assignment2Creator;
import org.sakaiproject.assignment2.tool.beans.PreviewAssignmentBean;
import org.sakaiproject.assignment2.tool.params.AssignmentAddViewParams;
import org.sakaiproject.assignment2.tool.params.SimpleAssignmentViewParams;
import org.sakaiproject.assignment2.tool.params.FilePickerHelperViewParams;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.GradebookItem;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.lang.String;
import java.util.Locale;

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
import uk.org.ponder.rsf.components.UILink;
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

    String reqStar = "<span class=\"reqStar\">*</span>";

    private NavBarRenderer navBarRenderer;
    private TextInputEvolver richTextEvolver;
    private MessageLocator messageLocator;
    private AssignmentLogic assignmentLogic;
    private ExternalLogic externalLogic;
    private ExternalGradebookLogic externalGradebookLogic;
    private PreviewAssignmentBean previewAssignmentBean;
    private Locale locale;
    
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
    	//Get View Params
    	AssignmentAddViewParams params = (AssignmentAddViewParams) viewparams;
    	
    	//get Passed assignmentId to pull in for editing if any
    	Long assignmentId = params.assignmentId;
    	
    	// use a date which is related to the current users locale
        DateFormat df = DateFormat.getDateInstance(DateFormat.MEDIUM, locale);
        
        //Gradebook Helper Url
        String gradebook_helper_url = externalGradebookLogic.getGradebookItemHelperUrl(externalLogic.getCurrentContextId()) + 
    	"?KeepThis=true&TB_iframe=true&height=500&width=700";
            	
        UIMessage.make(tofill, "page-title", "assignment2.assignment_add.title");
        navBarRenderer.makeNavBar(tofill, "navIntraTool:", VIEW_ID);
        UIMessage.make(tofill, "heading", "assignment2.assignment_add.heading");
        UIVerbatim.make(tofill, "instructions", messageLocator.getMessage("assignment2.assignment_add.instructions", 
        		new Object[]{ reqStar }));

        Date openTime = new Date();
        Date dueDate = new Date();
        Date acceptUntilDate = new Date();
        String assignment2OTP = "Assignment2.";
        Assignment2 assignment;
        if (assignmentId != null) {
        	assignment2OTP += assignmentId; 
        	//get Dates
        	assignment = assignmentLogic.getAssignmentById(assignmentId);
        } else if (params.fromViewId != null && params.fromViewId.equals(AssignmentPreviewProducer.VIEW_ID) && previewAssignmentBean.getAssignment() != null) {
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
    	acceptUntilDate = assignment.getAcceptUntilTime();
        
        UIForm form = UIForm.make(tofill, "assignment_form");
        
        //set dateEvolver
        dateEvolver.setStyle(FormatAwareDateInputEvolver.DATE_TIME_INPUT);
        
        UIVerbatim.make(form, "title_label", messageLocator.getMessage("assignment2.assignment_add.assignment_title",
        		new Object[]{ reqStar }));
        UIInput.make(form, "title", assignment2OTP + ".title", assignment.getTitle());
        UIVerbatim.make(form, "open_date_label", messageLocator.getMessage("assignment2.assignment_add.open_date",
        		new Object[]{ reqStar }));
        UIInput openDateField = UIInput.make(form, "open_date:", assignment2OTP + ".openTime");
		dateEvolver.evolveDateInput(openDateField, openTime);
		UIMessage.make(form, "open_date_instruction", "assignment2.assignment_add.open_date_instruction");
        
		UIVerbatim.make(form, "due_date_label", messageLocator.getMessage("assignment2.assignment_add.due_date",
        		new Object[]{ reqStar }));
		UIInput dueDateField = UIInput.make(form, "due_date:", assignment2OTP + ".dueDateForUngraded");
		dateEvolver.evolveDateInput(dueDateField, dueDate);
		
		UIVerbatim.make(form, "accept_until_label", messageLocator.getMessage("assignment2.assignment_add.accept_until",
        		new Object[]{ reqStar }));
        UIInput acceptUntilTimeField = UIInput.make(form, "accept_until:", assignment2OTP + ".acceptUntilTime");
        dateEvolver.evolveDateInput(acceptUntilTimeField, acceptUntilDate);
        UIMessage.make(form, "accept_until_instruction", "assignment2.assignment_add.accept_until_instruction");
        
        UIVerbatim.make(form, "student_submissions_label", messageLocator.getMessage("assignment2.assignment_add.student_submissions",
        		new Object[]{ reqStar }));
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
        //Honor Pledge
        UIBoundBoolean.make(form, "honor_pledge", assignment2OTP + ".honorPledge", assignment.isHonorPledge());
        
        //Attachments
        UIInternalLink.make(form, "add_attachments", UIMessage.make("assignment2.assignment_add.add_attachments"),
        		new FilePickerHelperViewParams(AddAttachmentHelperProducer.VIEWID, Boolean.TRUE, 
        				Boolean.TRUE, 500, 700, assignment.getAssignmentId()));
        				
        
        
        /********
         *Grading
         */  
        //Get Gradebook Items
        List<GradebookItem> gradebook_items = externalGradebookLogic.getViewableGradebookItems(externalLogic.getCurrentContextId());
        //Get an Assignment for currently selected from the select box
        // by default this the first item on the list returend from the externalGradebookLogic
        // this will be overwritten if we have a pre-existing assignment with an assigned
        // item
        GradebookItem currentSelected = new GradebookItem();
        if (gradebook_items.size() > 0) {
        	currentSelected = gradebook_items.get(0);
        }
        
        String[] gradebook_item_labels = new String[gradebook_items.size()];
        String[] gradebook_item_values = new String[gradebook_items.size()];
        for (int i=0; i < gradebook_items.size(); i++) {
        	//Fill out select options
        	gradebook_item_labels[i] = gradebook_items.get(i).getTitle();
        	gradebook_item_values[i] = gradebook_items.get(i).getGradableObjectId().toString();
        	
        	//CHeck if currently selected
        	if (gradebook_items.get(i).getGradableObjectId() == assignment.getGradableObjectId()) {
        		currentSelected = gradebook_items.get(i);
        	}
        }
        UISelect.make(form, "gradebook_item",gradebook_item_values, gradebook_item_labels, assignment2OTP + ".gradableObjectId"); 
        
        //Radio Buttons for Grading
        UISelect grading_select = UISelect.make(form, "grading_select", 
        		new String[]{Boolean.FALSE.toString(), Boolean.TRUE.toString()}, assignment2OTP + ".ungraded", assignment.isUngraded().toString());
        String grading_select_id = grading_select.getFullID();
        UISelectChoice graded = UISelectChoice.make(form, "graded", grading_select_id, 0);
        UISelectChoice ungraded = UISelectChoice.make(form, "ungraded", grading_select_id, 1);
        
        //Check if gradebook item due date is not null, else output the formatted date
        if (currentSelected == null || currentSelected.getDueDate() == null) {
        	UIMessage.make(form, "gradebook_item_due_date", "assignment2.assignment_add.gradebook_item_no_due_date");
        } else {
        	UIOutput.make(form, "gradebook_item_due_date", df.format(currentSelected.getDueDate()));
        }

        UILink.make(form, "gradebook_item_new_helper",
        		UIMessage.make("assignment2.assignment_add.gradebook_item_new_helper"),
        		gradebook_helper_url);
        UILink.make(form, "gradebook_item_edit_helper",
        		UIMessage.make("assignment2.assignment_add.gradebook_item_edit_helper"),
        		gradebook_helper_url);
        
        
        //Java Scripting to hide due dates
        Map selectattrmap = new HashMap();
        selectattrmap.put("onclick", "if(this.checked){assignment_selected_gradebook_item(true)}else{assignment_selected_gradebook_item(false)}");
        graded.decorators = new DecoratorList(new UIFreeAttributeDecorator(selectattrmap));
        selectattrmap = new HashMap();
        selectattrmap.put("onclick", "if(!this.checked){assignment_selected_gradebook_item(true)}else{assignment_selected_gradebook_item(false)}");
        ungraded.decorators = new DecoratorList(new UIFreeAttributeDecorator(selectattrmap));
        if (assignment.isUngraded()) {
        	UIVerbatim.make(tofill, "due_date_init", "$('.gradebook_item_due_date').hide()");
        } else {
        	UIVerbatim.make(tofill, "due_date_init", "$('.due_date').hide()");
        }
        
        
        /******
         * Access
         */
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
        if (assignment == null || assignment.getAssignmentId() == null || assignment.isDraft()){
        	UICommand.make(form, "save_draft", UIMessage.make("assignment2.assignment_add.save_draft"), "#{Assignment2Bean.processActionSaveDraft}");
        }
        UICommand.make(form, "cancel_assignment", UIMessage.make("assignment2.assignment_add.cancel_assignment"), "#{Assignment2Bean.processActionCancel}");
        
    }

	public List reportNavigationCases() {
    	List<NavigationCase> nav= new ArrayList<NavigationCase>();
        nav.add(new NavigationCase("post", new SimpleViewParameters(
            AssignmentListSortViewProducer.VIEW_ID)));
        nav.add(new NavigationCase("preview", new AssignmentAddViewParams(
        	AssignmentPreviewProducer.VIEW_ID, null, AssignmentAddProducer.VIEW_ID)));
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
    
    public void setExternalGradebookLogic(ExternalGradebookLogic externalGradebookLogic) {
    	this.externalGradebookLogic = externalGradebookLogic;
    }
    
    public void setPreviewAssignmentBean(PreviewAssignmentBean previewAssignmentBean) {
    	this.previewAssignmentBean = previewAssignmentBean;
    }
    
    public void setLocale(Locale locale) {
    	this.locale = locale;
    }
}