package org.sakaiproject.assignment2.tool.producers;

import java.text.DateFormat;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.HashSet;

import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.GradebookItem;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.AssignmentSubmissionAttachment;
import org.sakaiproject.assignment2.tool.params.FragmentGradebookDetailsViewParams;
import org.sakaiproject.content.api.ContentHostingService;
import org.sakaiproject.entitybroker.EntityBroker;
import org.sakaiproject.entitybroker.IdEntityReference;

import org.sakaiproject.tool.api.SessionManager;
import org.sakaiproject.tool.api.ToolSession;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIForm;
import uk.org.ponder.rsf.components.UILink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UISelect;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.content.ContentTypeReporter;
import uk.org.ponder.rsf.content.ContentTypeInfoRegistry;
import uk.org.ponder.rsf.evolvers.FormatAwareDateInputEvolver;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class FragmentGradebookDetailsProducer implements ViewComponentProducer, ContentTypeReporter, ViewParamsReporter{
	
    public static final String VIEW_ID = "fragment-gradebook-details";
    public String getViewID() {
        return VIEW_ID;
    }
    	
	private ExternalLogic externalLogic;
	public void setExternalLogic(ExternalLogic externalLogic) {
		this.externalLogic = externalLogic;
	}

	private GradebookDetailsRenderer gradebookDetailsRenderer;
	public void setGradebookDetailsRenderer(GradebookDetailsRenderer gradebookDetailsRenderer){
		this.gradebookDetailsRenderer = gradebookDetailsRenderer;
	}
	
	private AssignmentSubmissionLogic assignmentSubmissionLogic;
	public void setAssignmentSubmissionLogic(AssignmentSubmissionLogic assignmentSubmissionLogic) {
		this.assignmentSubmissionLogic = assignmentSubmissionLogic;
	}
	
    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    	FragmentGradebookDetailsViewParams params = (FragmentGradebookDetailsViewParams) viewparams;
    	
    	
    	AssignmentSubmission as = assignmentSubmissionLogic.getCurrentSubmissionByAssignmentIdAndStudentIdForInstructorView(params.assignmentId, params.userId);
    	gradebookDetailsRenderer.makeGradebookDetails(tofill, "gradebook_details", as, params.assignmentId, params.userId);
    	
    }
    	
	public String getContentType() {
		return ContentTypeInfoRegistry.HTML_FRAGMENT;
	}

	public ViewParameters getViewParameters() {
		return new FragmentGradebookDetailsViewParams();
	}
}