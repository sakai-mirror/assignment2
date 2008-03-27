package org.sakaiproject.assignment2.tool.producers;

import java.util.ArrayList;
import java.util.List;

import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.assignment2.tool.params.ViewSubmissionsViewParams;
import org.sakaiproject.assignment2.tool.params.ZipViewParams;

import uk.org.ponder.rsf.components.UIBoundBoolean;
import uk.org.ponder.rsf.components.UICommand;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIELBinding;
import uk.org.ponder.rsf.components.UIForm;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.flow.ARIResult;
import uk.org.ponder.rsf.flow.ActionResultInterceptor;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCase;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCaseReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class UploadAllProducer implements ViewComponentProducer, ViewParamsReporter,
		NavigationCaseReporter, ActionResultInterceptor
{
	public static final String VIEW_ID = "uploadall";

	public String getViewID()
	{
		return VIEW_ID;
	}

	public void fillComponents(UIContainer tofill, ViewParameters viewparams,
			ComponentChecker checker)
	{
		AssignmentViewParams params = (AssignmentViewParams) viewparams;

		ZipViewParams zvp = new ZipViewParams("zipSubmissions", params.assignmentId);
		UIInternalLink.make(tofill, "downloadtemplate", UIMessage
				.make("assignment2.assignment_grade-assignment.downloadall.button"), zvp);

		String uploadOptions = "uploadBean.uploadOptions";
		UIForm upload_form = UIForm.make(tofill, "upload_form");
		upload_form
				.addParameter(new UIELBinding(uploadOptions + ".assignmentId", zvp.assignmentId));

		// Render checkboxes for uploadable elements
		UIBoundBoolean.make(upload_form, "gradeFile", uploadOptions + ".gradeFile");
		UIBoundBoolean.make(upload_form, "feedbackText", uploadOptions + ".feedbackText");
		UIBoundBoolean.make(upload_form, "comments", uploadOptions + ".comments");
		UIBoundBoolean.make(upload_form, "feedbackAttachments", uploadOptions
				+ ".feedbackAttachments");

		// Render buttons
		UICommand.make(upload_form, "uploadButton", UIMessage.make("assignment2.uploadall.upload"),
				"uploadBean.processUpload");
		 UICommand.make(upload_form, "cancelButton", UIMessage.make("assignment2.uploadall.cancel"))
				.setReturn(ViewSubmissionsProducer.VIEW_ID);
	}

	public ViewParameters getViewParameters()
	{
		return new AssignmentViewParams();
	}

	public List<NavigationCase> reportNavigationCases()
	{
		List<NavigationCase> nav = new ArrayList<NavigationCase>();
		nav.add(new NavigationCase(ViewSubmissionsProducer.VIEW_ID, new ViewSubmissionsViewParams(
				ViewSubmissionsProducer.VIEW_ID, null)));
		return nav;
	}

	public void interceptActionResult(ARIResult result, ViewParameters incoming, Object actionReturn)
	{
		if (result.resultingView instanceof ViewSubmissionsViewParams)
		{
			ViewSubmissionsViewParams outgoing = (ViewSubmissionsViewParams) result.resultingView;
			AssignmentViewParams in = (AssignmentViewParams) incoming;
			outgoing.assignmentId = in.assignmentId;
		}
	}
}