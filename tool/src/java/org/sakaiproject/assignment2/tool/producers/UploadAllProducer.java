package org.sakaiproject.assignment2.tool.producers;

import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.assignment2.tool.params.ZipViewParams;

import uk.org.ponder.rsf.components.UIBoundBoolean;
import uk.org.ponder.rsf.components.UICommand;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIForm;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class UploadAllProducer implements ViewComponentProducer, ViewParamsReporter
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

		UIForm upload_form = UIForm.make(tofill, "upload_form");

		// Render checkboxes for uploadable elements
		UIBoundBoolean.make(upload_form, "gradeFile");
		UIBoundBoolean.make(upload_form, "feedbackTexts");
		UIBoundBoolean.make(upload_form, "feedbackComments");
		UIBoundBoolean.make(upload_form, "feedbackAttachments");

		// Render buttons
		UICommand.make(upload_form, "uploadButton", UIMessage.make("assignment2.uploadall.upload"),
				"");
		UICommand.make(upload_form, "cancelButton", UIMessage.make("assignment2.uploadall.cancel"));
	}

	public ViewParameters getViewParameters()
	{
		return new AssignmentViewParams();
	}
}