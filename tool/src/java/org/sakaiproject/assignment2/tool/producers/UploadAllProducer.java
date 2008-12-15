/**********************************************************************************
 * $URL$
 * $Id$
 ***********************************************************************************
 *
 * Copyright (c) 2007, 2008 The Sakai Foundation.
 *
 * Licensed under the Educational Community License, Version 1.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.opensource.org/licenses/ecl1.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 **********************************************************************************/

package org.sakaiproject.assignment2.tool.producers;

import java.util.ArrayList;
import java.util.List;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.assignment2.tool.params.ViewSubmissionsViewParams;
import org.sakaiproject.assignment2.tool.params.ZipViewParams;

import uk.org.ponder.messageutil.MessageLocator;
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
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

/**
 * This view shows the form for uploading grades.
 * 
 * One thing to note is the implementation of both {@link NavigationCaseReporter}
 * and {@link ActionResultInterceptor}. During runtime processing, RSF will try
 * to use the Navigation Cases first, and then try Action Result Interceptors.
 * 
 * TODO FIXME I'm pretty sure we can get rid of the NavigationCaseReporter, but
 * can't get a CSV file to upload so I can't test it.  Come back here later. SWG
 * 
 * @author sgithens
 * @author wagnermr
 * @author stuart.freeman
 * @author carl.hall
 *
 */
public class UploadAllProducer implements ViewComponentProducer, ViewParamsReporter,
NavigationCaseReporter, ActionResultInterceptor
{
    public static final String VIEW_ID = "uploadall";

    public String getViewID()
    {
        return VIEW_ID;
    }
    
    // Dependency
    private MessageLocator messageLocator;
    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }
    
    // Dependency
    private AssignmentLogic assignmentLogic;
    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
        this.assignmentLogic = assignmentLogic;
    }

    /*
     **** This is the original work for upload all. For now, we are only going to upload grades
	 public void fillComponents(UIContainer tofill, ViewParameters viewparams,
			ComponentChecker checker)
	{
		AssignmentViewParams params = (AssignmentViewParams) viewparams;

		ZipViewParams zvp = new ZipViewParams("zipSubmissions", params.assignmentId);
		UIInternalLink.make(tofill, "downloadtemplate", UIMessage
				.make("assignment2.assignment_grade-assignment.downloadall.button"), zvp);

		String uploadOptions = "UploadBean.uploadOptions";
		UIForm upload_form = UIForm.make(tofill, "upload_form");
		upload_form
				.addParameter(new UIELBinding(uploadOptions + ".assignmentId", zvp.assignmentId));

		// Render checkboxes for uploadable elements
		UIBoundBoolean.make(upload_form, "gradeFile", uploadOptions + ".gradeFile");
		UIBoundBoolean.make(upload_form, "feedbackText", uploadOptions + ".feedbackText");
		UIBoundBoolean.make(upload_form, "feedbackAttachments", uploadOptions
				+ ".feedbackAttachments");

		// Render buttons
		UICommand.make(upload_form, "uploadButton", UIMessage.make("assignment2.uploadall.upload"),
				"UploadBean.processUpload");
		 UICommand.make(upload_form, "cancelButton", UIMessage.make("assignment2.uploadall.cancel"))
				.setReturn(ViewSubmissionsProducer.VIEW_ID);
	}*/

    public void fillComponents(UIContainer tofill, ViewParameters viewparams,
            ComponentChecker checker)
    {
        AssignmentViewParams params = (AssignmentViewParams) viewparams;
        
        Assignment2 assignment = assignmentLogic.getAssignmentById(params.assignmentId);
        
        // Make BreadCrumbs
        UIInternalLink.make(tofill, "breadcrumb_asnn_list", 
                messageLocator.getMessage("assignment2.list.heading"),
                new SimpleViewParameters(ListProducer.VIEW_ID));
        UIInternalLink.make(tofill, "breadcrumb_asnn_submissions", 
                messageLocator.getMessage("assignment2.upload_grades.breadcrumb.back_to_submissions", assignment.getTitle() )
                , new ViewSubmissionsViewParams(ViewSubmissionsProducer.VIEW_ID, assignment.getId()));
        

        ZipViewParams zvp = new ZipViewParams("zipSubmissions", params.assignmentId);
        UIForm upload_form = UIForm.make(tofill, "upload_form");
        upload_form.addParameter(new UIELBinding("UploadBean.uploadOptions.assignmentId", zvp.assignmentId));

        // Render buttons
        UICommand.make(upload_form, "uploadButton", UIMessage.make("assignment2.uploadall.upload"),
            "UploadBean.processUploadGradesCSV");
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