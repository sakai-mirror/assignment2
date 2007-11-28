package org.sakaiproject.assignment2.tool.producers;

import java.util.ArrayList;
import java.util.List;

import org.sakaiproject.authz.api.PermissionsHelper;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.content.api.FilePickerHelper;
import org.sakaiproject.tool.api.SessionManager;
import org.sakaiproject.tool.api.ToolSession;
import org.sakaiproject.content.api.FilePickerHelper;
import org.sakaiproject.assignment2.tool.params.FilePickerHelperViewParams;

import uk.ac.cam.caret.sakai.rsf.helper.HelperViewParameters;
import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.ParameterList;
import uk.org.ponder.rsf.components.UICommand;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIELBinding;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCase;
import uk.org.ponder.rsf.flow.jsfnav.NavigationCaseReporter;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;

public class AddAttachmentHelperProducer implements ViewComponentProducer, ViewParamsReporter, NavigationCaseReporter
{
  public static final String VIEWID = "AddAttachment";
  
  public String getViewID() {
    return VIEWID;
  }
  
  private SessionManager sessionManager;

  public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
    FilePickerHelperViewParams params = (FilePickerHelperViewParams) viewparams;
	  
	  
	//parameters for helper
	ToolSession toolSession = sessionManager.getCurrentToolSession();
	toolSession.setAttribute(FilePickerHelper.FILE_PICKER_TITLE_TEXT, "XML File Data Import");
	toolSession.setAttribute(FilePickerHelper.FILE_PICKER_INSTRUCTION_TEXT, "Please select an XML data file from which to read data.");
	toolSession.setAttribute(FilePickerHelper.FILE_PICKER_MAX_ATTACHMENTS, FilePickerHelper.CARDINALITY_MULTIPLE);
	  
    UIOutput.make(tofill, HelperViewParameters.HELPER_ID, "sakai.filepicker");
    UICommand goattach = UICommand.make(tofill, HelperViewParameters.POST_HELPER_BINDING, "#{FilePickerBean.process}");
    goattach.parameters = new ParameterList();
    goattach.parameters.add(new UIELBinding("FilePickerBean.otpkey", params.otpkey));
  }

  public ViewParameters getViewParameters() {
      return new FilePickerHelperViewParams();
  }
  
  
  public List reportNavigationCases() {
    List l = new ArrayList();
    l.add(new NavigationCase("processed", new SimpleViewParameters(FinishedHelperProducer.VIEWID)));
    return l;
  }
  
  public void setSessionManager(SessionManager sessionManager) {
	  this.sessionManager = sessionManager;
  }
}