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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.tool.params.FinishedHelperViewParameters;
import org.sakaiproject.assignment2.tool.params.ThickboxHelperViewParams;
import org.sakaiproject.assignment2.tool.producers.renderers.AttachmentListRenderer;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParamsReporter;
import uk.org.ponder.htmlutil.HTMLUtil;

import org.sakaiproject.content.api.ContentResource;
import org.sakaiproject.content.api.ContentHostingService;
import org.sakaiproject.content.api.FilePickerHelper;
import org.sakaiproject.entity.api.Reference;
import org.sakaiproject.tool.api.SessionManager;
import org.sakaiproject.tool.api.ToolSession;

import java.util.List;

/**
 * The FinishedHelper View is a little special. It is redirected to when the 
 * FilePicker is done, see the {@link AddAttachmentHelperProducer} navigation
 * cases. 
 * 
 * This pages sole content is made up of several &lt;script$gt; tags which
 * produce javascript to resize the frame and interact with the Thickbox to 
 * close it down and return to the regular screen. See {@link ThickboxHelperViewParams}
 * for more information about the Thickbox parameters. The important thing in 
 * the template is that parent.tb_remove() closes the dialog.
 * 
 * @author rjlowe
 * @author sgithens
 *
 */
public class FinishedHelperProducer implements ViewComponentProducer, ViewParamsReporter
{
    private static final Log LOG = LogFactory.getLog(FinishedHelperProducer.class);
    public static final String VIEWID = "FinishedHelper";

    public String getViewID() {
        return VIEWID;
    }

    private SessionManager sessionManager;
    public void setSessionManager(SessionManager sessionManager) {
        this.sessionManager = sessionManager;
    }

    private ContentHostingService contentHostingService;
    public void setContentHostingService(ContentHostingService contentHostingService) {
        this.contentHostingService = contentHostingService;
    }

    private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }

    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {

        FinishedHelperViewParameters params = (FinishedHelperViewParameters) viewparams;

        //Really do nothing, let the JS do it all, call thickbox close window and Ajax call
        // except call frame resize because the parent document window may have changed

        UIVerbatim.make(tofill, "sizeFrame",
                HTMLUtil.emitJavascriptCall("parent.a2SetMainFrameHeight",
                        new String[] {org.sakaiproject.util.Web.escapeJavascript(
                                "Main" + org.sakaiproject.tool.cover.ToolManager.getCurrentPlacement().getId())
                }
                )
        );

        //check session for attachment refs returned from a file picker helper
        ToolSession toolSession = sessionManager.getCurrentToolSession();
        if (toolSession.getAttribute(FilePickerHelper.FILE_PICKER_CANCEL) == null &&
                toolSession.getAttribute(FilePickerHelper.FILE_PICKER_ATTACHMENTS) != null) 
        {
            List<Reference> refs = (List)toolSession.getAttribute(FilePickerHelper.FILE_PICKER_ATTACHMENTS);
            int i=0;
            String markup = "";
            for (Reference ref : refs) {
                try{
                    //TODO - but all contentHosting calls in an external Logic
                    ContentResource cr = contentHostingService.getResource(ref.getId());
                    markup += HTMLUtil.emitJavascriptCall("parent.updateAttachments", 
                            new String[]{externalLogic.getContentTypeImagePath(cr), 
                            cr.getProperties().getProperty(cr.getProperties().getNamePropDisplayName()), 
                            cr.getUrl(), ref.getId(), externalLogic.getReadableFileSize(cr.getContentLength())});  

                } catch(Exception e) {
                    //do nothing
                    LOG.error(e.getMessage(), e);
                }

                i++;
            }
            UIVerbatim.make(tofill, "updateAttachments", markup);
            //Here are my references... now emit a JS call to add these references to the UI
            //Then remove the FilePickerBean
            //Then make the UI just duplicate or create a row for attachments
            //Then do it for all attachments on the site
            toolSession.removeAttribute(FilePickerHelper.FILE_PICKER_ATTACHMENTS);
            toolSession.removeAttribute(FilePickerHelper.FILE_PICKER_CANCEL);
        }

        if (params.value != null && !params.value.equals("")) {
            UIVerbatim.make(tofill, "useValue", 
                    HTMLUtil.emitJavascriptCall("parent.useValue", params.value));
        }
    }

    public ViewParameters getViewParameters() {
        return new FinishedHelperViewParameters();
    }

}