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

package org.sakaiproject.assignment2.tool.beans;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.sakaiproject.assignment2.exception.UploadException;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.UploadGradesLogic;
import org.sakaiproject.assignment2.model.UploadAllOptions;
import org.sakaiproject.assignment2.tool.WorkFlowResult;
import org.sakaiproject.assignment2.tool.producers.ViewSubmissionsProducer;
import org.springframework.web.multipart.MultipartFile;

import uk.org.ponder.messageutil.TargettedMessage;
import uk.org.ponder.messageutil.TargettedMessageList;

/**
 * This bean is for binding the Upload Grades form. Currently thats the 
 * uploadall template and producer.
 * 
 * @author carl.hall
 * @author wagnermr
 * @author stuart.freeman
 * @author sgithens
 *
 */
public class UploadBean
{
    private UploadAllOptions uploadOptions;
    private Map<String, MultipartFile> uploads;

    private static final String FAILURE = "failure";

    // Property / Dependency
    private TargettedMessageList messages;
    public void setTargettedMessageList(TargettedMessageList messages) {
        this.messages = messages;
    }

    // Dependency
    private UploadGradesLogic uploadGradesLogic;
    public void setUploadGradesLogic(UploadGradesLogic uploadGradesLogic)
    {
        this.uploadGradesLogic = uploadGradesLogic;
    }

    public void setMultipartMap(Map<String, MultipartFile> uploads)
    {
        this.uploads = uploads;
    }

    // Property
    public UploadAllOptions getUploadOptions()
    {
        if (uploadOptions == null)
            uploadOptions = new UploadAllOptions();
        return uploadOptions;
    }

    // Dependency
    private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }
    
    /*
     * The members below are stateful variables used to store the uploaded data
     * while it's verified in the wizard/workflow
     */
    public Map<String, String> displayIdUserIdMap;
    public List<List<String>> parsedContent;

    /**
     * Action Method Binding for going back to the Upload after viewing the
     * parsed contents of the previous upload attempt.  
     * 
     * @return
     */
    public WorkFlowResult processBackToUpload() {
        // Clear previous data
        displayIdUserIdMap = null;
        parsedContent = null;
        return WorkFlowResult.UPLOADALL_CSV_BACK_TO_UPLOAD;
    }
    
    /**
     * Action Method Binding for confirming the save information processing
     * after viewing the parsed data from the upload.
     * 
     * 
     * @return
     */
    public WorkFlowResult processUploadConfirmAndSave() {
        // Putting in Confirm Dialog ASNN-313
        List<String> usersNotUpdated = uploadGradesLogic.uploadGrades(displayIdUserIdMap, uploadOptions.assignmentId, parsedContent);

        if (!usersNotUpdated.isEmpty()) {
            messages.addMessage(new TargettedMessage("assignment2.upload_grades.upload_successful_with_exception",
                    new Object[] {getListAsString(usersNotUpdated)}, TargettedMessage.SEVERITY_INFO));
        } else {
            messages.addMessage(new TargettedMessage("assignment2.upload_grades.upload_successful",
                    new Object[] {}, TargettedMessage.SEVERITY_INFO));
        }

        return WorkFlowResult.UPLOADALL_CSV_CONFIRM_AND_SAVE;
    }
    
    /**
     * Action Method Binding for the Upload Button on the inital page of the
     * upload workflow/wizard.
     * 
     * @return
     */
    public WorkFlowResult processUploadGradesCSV()
    {
        if (uploadOptions == null || uploadOptions.assignmentId == null ) {
            messages.addMessage(new TargettedMessage("No assignmentId was passed " +
                    "in the request to processUploadGradesCSV. Cannot continue.", new Object[] {},
                    TargettedMessage.SEVERITY_ERROR));
            return WorkFlowResult.UPLOADALL_CSV_UPLOAD_FAILURE;
        }

        if (uploads.isEmpty()) 
        {
            messages.addMessage(new TargettedMessage("assignment2.upload_grades.missing_file", new Object[] {},
                    TargettedMessage.SEVERITY_ERROR));
            return WorkFlowResult.UPLOADALL_CSV_UPLOAD_FAILURE;
        }

        MultipartFile uploadedFile = uploads.get("file");

        if (uploadedFile.getSize() == 0)
        {
            messages.addMessage(new TargettedMessage("assignment2.upload_grades.missing_file", new Object[] {},
                    TargettedMessage.SEVERITY_ERROR));
            return WorkFlowResult.UPLOADALL_CSV_UPLOAD_FAILURE;
        }

        if (!uploadedFile.getOriginalFilename().endsWith("csv"))
        {
            messages.addMessage(new TargettedMessage("assignment2.upload_grades.not_csv", new Object[] {},
                    TargettedMessage.SEVERITY_ERROR));
            return WorkFlowResult.UPLOADALL_CSV_UPLOAD_FAILURE;
        }

        // now let's parse the content of the file so we can do some validation
        // on the data before we attempt to save it. 
        String contextId = externalLogic.getCurrentContextId();

        File newFile = null;
        try {
            newFile = File.createTempFile(uploadedFile.getName(), ".csv");
            uploadedFile.transferTo(newFile);
        } catch (IOException ioe) {
            throw new UploadException(ioe.getMessage(), ioe);
        }

        // retrieve the displayIdUserId info once and re-use it
        displayIdUserIdMap = externalLogic.getUserDisplayIdUserIdMapForStudentsInSite(contextId);		
        parsedContent = uploadGradesLogic.getCSVContent(newFile);

        // let's check that the students included in the file are actually in the site
        List<String> invalidDisplayIds = uploadGradesLogic.getInvalidDisplayIdsInContent(displayIdUserIdMap, parsedContent);
        if (invalidDisplayIds != null && !invalidDisplayIds.isEmpty()) {
            messages.addMessage(new TargettedMessage("assignment2.upload_grades.user_not_in_site", 
                    new Object[] {getListAsString(invalidDisplayIds)}, TargettedMessage.SEVERITY_ERROR ));
            return WorkFlowResult.UPLOADALL_CSV_UPLOAD_FAILURE;
        }

        // check that the grades are valid
        List<String> displayIdsWithInvalidGrade = uploadGradesLogic.getStudentsWithInvalidGradesInContent(parsedContent, uploadOptions.assignmentId);
        if (displayIdsWithInvalidGrade != null && !displayIdsWithInvalidGrade.isEmpty()) {
            messages.addMessage(new TargettedMessage("assignment2.upload_grades.grades_not_valid", 
                    new Object[] {getListAsString(displayIdsWithInvalidGrade)}, TargettedMessage.SEVERITY_ERROR ));
            return WorkFlowResult.UPLOADALL_CSV_UPLOAD_FAILURE;
        }

        // let's proceed with the grade upload
        return WorkFlowResult.UPLOADALL_CSV_UPLOAD;
    }

    private String getListAsString(List<String> itemList) {
        StringBuilder sb = new StringBuilder();
        if (itemList != null) {
            for (int i = 0; i < itemList.size(); i++) {
                if (i != 0) {
                    sb.append(", ");
                }

                sb.append(itemList.get(i));
            }
        }

        return sb.toString();
    }

    /*
     * **** This is the original code for a full upload. For now, we are only updating grades via the csv
	 public String processUpload()
	{
		MultipartFile upFile = null;
		boolean isZip = false;
		if (uploads.isEmpty())
			messages.addMessage(new TargettedMessage("assignment2.uploadall.alert.zipFile"));
		else
		{
			upFile = uploads.get("file");
			if (upFile.getSize() == 0)
			{
				messages.addMessage(new TargettedMessage("assignment2.uploadall.alert.zipFile"));
			}
			else if ("application/zip".equals(upFile.getContentType()))
			{
				isZip = true;
			}
		}

		// check that at least 1 option has been selected
		if ((uploadOptions == null
				|| (!uploadOptions.feedbackText && !uploadOptions.gradeFile
						&& !uploadOptions.feedbackAttachments)) && isZip)
		{
			messages.addMessage(new TargettedMessage("assignment2.uploadall.alert.choose.element"));
		}
		else
		{
			try
			{
				File f = null;
				if (isZip)
				{
					f = File.createTempFile(upFile.getName(), ".zip");
					upFile.transferTo(f);
					updownLogic.uploadAll(uploadOptions, f);
				}
				else
				{
					f = File.createTempFile(upFile.getName(), ".csv");
					upFile.transferTo(f);
					updownLogic.uploadCSV(uploadOptions, f);
				}
			}
			catch (IOException ioe)
			{
				messages.addMessage(new TargettedMessage("assignment2.uploadall.exception",
						new Object[] { ioe.getMessage() }));
			}
			catch (UploadException ue)
			{
				messages.addMessage(new TargettedMessage("assignment2.uploadall.exception",
						new Object[] { ue.getMessage() }));
			}
		}

		return ViewSubmissionsProducer.VIEW_ID;
	}*/
}