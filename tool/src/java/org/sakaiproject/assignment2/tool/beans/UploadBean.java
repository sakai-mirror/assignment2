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
import java.util.Map;
import java.util.zip.ZipFile;

import org.sakaiproject.assignment2.logic.UploadAllLogic;
import org.sakaiproject.assignment2.logic.UploadException;
import org.sakaiproject.assignment2.model.UploadAllOptions;
import org.sakaiproject.assignment2.tool.producers.ViewSubmissionsProducer;
import org.springframework.web.multipart.MultipartFile;

import uk.org.ponder.messageutil.TargettedMessage;
import uk.org.ponder.messageutil.TargettedMessageList;

public class UploadBean
{
	private UploadAllOptions uploadOptions;
	private TargettedMessageList messages;
	private UploadAllLogic updownLogic;
	private Map<String, MultipartFile> uploads;

	public void setTargettedMessageList(TargettedMessageList messages)
	{
		this.messages = messages;
	}

	public void setUploadAllLogic(UploadAllLogic updownLogic)
	{
		this.updownLogic = updownLogic;
	}

	public void setMultipartMap(Map<String, MultipartFile> uploads)
	{
		this.uploads = uploads;
	}

	public UploadAllOptions getUploadOptions()
	{
		if (uploadOptions == null)
			uploadOptions = new UploadAllOptions();
		return uploadOptions;
	}

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
	}
}