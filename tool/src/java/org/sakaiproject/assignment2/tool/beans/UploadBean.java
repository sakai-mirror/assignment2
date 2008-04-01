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
		// make sure a file was uploaded
		// if (file == null)
		// messages.addMessage(new TargettedMessage("uploadall.alert.zipFile"));

		// check that the uploaded file isn't over the limit
		// String max_file_size_mb = scs.getString("content.upload.max", "1");
		// int max_bytes = 1024 * 1024;
		// try
		// {
		// max_bytes = Integer.parseInt(max_file_size_mb) * 1024 * 1024;
		// }
		// catch (NumberFormatException e)
		// {
		// // if unable to parse an integer from the value
		// // in the properties file, use 1 MB as a default
		// max_file_size_mb = "1";
		// max_bytes = 1024 * 1024;
		// }
		// if(zipFile.length >= max_bytes)
		// {
		// addAlert(state, rb.getString("uploadall.size") + " " + max_file_size_mb + "MB " +
		// rb.getString("uploadall.exceeded"));
		// }
		MultipartFile upFile = null;
		if (uploads.isEmpty())
			messages.addMessage(new TargettedMessage("assignment2.uploadall.alert.zipFile"));
		else
		{
			upFile = uploads.get("file");
			if (upFile.getSize() == 0)
			{
				messages.addMessage(new TargettedMessage("assignment2.uploadall.alert.zipFile"));
			}
		}

		// check that at least 1 option has been selected
		if (uploadOptions == null
				|| (!uploadOptions.feedbackText && !uploadOptions.gradeFile
						&& !uploadOptions.comments && !uploadOptions.feedbackAttachments))
		{
			messages.addMessage(new TargettedMessage("assignment2.uploadall.alert.choose.element"));
		}
		else
		{
			try
			{
				File f = File.createTempFile(upFile.getName(), ".zip");
				upFile.transferTo(f);
				ZipFile zipFile = new ZipFile(f);
				updownLogic.uploadAll(uploadOptions, zipFile);
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