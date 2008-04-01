package org.sakaiproject.assignment2.logic.impl;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.AssignmentBundleLogic;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.UploadAllLogic;
import org.sakaiproject.assignment2.logic.UploadException;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.FeedbackAttachment;
import org.sakaiproject.assignment2.model.FeedbackVersion;
import org.sakaiproject.assignment2.model.UploadAllOptions;
import org.sakaiproject.content.api.ContentHostingService;
import org.sakaiproject.content.api.ContentResourceEdit;
import org.sakaiproject.content.api.ContentTypeImageService;
import org.sakaiproject.entity.api.ResourceProperties;
import org.sakaiproject.exception.IdInvalidException;
import org.sakaiproject.exception.IdUsedException;
import org.sakaiproject.exception.InconsistentException;
import org.sakaiproject.exception.OverQuotaException;
import org.sakaiproject.exception.PermissionException;
import org.sakaiproject.exception.ServerOverloadException;
import org.sakaiproject.util.StringUtil;

/**
 * Functionality for uploading and downloading an archive bundle
 * 
 * @author <a href="mailto:carl.hall@et.gatech.edu">Carl Hall</a>
 */
public class UploadAllLogicImpl implements UploadAllLogic
{
	private static final Log log = LogFactory.getLog(UploadAllLogicImpl.class);

	private AssignmentLogic assnLogic;
	private AssignmentSubmissionLogic assnSubLogic;
	private ContentHostingService chs;
	private ContentTypeImageService ctis;
	private ExternalGradebookLogic gradebookLogic;
	private AssignmentBundleLogic bundle;
	private ExternalLogic externalLogic;

	public void setAssignmentLogic(AssignmentLogic assnLogic)
	{
		this.assnLogic = assnLogic;
	}

	public void setContentHostingService(ContentHostingService chs)
	{
		this.chs = chs;
	}

	public void setContentTypeImageService(ContentTypeImageService ctis)
	{
		this.ctis = ctis;
	}

	public void setExternalGradebookLogic(ExternalGradebookLogic gradebookLogic)
	{
		this.gradebookLogic = gradebookLogic;
	}

	public void setAssignmentSubmissionLogic(AssignmentSubmissionLogic assnSubLogic)
	{
		this.assnSubLogic = assnSubLogic;
	}

	public void setAssignmentBundleLogic(AssignmentBundleLogic bundle)
	{
		this.bundle = bundle;
	}

	public void setExternalLogic(ExternalLogic externalLogic)
	{
		this.externalLogic = externalLogic;
	}

	/**
	 * @see org.sakaiproject.assignment2.logic.UploadAllLogic#uploadAll(java.lang.Long,
	 *      org.sakaiproject.assignment2.model.UploadAllOptions, java.util.zip.ZipFile,
	 *      java.lang.String, java.lang.String)
	 */
	public void uploadAll(UploadAllOptions options, ZipFile zipFile) throws UploadException
	{
		if (options == null)
			throw new IllegalArgumentException("options cannot be null.");

		// construct the hashtable for all submission objects
		Hashtable<String, UploadGradeWrapper> submisTable = new Hashtable<String, UploadGradeWrapper>();
		Assignment2 assn = assnLogic.getAssignmentById(options.assignmentId);

		if (zipFile == null)
			throw new IllegalArgumentException("zipFile cannot be null.");

		Enumeration<? extends ZipEntry> entries = zipFile.entries();
		String feedbackAttachmentFolder = bundle
				.getString("assignment2.download.feedback.attachment");
		while (entries.hasMoreElements())
		{
			ZipEntry entry = entries.nextElement();
			String entryName = entry.getName();
			if (!entry.isDirectory())
			{
				try
				{
					InputStream is = zipFile.getInputStream(entry);
					if (entryName.endsWith("grades.csv"))
					{
						if (options.gradeFile)
						{
							processGrades(submisTable, assn, StringUtil
									.trimToZero(readIntoString(is)));
						}
					}
					else
					{
						processEntry(options, feedbackAttachmentFolder, submisTable, is, entryName);
					}
				}
				catch (Exception e)
				{
					log.warn(e.getMessage(), e);
					// catch everything from these calls and wrap with generic UploadException
					throw new UploadException(e.getMessage(), e);
				}
			}
		}

		String userId = externalLogic.getCurrentUserId();
		String format = "yyyy-MM-dd hh:mm:ss.S";
		SimpleDateFormat dateFormat = new SimpleDateFormat(format);
		for (Map.Entry<String, UploadGradeWrapper> entry : submisTable.entrySet())
		{
			String userEid = entry.getKey();
			UploadGradeWrapper w = entry.getValue();
			try
			{
				// save the feedback changes
				// date format is derived from output in ZipExporter which uses Date.toString()
				FeedbackVersion feedback = assnSubLogic.getFeedbackByUserIdAndSubmittedTime(
						userEid, dateFormat.parse(w.timeStamp));
				feedback.getFeedbackAttachSet();
				if (options.comments)
					feedback.setFeedbackNotes(w.comment);
				if (options.feedbackText)
					feedback.setAnnotatedText(w.feedbackText);
				if (options.feedbackAttachments)
					feedback.setFeedbackAttachSet(w.feedbackAttachments);
				feedback.setLastFeedbackSubmittedBy(userId);
				assnSubLogic.updateFeedbackForVersion(feedback);

				// save the grade
				gradebookLogic.saveGradeAndCommentForStudent(assn.getContextId(), assn
						.getGradableObjectId(), userEid, w.grade, null);
			}
			catch (ParseException pe)
			{
				String msg = "Unable to parse date [" + w.timeStamp + "]; expected " + format;
				log.warn(msg, pe);
				throw new UploadException(msg, pe);
			}
		}
	}

	private void processEntry(UploadAllOptions options, String feedbackAttachmentFolder,
			Hashtable<String, UploadGradeWrapper> submisTable, InputStream zin, String entryName)
			throws IOException, InconsistentException, IdUsedException, IdInvalidException,
			ServerOverloadException, PermissionException, OverQuotaException
	{
		String userEid = getUserEid(entryName);
		UploadGradeWrapper r = submisTable.get(userEid);
		if (r == null)
			r = new UploadGradeWrapper();

		if (options.comments && entryName.contains("comments"))
		{
			// read the comments file
			String comment = getBodyTextFromZipHtml(zin);
			if (comment != null)
				r.comment = comment;
		}
		if (options.feedbackText && entryName.contains("feedbackText"))
		{
			// upload the feedback text
			String text = getBodyTextFromZipHtml(zin);
			if (text != null)
				r.feedbackText = text;
		}
		if (options.feedbackAttachments)
		{
			// upload the feedback attachment
			String submissionFolder = "/" + feedbackAttachmentFolder + "/";
			if (entryName.contains(submissionFolder))
			{
				// clear the submission attachment first
				r.feedbackAttachments = new HashSet<FeedbackAttachment>();
				uploadZipAttachments(submisTable, readIntoBytes(zin), entryName, userEid);
			}
		}

		// if this is a timestamp file
		if (entryName.contains("timestamp"))
		{
			byte[] timeStamp = readIntoBytes(zin);
			r.timeStamp = new String(timeStamp);
		}
		submisTable.put(userEid, r);
	}

	private String getUserEid(String entryName)
	{
		// get user eid part
		String userEid = "";
		if (entryName.contains("/"))
		{
			// remove the part of zip name
			userEid = entryName.substring(entryName.indexOf("/") + 1);
			// get out the user name part
			if (userEid.contains("/"))
				userEid = userEid.substring(0, userEid.indexOf("/"));

			// get the eid part
			if (userEid.contains("("))
				userEid = userEid.substring(userEid.indexOf("(") + 1, userEid.indexOf(")"));

			userEid = StringUtil.trimToNull(userEid);
		}
		return userEid;
	}

	private void processGrades(Hashtable<String, UploadGradeWrapper> submisTable, Assignment2 assn,
			String result) throws UploadException
	{
		// read grades.cvs from zip
		String[] lines = splitLine(result);
		// skip the first 3 lines because they are headers
		// - line 1: assignment title, type of grading
		// - line 2: blank line
		// - line 3: column header
		// -- [display id, user id, last name, first name, grade]
		// define position constants
		final int DISP_ID = 0;
		final int GRADE = 4;
		for (int i = 3; i < lines.length; i++)
		{
			String[] items = lines[i].split(",");
			if (items.length > 4)
			{
				// has grade information
				String displayId = items[DISP_ID];
				UploadGradeWrapper w = (UploadGradeWrapper) submisTable.get(displayId);
				if (w == null)
					w = new UploadGradeWrapper();
				String itemGrade = items[GRADE];
				if (gradebookLogic.isGradeValid(assn.getContextId(), itemGrade))
					w.grade = itemGrade;
				else
				{
					String msg = bundle.getFormattedMessage("assignment2.uploadall.invalid.grade",
							new Object[] { assn.getId(), itemGrade });
					throw new UploadException(msg);
				}
			}
		}
	}

	/**
	 * Splits a String into an array of lines based on predictable line endings.
	 * 
	 * @param result
	 * @return
	 */
	private String[] splitLine(String result)
	{
		String[] lines = null;
		if (result.contains("\r\n"))
			lines = result.split("\r\n");
		else if (result.contains("\r"))
			lines = result.split("\r");
		else if (result.contains("\n"))
			lines = result.split("\n");
		return lines;
	}

	private byte[] readIntoBytes(InputStream zin) throws IOException
	{
		byte[] buffer = new byte[4096];
		ByteArrayOutputStream out = new ByteArrayOutputStream();

		int len = -1;
		while ((len = zin.read(buffer)) > 0)
		{
			out.write(buffer, 0, len);
		}

		byte[] data = out.toByteArray();
		return data;
	}

	private String readIntoString(InputStream zin) throws IOException
	{
		StringBuilder buffer = new StringBuilder();
		int size = 2048;
		byte[] data = new byte[size];

		while ((size = zin.read(data, 0, data.length)) > 0)
			buffer.append(new String(data, 0, size));

		return buffer.toString();
	}

	private String getBodyTextFromZipHtml(InputStream zin) throws IOException
	{
		String rv = StringUtil.trimToNull(readIntoString(zin));
		if (rv != null)
		{
			int start = rv.indexOf("<body>");
			int end = rv.indexOf("</body>");
			if (start != -1 && end != -1)
			{
				// get the text in between
				rv = rv.substring(start + 6, end);
			}
		}
		return rv;
	}

	/**
	 * This is to get the submission or feedback attachment from the upload zip file into the
	 * submission object
	 * 
	 * @param submissionTable
	 * @param content
	 * @param entryName
	 * @param userEid
	 */
	private void uploadZipAttachments(Hashtable<String, UploadGradeWrapper> submissionTable,
			byte[] content, String entryName, String userEid) throws InconsistentException,
			IdUsedException, IdInvalidException, IOException, ServerOverloadException,
			PermissionException, OverQuotaException
	{
		// upload all the files as instructor attachments to the submission for grading purpose
		String fName = entryName.substring(entryName.lastIndexOf("/") + 1, entryName.length());
		// get file extension for detecting content type
		// ignore those hidden files
		String extension = "";
		if (!fName.contains(".") || (fName.contains(".") && fName.indexOf(".") != 0))
		{
			// add the file as attachment
			ResourceProperties properties = chs.newResourceProperties();
			properties.addProperty(ResourceProperties.PROP_DISPLAY_NAME, fName);

			String[] parts = fName.split("\\.");
			if (parts.length > 1)
				extension = parts[parts.length - 1];

			String contentType = ctis.getContentType(extension);
			ContentResourceEdit attachment = chs.addAttachmentResource(fName);
			attachment.setContent(content);
			attachment.setContentType(contentType);
			attachment.getPropertiesEdit().addAll(properties);
			chs.commitResource(attachment);

			UploadGradeWrapper r = (UploadGradeWrapper) submissionTable.get(userEid);
			FeedbackAttachment fa = new FeedbackAttachment();
			fa.setAttachmentReference(attachment.getReference());
			r.feedbackAttachments.add(fa);
			submissionTable.put(userEid, r);
		}
	}

	/**
	 * the UploadGradeWrapper class to be used for the "upload all" feature
	 */
	public static class UploadGradeWrapper
	{
		private String grade = null;
		private String comment = "";
		private String timeStamp;
		private String feedbackText = "";
		// entityManager.newReferenceList();
		private Set<FeedbackAttachment> feedbackAttachments = new HashSet<FeedbackAttachment>();

		public UploadGradeWrapper()
		{
		}

		public UploadGradeWrapper(String grade, String comment,
				Set<FeedbackAttachment> feedbackAttachments, String timeStamp, String feedbackText)
		{
			this.grade = grade;
			this.comment = comment;
			if (feedbackAttachments != null)
				this.feedbackAttachments = feedbackAttachments;
			this.feedbackText = feedbackText;
			this.timeStamp = timeStamp;
		}
	}
}