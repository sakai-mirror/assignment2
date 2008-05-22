package org.sakaiproject.assignment2.logic.impl;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.vfs.FileContent;
import org.apache.commons.vfs.FileDepthSelector;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileSystemManager;
import org.apache.commons.vfs.FileType;
import org.apache.commons.vfs.VFS;
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
	public void uploadAll(UploadAllOptions options, File file) throws UploadException
	{
		if (options == null)
			throw new IllegalArgumentException("options cannot be null.");
		
		
		
		// construct the hashtable for all submission objects
		Hashtable<String, UploadGradeWrapper> submisTable = new Hashtable<String, UploadGradeWrapper>();
		Assignment2 assn = assnLogic.getAssignmentById(options.assignmentId);

		if (file == null)
			throw new IllegalArgumentException("file cannot be null.");

		try
		{
			FileSystemManager fsManager = VFS.getManager();
			FileObject zipFile = fsManager.toFileObject(file);
			zipFile = fsManager.createFileSystem("zip", zipFile);
			
			zipFile = zipFile.resolveFile(assn.getTitle());
			if (options.gradeFile)
			{
				processGrades(submisTable, assn, zipFile.resolveFile("grades.csv").getContent());
			}
			for (FileObject dir : zipFile.findFiles(new FileDepthSelector(1,1)))
			{
				if (dir.getType().equals(FileType.FOLDER))
				{
					processFolder(dir, submisTable, options);
				}
			}
			saveGrades(options, submisTable, true);
		} catch (FileSystemException e) {
			throw new UploadException(e.getMessage(), e);
		}
	}

	private void saveGrades(UploadAllOptions options,
			Hashtable<String, UploadGradeWrapper> submisTable, boolean isZip)
			throws UploadException
	{
		String userId = externalLogic.getCurrentUserId();
		String format = "yyyy-MM-dd HH:mm:ss.S";
		SimpleDateFormat dateFormat = new SimpleDateFormat(format);
		Assignment2 assn = assnLogic.getAssignmentById(options.assignmentId);
		for (Map.Entry<String, UploadGradeWrapper> entry : submisTable.entrySet())
		{
			String userEid = entry.getKey();
			UploadGradeWrapper w = entry.getValue();
			try
			{
				// save the feedback changes
				// date format is derived from output in ZipExporter which uses Date.toString()
				if (isZip && (options.feedbackText || options.feedbackAttachments))
				{
					FeedbackVersion feedback = assnSubLogic.getFeedbackByUserIdAndSubmittedTime(
							userEid, dateFormat.parse(w.timeStamp));
					feedback.getFeedbackAttachSet();

					if (options.feedbackText)
						feedback.setAnnotatedText(w.feedbackText);
					if (options.feedbackAttachments)
						feedback.setFeedbackAttachSet(w.feedbackAttachments);
					feedback.setLastFeedbackSubmittedBy(userId);
					assnSubLogic.updateFeedbackForVersion(feedback);
				}
				// save the grade
				if (options.gradeFile || ! isZip)
				{
					gradebookLogic.saveGradeAndCommentForStudent(assn.getContextId(), assn
						.getGradableObjectId(), userEid, w.grade, w.comment);
				}
			}
			catch (ParseException pe)
			{
				String msg = "Unable to parse date [" + w.timeStamp + "]; expected " + format;
				log.warn(msg, pe);
				throw new UploadException(msg, pe);
			}
		}
	}

	private void processFolder(FileObject folder, Hashtable<String, UploadGradeWrapper> submisTable, UploadAllOptions options) throws UploadException
	{
		String feedbackAttachmentFolder = bundle
			.getString("assignment2.download.feedback.attachment");
		try
		{
			//parse the timestamp file
			String timestamp = readIntoString(folder.resolveFile("timestamp.txt").getContent());
			String userEid = splitLine(timestamp)[1];
			
			//now we're ready to parse the rest of the folder
			UploadGradeWrapper r = submisTable.get(userEid);
			if (r == null)
				r = new UploadGradeWrapper();
			if (options.feedbackText)
			{
				r.feedbackText = getBodyText(folder.resolveFile("feedback.txt").getContent());
			}
			if (options.feedbackAttachments)
			{
				FileObject feedbackFolder = folder.resolveFile(feedbackAttachmentFolder);
				uploadZipAttachments(feedbackFolder, submisTable, userEid);
			}
			r.timeStamp = splitLine(timestamp)[0];
			submisTable.put(userEid, r);
		} catch (FileSystemException e) {
			throw new UploadException(e.getMessage(), e);
		} catch (IOException e) {
			throw new UploadException(e.getMessage(), e);
		}
	}

	private void processGrades(Hashtable<String, UploadGradeWrapper> submisTable, Assignment2 assn,
			FileContent fc) throws UploadException
	{
		// read grades.cvs from zip
		try
		{
			String[] lines = splitLine(readIntoString(fc));
			// skip the first 3 lines because they are headers
			// - line 1: assignment title, type of grading
			// - line 2: blank line
			// - line 3: column header
			// -- [display id, user id, last name, first name, grade]
			// define position constants
			final int EID = 0;
			final int DISP_ID = 2;
			final int GRADE = 5;
			final int COMMENT = 6;
			for (int i = 3; i < lines.length; i++)
			{
				String[] items = lines[i].split(",");
				if (items.length > 6)
				{
					// has grade information
					String eid = items[EID];
					if (externalLogic.getUser(eid).getDisplayId().equals(items[DISP_ID]))
					{
						UploadGradeWrapper w = (UploadGradeWrapper) submisTable.get(eid);
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
						w.comment = items[COMMENT];
						submisTable.put(eid, w);
					} else {
						throw new UploadException("DisplayId doesn't belong to this EID, refusing to update grade.");
					}
				}
			}
		} catch (FileSystemException e) {
			throw new UploadException(e.getMessage(), e);
		} catch (IOException e) {
			throw new UploadException(e.getMessage(), e);
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
		if (result == null)
			return null;
		String[] lines = null;
		if (result.contains("\r\n"))
			lines = result.split("\r\n");
		else if (result.contains("\r"))
			lines = result.split("\r");
		else if (result.contains("\n"))
			lines = result.split("\n");
		return lines;
	}

	private String readIntoString(FileContent fc) throws IOException
	{
		InputStream in = fc.getInputStream();
		StringBuilder buffer = new StringBuilder();
		int size = 2048;
		byte[] data = new byte[size];

		while ((size = in.read(data, 0, data.length)) > 0)
			buffer.append(new String(data, 0, size));

		return buffer.toString();
	}

	private String getBodyText(FileContent fc) throws IOException
	{
		String rv = StringUtil.trimToNull(readIntoString(fc));
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
	 * @param submisTable
	 * @param entryName
	 * @param userEid
	 */	
	private void uploadZipAttachments(FileObject feedbackFolder, Hashtable<String, UploadGradeWrapper> submisTable, String userEid)throws UploadException
	{
		try
		{
			for (FileObject file : feedbackFolder.getChildren())
			{
				String fname = file.getName().getBaseName();
				ResourceProperties properties = chs.newResourceProperties();
				properties.addProperty(ResourceProperties.PROP_DISPLAY_NAME, fname);
				String extension = file.getName().getExtension();
				String contentType = ctis.getContentType(extension);
				ContentResourceEdit attachment = chs.addAttachmentResource(fname);
				attachment.setContent(file.getContent().getInputStream());
				attachment.setContentType(contentType);
				attachment.getPropertiesEdit().addAll(properties);
				chs.commitResource(attachment);
				
				UploadGradeWrapper r = (UploadGradeWrapper) submisTable.get(userEid);
				FeedbackAttachment fa = new FeedbackAttachment();
				fa.setAttachmentReference(attachment.getReference());
				r.feedbackAttachments.add(fa);
				submisTable.put(userEid, r);
			}
		} catch (FileSystemException e) {
			throw new UploadException(e.getMessage(), e);
		} catch (PermissionException e) {
			throw new UploadException(e.getMessage(), e);
		} catch (InconsistentException e) {
			throw new UploadException(e.getMessage(), e);
		} catch (ServerOverloadException e) {
			throw new UploadException(e.getMessage(), e);
		} catch (IdInvalidException e) {
			throw new UploadException(e.getMessage(), e);
		} catch (IdUsedException e) {
			throw new UploadException(e.getMessage(), e);
		} catch (OverQuotaException e) {
			throw new UploadException(e.getMessage(), e);
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

	public void uploadCSV(UploadAllOptions options, File file)throws UploadException
	{
		if (options == null)
		{
			throw new IllegalArgumentException("options cannot be null.");
		}

		// construct the hashtable for all submission objects
		Hashtable<String, UploadGradeWrapper> submisTable = new Hashtable<String, UploadGradeWrapper>();
		Assignment2 assn = assnLogic.getAssignmentById(options.assignmentId);

		if (file == null)
		{
			throw new IllegalArgumentException("file cannot be null.");
		}

		try
		{
			FileSystemManager fsManager = VFS.getManager();
			FileObject f = fsManager.toFileObject(file);
			processGrades(submisTable, assn, f.getContent());
		}
		catch (Exception e)
		{
			log.warn(e.getMessage(), e);
			// catch everything from these calls and wrap with generic UploadException
			throw new UploadException(e.getMessage(), e);
		}
		saveGrades(options, submisTable, false);
	}
}