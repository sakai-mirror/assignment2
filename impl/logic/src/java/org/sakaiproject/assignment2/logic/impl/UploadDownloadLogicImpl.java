package org.sakaiproject.assignment2.logic.impl;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
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
import org.sakaiproject.assignment2.logic.UploadDownloadLogic;
import org.sakaiproject.assignment2.logic.UploadException;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.FeedbackAttachment;
import org.sakaiproject.assignment2.model.FeedbackVersion;
import org.sakaiproject.assignment2.model.UploadAllOptions;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
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
import org.sakaiproject.time.api.TimeService;
import org.sakaiproject.util.StringUtil;

/**
 * Functionality for uploading and downloading an archive bundle
 * 
 * @author <a href="mailto:carl.hall@et.gatech.edu">Carl Hall</a>
 */
public class UploadDownloadLogicImpl implements UploadDownloadLogic
{
	private static final Log log = LogFactory.getLog(UploadDownloadLogicImpl.class);

	private AssignmentLogic assnLogic;
	private AssignmentSubmissionLogic assnSubLogic;
	private TimeService ts;
	private ContentHostingService chs;
	private ContentTypeImageService ctis;
	private ExternalGradebookLogic gradebookLogic;
	private AssignmentBundleLogic bundle;
	private ExternalLogic externalLogic;

	public void setAssnLogic(AssignmentLogic assnLogic)
	{
		this.assnLogic = assnLogic;
	}

	public void setTS(TimeService ts)
	{
		this.ts = ts;
	}

	public void setCHS(ContentHostingService chs)
	{
		this.chs = chs;
	}

	public void setCTIS(ContentTypeImageService ctis)
	{
		this.ctis = ctis;
	}

	public void setGradebookLogic(ExternalGradebookLogic gradebookLogic)
	{
		this.gradebookLogic = gradebookLogic;
	}

	public void setAssnSubLogic(AssignmentSubmissionLogic assnSubLogic)
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
	 * @see org.sakaiproject.assignment2.logic.UploadDownloadLogic#uploadAll(java.lang.Long,
	 *      org.sakaiproject.assignment2.model.UploadAllOptions, java.util.zip.ZipFile,
	 *      java.lang.String, java.lang.String)
	 */
	public void uploadAll(Long assignmentId, UploadAllOptions options, ZipFile zipFile) throws UploadException
	{
		ArrayList<String> alerts = new ArrayList<String>();

		if (!options.isFeedbackText() && !options.isGradeFile() && !options.isComments()
				&& !options.isFeedbackAttachments())
		{
			String key = "uploadall.alert.choose.element";
			String msg = bundle.getString(key);
			log.warn(msg);
			throw new UploadException(key, msg);
		}

		// constructor the hashtable for all submission objects
		Hashtable<String, UploadGradeWrapper> submisTable = new Hashtable<String, UploadGradeWrapper>();
		Assignment2 assn = assnLogic.getAssignmentById(assignmentId);

		if (zipFile == null)
		{
			String key = "uploadall.alert.zipFile";
			String msg = bundle.getString(key);
			log.error(msg);
			throw new UploadException(key, msg);
		}

		Enumeration<? extends ZipEntry> entries = zipFile.entries();
		String feedbackAttachmentFolder = bundle.getString("download.feedback.attachment");
		while (entries.hasMoreElements())
		{
			ZipEntry entry = entries.nextElement();
			String entryName = entry.getName();
			if (!entry.isDirectory())
			{
				try
				{
					InputStream is = zipFile.getInputStream(entry);
					if (entryName.endsWith("grades.csv") && options.isGradeFile())
						processGrades(alerts, submisTable, assn, StringUtil
								.trimToZero(readIntoString(is)));
					else
						processEntry(options, feedbackAttachmentFolder, submisTable, is, entryName);
				}
				catch (Exception e)
				{
					// catch everything from these calls and wrap with generic UploadException
					throw new UploadException(e.getMessage(), e);
				}
			}
		}

		String userId = externalLogic.getCurrentUserId();
		for (String userEid : submisTable.keySet())
		{
			UploadGradeWrapper w = (UploadGradeWrapper) submisTable.get(userEid);
			// save the feedback changes
			FeedbackVersion feedback = assnSubLogic.getFeedbackByUserIdAndSubmittedTime(userEid,
					new Date(ts.newTimeGmt(w.timeStamp).getTime()));
			if (options.isComments())
				feedback.setFeedbackNotes(w.comment);
			if (options.isFeedbackText())
				feedback.setAnnotatedText(w.feedbackText);
			if (options.isFeedbackAttachments())
				feedback.setFeedbackAttachSet(w.feedbackAttachments);
			feedback.setLastFeedbackSubmittedBy(userId);
			assnSubLogic.updateFeedbackForVersion(feedback);

			// save the grade
//			gradebookLogic.updateGrade(w.grade);
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

		if (options.isComments() && entryName.contains("comments"))
		{
			// read the comments file
			String comment = getBodyTextFromZipHtml(zin);
			if (comment != null)
				r.comment = comment;
		}
		if (options.isFeedbackText() && entryName.contains("feedbackText"))
		{
			// upload the feedback text
			String text = getBodyTextFromZipHtml(zin);
			if (text != null)
				r.feedbackText = text;
		}
		if (options.isFeedbackAttachments())
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

	private void processGrades(ArrayList<String> alerts,
			Hashtable<String, UploadGradeWrapper> submisTable, Assignment2 assn, String result)
			throws UploadException
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
				if (w != null)
					w = new UploadGradeWrapper();
				String itemString = items[GRADE];
				int gradeType = Integer.parseInt(gradebookLogic.getGradeType(assn
						.getGradableObjectId().toString()));
				if (gradeType == AssignmentConstants.GRADE_TYPE_SCORE)
					validPointGrade(itemString, alerts);
				else
					validLetterGrade(itemString, alerts);
				// check that no error messages were
				// generated
				if (alerts.size() == 0)
				{
					String grade = (gradeType == AssignmentConstants.GRADE_TYPE_SCORE) ? scalePointGrade(
							itemString, alerts)
							: itemString;
					w.grade = grade;
					submisTable.put(displayId, w);
				}
				else
				{
					throw new UploadException(alerts);
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
	 * valid grade for point based type
	 */
	private void validLetterGrade(String grade, List<String> alerts)
	{
		final String VALID_CHARS_FOR_LETTER_GRADE = " ABCDEFGHIJKLMNOPQRSTUVWXYZ+-";
		boolean invalid = false;
		if (grade != null)
		{
			grade = grade.toUpperCase();
			for (int i = 0; i < grade.length() && !invalid; i++)
			{
				char c = grade.charAt(i);
				if (VALID_CHARS_FOR_LETTER_GRADE.indexOf(c) == -1)
				{
					invalid = true;
					break;
				}
			}
			if (invalid)
			{
				alerts.add(bundle.getString("plesuse0"));
			}
		}
	}

	/**
	 * valid grade for point based type
	 */
	private void validPointGrade(String grade, List<String> alerts)
	{
		if (grade != null && grade.length() > 0)
		{
			if (grade.startsWith("-"))
			{
				// check for negative sign
				alerts.add(bundle.getString("plesuse3"));
			}
			else
			{
				int index = grade.indexOf(".");
				if (index != -1)
				{
					// when there is decimal points inside the grade, scale the number by 10
					// but only one decimal place is supported
					// for example, change 100.0 to 1000
					if (!grade.equals("."))
					{
						if (grade.length() > index + 2)
						{
							// if there are more than one decimal point
							alerts.add(bundle.getString("plesuse2"));
						}
						else
						{
							// decimal points is the only allowed character inside grade
							// replace it with '1', and try to parse the new String into int
							String gradeString = (grade.endsWith(".")) ? grade.substring(0, index)
									.concat("0") : grade.substring(0, index).concat(
									grade.substring(index + 1));
							try
							{
								Integer.parseInt(gradeString);
							}
							catch (NumberFormatException e)
							{
								alertInvalidPoint(gradeString, alerts);
							}
						}
					}
					else
					{
						// grade is "."
						alerts.add(bundle.getString("plesuse1"));
					}
				}
				else
				{
					// There is no decimal point; should be int number
					String gradeString = grade + "0";
					try
					{
						Integer.parseInt(gradeString);
					}
					catch (NumberFormatException e)
					{
						alertInvalidPoint(gradeString, alerts);
					}
				}
			}
		}
	} // validPointGrade

	private void alertInvalidPoint(String grade, List<String> alerts)
	{
		String VALID_CHARS_FOR_INT = "-01234567890";

		boolean invalid = false;
		// case 1: contains invalid char for int
		for (int i = 0; i < grade.length() && !invalid; i++)
		{
			char c = grade.charAt(i);
			if (VALID_CHARS_FOR_INT.indexOf(c) == -1)
			{
				invalid = true;
			}
		}
		if (invalid)
		{
			alerts.add(bundle.getString("plesuse1"));
		}
		else
		{
			int maxInt = Integer.MAX_VALUE / 10;
			int maxDec = Integer.MAX_VALUE - maxInt * 10;
			// case 2: Due to our internal scaling, input String is larger than Integer.MAX_VALUE/10
			alerts.add(grade.substring(0, grade.length() - 1) + "."
					+ grade.substring(grade.length() - 1) + " " + bundle.getString("plesuse4")
					+ maxInt + "." + maxDec + ".");
		}
	}

	/**
	 * scale the point value by 10 if there is a valid point grade
	 */
	private String scalePointGrade(String point, List<String> alerts)
	{
		validPointGrade(point, alerts);
		if (alerts.size() == 0)
		{
			if (point != null && (point.length() >= 1))
			{
				// when there is decimal points inside the grade, scale the number by 10
				// but only one decimal place is supported
				// for example, change 100.0 to 1000
				int index = point.indexOf(".");
				if (index != -1)
				{
					if (index == 0)
					{
						// if the point is the first char, add a 0 for the integer part
						point = "0".concat(point.substring(1));
					}
					else if (index < point.length() - 1)
					{
						// use scale integer for gradePoint
						point = point.substring(0, index) + point.substring(index + 1);
					}
					else
					{
						// decimal point is the last char
						point = point.substring(0, index) + "0";
					}
				}
				else
				{
					// if there is no decimal place, scale up the integer by 10
					point = point + "0";
				}

				// filter out the "zero grade"
				if (point.equals("00"))
				{
					point = "0";
				}
			}
		}
		return point;

	} // scalePointGrade

	/**
	 * the UploadGradeWrapper class to be used for the "upload all" feature
	 */
	public class UploadGradeWrapper
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