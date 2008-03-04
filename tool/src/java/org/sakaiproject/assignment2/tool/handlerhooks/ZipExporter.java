package org.sakaiproject.assignment2.tool.handlerhooks;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.AttachmentBase;
import org.sakaiproject.content.api.ContentHostingService;
import org.sakaiproject.content.api.ContentResource;
import org.sakaiproject.entity.api.Entity;
import org.sakaiproject.entity.api.ResourceProperties;
import org.sakaiproject.exception.IdUnusedException;
import org.sakaiproject.exception.PermissionException;
import org.sakaiproject.exception.ServerOverloadException;
import org.sakaiproject.exception.TypeException;
import org.sakaiproject.util.FormattedText;
import org.sakaiproject.util.StringUtil;
import org.sakaiproject.util.Validator;

import uk.org.ponder.messageutil.MessageLocator;

public class ZipExporter
{
	private static Log log = LogFactory.getLog(ZipExporter.class);

	private AssignmentLogic assignmentLogic;

	public void setAssignmentLogic(AssignmentLogic assignmentLogic)
	{
		this.assignmentLogic = assignmentLogic;
	}

	private AssignmentSubmissionLogic assignmentSubmissionLogic;

	public void setAssignmentSubmissionLogic(
			AssignmentSubmissionLogic assignmentSubmissionLogic)
	{
		this.assignmentSubmissionLogic = assignmentSubmissionLogic;
	}

	private ExternalGradebookLogic gradebookLogic;

	public void setGradebookLogic(ExternalGradebookLogic gradebookLogic)
	{
		this.gradebookLogic = gradebookLogic;
	}

	private ExternalLogic externalLogic;

	public void setExternalLogic(ExternalLogic externalLogic)
	{
		this.externalLogic = externalLogic;
	}

	private ContentHostingService contentHostingService;

	public void setContentHostingService(ContentHostingService contentHostingService)
	{
		this.contentHostingService = contentHostingService;
	}

	private MessageLocator messageLocator;

	public void setMessageLocator(MessageLocator messageLocator)
	{
		this.messageLocator = messageLocator;
	}

	public void getSubmissionsZip(OutputStream outputStream, Long assignmentId)
			throws PermissionException
	{
		Assignment2 assignment = assignmentLogic.getAssignmentById(assignmentId);
		if (log.isDebugEnabled())
			log.debug(this + ": getSubmissionsZip reference=" + assignmentId);

		List<AssignmentSubmission> submissions = assignmentSubmissionLogic
				.getViewableSubmissionsForAssignmentId(assignment.getId());

		StringBuilder exceptionMessage = new StringBuilder();
		if (gradebookLogic.isCurrentUserAbleToGrade(assignment.getContextId()))
		{
			zipSubmissions(assignment, submissions.iterator(), outputStream,
					exceptionMessage);

			if (exceptionMessage.length() > 0)
			{
				// log any error messages
				if (log.isDebugEnabled())
					log.debug(this + assignmentId.toString()
							+ exceptionMessage.toString());
			}
		}
	} // getSubmissionsZip

	protected void zipSubmissions(Assignment2 assignment,
			Iterator<AssignmentSubmission> submissions, OutputStream outputStream,
			StringBuilder exceptionMessage)
	{
		String assignmentTitle = assignment.getTitle();
		// String assignmentId = assignment.getAssignmentId().toString();
		String gradeTypeString = messageLocator.getMessage("assignment2.assignment_grade-assignment.downloadall.type." +
				gradebookLogic.getGradeType(assignment.getContextId()));
		try
		{
			ZipOutputStream out = new ZipOutputStream(outputStream);

			// create the folder structure - named after the assignment's title
			String root = Validator.escapeZipEntry(assignmentTitle) + Entity.SEPARATOR;

			String submittedText = "";
			if (!submissions.hasNext())
			{
				exceptionMessage.append("There is no submission yet. ");
			}

			// the buffer used to store grade information
			StringBuilder gradesBuilder = new StringBuilder(assignmentTitle + ","
					+ gradeTypeString + "\n\n");
			gradesBuilder
					.append(
							messageLocator
									.getMessage("assignment2.assignment_grade-assignment.downloadall.id"))
					.append(",")
					.append(
							messageLocator
									.getMessage("assignment2.assignment_grade-assignment.downloadall.eid"))
					.append(",")
					.append(
							messageLocator
									.getMessage("assignment2.assignment_grade-assignment.downloadall.lastname"))
					.append(",")
					.append(
							messageLocator
									.getMessage("assignment2.assignment_grade-assignment.downloadall.firstname"))
					.append(",")
					.append(
							messageLocator
									.getMessage("assignment2.assignment_grade-assignment.downloadall.grade"))
					.append("\n");

			// Create the ZIP file
			String submittersName = "";
			int count = 1;
			while (submissions.hasNext())
			{
				AssignmentSubmission s = (AssignmentSubmission) submissions.next();
				String userId = (String) s.getUserId();
				AssignmentSubmissionVersion sv = assignmentSubmissionLogic
						.getCurrentSubmissionByAssignmentIdAndStudentId(
								assignment.getId(), userId).getCurrentSubmissionVersion();

				if (sv.getSubmittedTime() != null)
				{
					String name = externalLogic.getUserDisplayName(userId);
					String fullName = externalLogic.getUserFullName(userId);
					String submittersString = name + "(" + userId + ")";
					gradesBuilder.append(name).append(",").append(userId).append(",").append(
							fullName).append(",").append(s.getGradebookGrade()).append("\n");

					if (StringUtil.trimToNull(submittersString) != null)
					{
						submittersName = submittersName.concat(StringUtil
								.trimToNull(submittersString));
						submittedText = sv.getSubmittedText();

						boolean added = false;
						while (!added)
						{
							try
							{
								submittersName = submittersName.concat("/");
								// create the folder structure - named after the
								// submitter's name
								if (submittedText != null && submittedText != "")
								{
									// create the text file only when a text
									// submission is allowed
									ZipEntry textEntry = new ZipEntry(root + submittersName
											+ submittersString + "_submissionText.txt");
									out.putNextEntry(textEntry);
									byte[] text = submittedText.getBytes();
									out.write(text);
									textEntry.setSize(text.length);
									out.closeEntry();
								}

								// Write the timestamp for the submission
								ZipEntry textEntry = new ZipEntry(root + submittersName
										+ "timestamp.txt");
								out.putNextEntry(textEntry);
								byte[] b = (sv.getSubmittedTime().toString()).getBytes();
								out.write(b);
								textEntry.setSize(b.length);
								out.closeEntry();

								// the comments.txt file to show instructor's
								// comments
								ZipEntry ctextEntry = new ZipEntry(root + submittersName
										+ "comments.txt");
								out.putNextEntry(ctextEntry);
								byte[] cb = FormattedText.encodeUnicode(
										sv.getFeedbackNotes()).getBytes();
								out.write(cb);
								ctextEntry.setSize(cb.length);
								out.closeEntry();

								// create an attachment folder for the feedback
								// attachments
								String feedbackSubAttachmentFolder = root + submittersName
										+ messageLocator
												.getMessage("assignment2.assignment_grade-assignment.downloadall.feedbackdir")
										+ "/";
								ZipEntry feedbackSubAttachmentFolderEntry = new ZipEntry(
										feedbackSubAttachmentFolder);
								out.putNextEntry(feedbackSubAttachmentFolderEntry);
								out.closeEntry();

								// create a attachment folder for the submission
								// attachments
								String sSubAttachmentFolder = root + submittersName
										+ messageLocator
												.getMessage("assignment2.assignment_grade-assignment.downloadall.submdir")
										+ "/";
								ZipEntry sSubAttachmentFolderEntry = new ZipEntry(
										sSubAttachmentFolder);
								out.putNextEntry(sSubAttachmentFolderEntry);
								out.closeEntry();
								// add all submission attachment into the
								// submission attachment folder
								zipAttachments(out, root + submittersName, sSubAttachmentFolder,
										sv.getSubmissionAttachSet());
								// add all feedback attachment folder
								zipAttachments(out, root + submittersName,
										feedbackSubAttachmentFolder, sv
												.getFeedbackAttachSet());

								added = true;
							}
							catch (IOException e)
							{
								exceptionMessage
										.append("Can not establish the IO to create zip file for user "
												+ root + submittersName);
								log
										.debug(this
												+ ": getSubmissionsZip--IOException unable to create the zip file for user"
												+ root + submittersName);
								submittersName = submittersName.substring(0,
										submittersName.length() - 1)
										+ "_" + count++;
							}
						} // while
					} // if
				} // if
			} // while -- there is submission

			// create a grades.csv file into zip
			ZipEntry gradesCSVEntry = new ZipEntry(root + "grades.csv");
			out.putNextEntry(gradesCSVEntry);
			byte[] grades = gradesBuilder.toString().getBytes();
			out.write(grades);
			gradesCSVEntry.setSize(grades.length);
			out.closeEntry();

			// Complete the ZIP file
			out.finish();
			out.flush();
			out.close();
		}
		catch (IOException e)
		{
			exceptionMessage.append("Can not establish the IO to create zip file. ");
			log
					.debug(this
							+ ": getSubmissionsZip--IOException unable to create the zip file for assignment "
							+ assignmentTitle);
		}
	}

	private void zipAttachments(ZipOutputStream out, String submittersName,
			String sSubAttachmentFolder, Set<? extends AttachmentBase> attachments)
	{
		int attachedUrlCount = 0;
		for (AttachmentBase r : attachments)
		{

			try
			{
				ContentResource resource = contentHostingService.getResource(r
						.getAttachmentReference());

				String contentType = resource.getContentType();

				ResourceProperties props = resource.getProperties();
				String displayName = props.getPropertyFormatted(props
						.getNamePropDisplayName());

				// for URL content type, encode a redirect to the body URL
				if (contentType.equalsIgnoreCase(ResourceProperties.TYPE_URL))
				{
					displayName = "attached_URL_" + attachedUrlCount;
					attachedUrlCount++;
				}

				// buffered stream input
				InputStream content = resource.streamContent();
				byte data[] = new byte[1024 * 10];
				BufferedInputStream bContent = new BufferedInputStream(content,
						data.length);

				ZipEntry attachmentEntry = new ZipEntry(sSubAttachmentFolder
						+ displayName);
				out.putNextEntry(attachmentEntry);
				int bCount = -1;
				while ((bCount = bContent.read(data, 0, data.length)) != -1)
				{
					out.write(data, 0, bCount);
				}
				out.closeEntry();
				content.close();
			}
			catch (PermissionException e)
			{
				log.debug(this
						+ ": getSubmissionsZip--PermissionException submittersName="
						+ submittersName + " attachment reference=" + r);
			}
			catch (IdUnusedException e)
			{
				log.debug(this + ": getSubmissionsZip--IdUnusedException submittersName="
						+ submittersName + " attachment reference=" + r);
			}
			catch (TypeException e)
			{
				log.debug(this + ": getSubmissionsZip--TypeException: submittersName="
						+ submittersName + " attachment reference=" + r);
			}
			catch (IOException e)
			{
				log
						.debug(this
								+ ": getSubmissionsZip--IOException: Problem in creating the attachment file: submittersName="
								+ submittersName + " attachment reference=" + r);
			}
			catch (ServerOverloadException e)
			{
				log.debug(this
						+ ": getSubmissionsZip--ServerOverloadException: submittersName="
						+ submittersName + " attachment reference=" + r);
			}
		} // for
	}
}
