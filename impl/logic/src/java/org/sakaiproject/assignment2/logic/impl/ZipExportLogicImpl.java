package org.sakaiproject.assignment2.logic.impl;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.AssignmentBundleLogic;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.GradeInformation;
import org.sakaiproject.assignment2.logic.ZipExportLogic;
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
import org.sakaiproject.user.api.User;
import org.sakaiproject.util.FormattedText;
import org.sakaiproject.util.StringUtil;
import org.sakaiproject.util.Validator;

public class ZipExportLogicImpl implements ZipExportLogic
{
	private static Log log = LogFactory.getLog(ZipExportLogicImpl.class);

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

	private AssignmentBundleLogic bundle;

	public void setAssignmentBundleLogic(AssignmentBundleLogic bundle)
	{
		this.bundle = bundle;
	}

	/* (non-Javadoc)
	 * @see org.sakaiproject.assignment2.tool.handlerhooks.ZipExporterI#getSubmissionsZip(java.io.OutputStream, java.lang.Long)
	 */
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
			zipSubmissions(assignment, submissions, outputStream, exceptionMessage);

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
			List<AssignmentSubmission> submissions, OutputStream outputStream,
			StringBuilder exceptionMessage)
	{
		String assignmentTitle = assignment.getTitle();
		// String assignmentId = assignment.getAssignmentId().toString();
		String gradeTypeString = bundle
				.getString("assignment2.assignment_grade-assignment.downloadall.type."
						+ gradebookLogic.getGradeType(assignment.getContextId()));
		
		Map<String, GradeInformation> studentIdToGradeMap = new HashMap<String, GradeInformation>();
		// retrieve all of the grade information for these submissions if
		// this is a graded assignment
		if (submissions != null && !assignment.isUngraded() && assignment.getGradableObjectId() != null) {
			//first, we need a list of the student ids
			List<String> studentIds = new ArrayList<String>();
			for (AssignmentSubmission submission : submissions) {
				studentIds.add(submission.getUserId());
			}
			
			// now retrieve the grades
			studentIdToGradeMap = gradebookLogic.getGradeInformationForStudents(externalLogic.getCurrentContextId(),
					studentIds, assignment);
		}
		
		try
		{
			ZipOutputStream out = new ZipOutputStream(outputStream);

			// create the folder structure - named after the assignment's title
			String root = Validator.escapeZipEntry(assignmentTitle) + Entity.SEPARATOR;

			String submittedText = "";
			if (submissions.isEmpty())
			{
				exceptionMessage.append("There is no submission yet. ");
			}

			// the buffer used to store grade information
			StringBuilder gradesBuilder = new StringBuilder(assignmentTitle + ","
					+ gradeTypeString + "\n\n");
			gradesBuilder.append(
					bundle.getString("assignment2.assignment_grade-assignment.downloadall.header"))
					.append("\n");

			// Create the ZIP file
			String submittersName = "";
			int count = 1;
			for (AssignmentSubmission s : submissions)
			{
				submittersName = "";
				String userId = (String) s.getUserId();
				AssignmentSubmissionVersion sv = assignmentSubmissionLogic
						.getCurrentSubmissionByAssignmentIdAndStudentId(
								assignment.getId(), userId).getCurrentSubmissionVersion();

				if (sv != null && sv.getSubmittedTime() != null)
				{
					User user = externalLogic.getUser(userId);
					String name = externalLogic.getUserDisplayName(userId);
					String displayId = user.getDisplayId();
					String fullName = externalLogic.getUserFullName(userId);
					String submittersString = name + "(" + displayId + ")";
					
					String grade = "";
					String gradeComment = "";
					if (!assignment.isUngraded() && assignment.getGradableObjectId() != null) {
						GradeInformation gradeInfo = studentIdToGradeMap.get(userId);
						if (gradeInfo != null) {
							grade = gradeInfo.getGradebookGrade();
							gradeComment = gradeInfo.getGradebookComment();
						}
					}
					gradesBuilder.append(userId).append(",").append(name).append(",").append(displayId).append(",")
							.append(fullName).append(",").append(grade).append(",").append(gradeComment)
							.append("\n");

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
									ZipEntry textEntry = new ZipEntry(root
											+ submittersName + submittersString
											+ "_submissionText.txt");
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
								byte[] b = (sv.getSubmittedTime().toString()+"\n"+userId).getBytes();
								out.write(b);
								textEntry.setSize(b.length);
								out.closeEntry();

								/* Comments go in the csv now, and this was reading the wrong thing anyway
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
								*/
								
								// the feedback.txt file
								ZipEntry fbtextEntry = new ZipEntry(root + submittersName + "feedback.txt");
								out.putNextEntry(fbtextEntry);
								byte[] fbb = FormattedText.encodeUnicode(sv.getAnnotatedText()).getBytes();
								out.write(fbb);
								fbtextEntry.setSize(fbb.length);
								out.closeEntry();
								
								// create an attachment folder for the feedback
								// attachments
								String feedbackSubAttachmentFolder = root
										+ submittersName
										+ bundle
												.getString("assignment2.assignment_grade-assignment.downloadall.feedbackdir")
										+ "/";
								ZipEntry feedbackSubAttachmentFolderEntry = new ZipEntry(
										feedbackSubAttachmentFolder);
								out.putNextEntry(feedbackSubAttachmentFolderEntry);
								out.flush();
								out.closeEntry();

								// create a attachment folder for the submission
								// attachments
								String sSubAttachmentFolder = root
										+ submittersName
										+ bundle
												.getString("assignment2.assignment_grade-assignment.downloadall.submdir")
										+ "/";
								ZipEntry sSubAttachmentFolderEntry = new ZipEntry(
										sSubAttachmentFolder);
								out.putNextEntry(sSubAttachmentFolderEntry);
								out.flush();
								out.closeEntry();
								// add all submission attachment into the
								// submission attachment folder
								zipAttachments(out, root + submittersName,
										sSubAttachmentFolder, sv.getSubmissionAttachSet());
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
			} // for submissions

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
			InputStream content = null;
			BufferedInputStream bContent = null;
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
				content = resource.streamContent();
				byte data[] = new byte[1024 * 10];
				bContent = new BufferedInputStream(content,
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
				log.warn(this
						+ ": getSubmissionsZip--PermissionException submittersName="
						+ submittersName + " attachment reference=" + r);
			}
			catch (IdUnusedException e)
			{
				log.warn(this + ": getSubmissionsZip--IdUnusedException submittersName="
						+ submittersName + " attachment reference=" + r);
			}
			catch (TypeException e)
			{
				log.warn(this + ": getSubmissionsZip--TypeException: submittersName="
						+ submittersName + " attachment reference=" + r);
			}
			catch (IOException e)
			{
				log
						.warn(this
								+ ": getSubmissionsZip--IOException: Problem in creating the attachment file: submittersName="
								+ submittersName + " attachment reference=" + r);
			}
			catch (ServerOverloadException e)
			{
				log.warn(this
						+ ": getSubmissionsZip--ServerOverloadException: submittersName="
						+ submittersName + " attachment reference=" + r);
			}
			finally
			{
				if (content != null)
				{
					try
					{
						content.close();
					}
					catch (IOException e)
					{
						log.warn("IOException when closing content stream", e);
					}
				}
				if (bContent != null)
				{
					try
					{
						bContent.close();
					}
					catch (IOException e)
					{
						log.warn("IOException when closing bContent stream", e);
					}
				}
			}
		} // for
	}
}
