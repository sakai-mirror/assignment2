package org.sakaiproject.assignment2.logic.impl;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
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
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
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
	
	private AssignmentPermissionLogic permissionLogic;
	public void setAssignmentPermissionLogic(AssignmentPermissionLogic permissionLogic) {
		this.permissionLogic = permissionLogic;
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

		StringBuilder exceptionMessage = new StringBuilder();
		if (gradebookLogic.isCurrentUserAbleToGrade(assignment.getContextId()))
		{
			List<AssignmentSubmission> submissions = assignmentSubmissionLogic
				.getViewableSubmissionsWithHistoryForAssignmentId(assignment.getId());
			
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
			List<AssignmentSubmission> submissionsWithHistory, OutputStream outputStream,
			StringBuilder exceptionMessage)
	{
		String assignmentTitle = assignment.getTitle();
		String contextId = externalLogic.getCurrentContextId();
		List<String> viewableStudents = permissionLogic.getViewableStudentsForUserForItem(assignment);
		Map<String, User> userIdUserMap = externalLogic.getUserIdUserMap(viewableStudents);
		
		String formatWithTime = bundle.getString("assignment2.assignment_grade_assignment.downloadall.filename_date_format_with_time");
		String formatNoTime = bundle.getString("assignment2.assignment_grade_assignment.downloadall.filename_date_format");
		DateFormat df_withTime = new SimpleDateFormat(formatWithTime, bundle.getLocale());
		DateFormat df_noTime = new SimpleDateFormat(formatNoTime, bundle.getLocale());
		
		try
		{
			ZipOutputStream out = new ZipOutputStream(outputStream);

			// create the folder structure - named after the assignment's title
			String root = Validator.escapeZipEntry(assignmentTitle) + Entity.SEPARATOR;

			if (submissionsWithHistory != null && !submissionsWithHistory.isEmpty())
			{
				// Create the ZIP file
				String submittersName = "";
				for (AssignmentSubmission s : submissionsWithHistory)
				{
					User submitterUser = userIdUserMap.get(s.getUserId());
					if (submitterUser != null) {
						submittersName = submitterUser.getSortName();

						Set<AssignmentSubmissionVersion> versionHistory = s.getSubmissionHistorySet();

						if (versionHistory != null && !versionHistory.isEmpty()) {

							for (AssignmentSubmissionVersion version : versionHistory) {
								// only include submitted versions
								if (version.getSubmittedDate() != null) {
									// we will create a folder for each submitted version for
									// this student
									String versionFolder = root + "/" + submittersName + "/" 
									+ df_withTime.format(version.getSubmittedDate())
									+ "/";
									ZipEntry versionFolderEntry = new ZipEntry(
											versionFolder);
									out.putNextEntry(versionFolderEntry);
									out.flush();
									out.closeEntry();

									// inside this folder, we will put the submission info
									if (version.getSubmittedText() != null && version.getSubmittedText().trim().length() > 0)
									{
										// create the text file only when a text
										// exists
										ZipEntry textEntry = new ZipEntry(versionFolder
												+ bundle.getString("assignment2.assignment_grade_assignment.downloadall.filename_submitted_text")
												+ ".txt");
										out.putNextEntry(textEntry);
										byte[] text = version.getSubmittedText().getBytes();
										out.write(text);
										textEntry.setSize(text.length);
										out.closeEntry();
									}

									// add the submission attachments
									if (version.getSubmissionAttachSet() != null && !version.getSubmissionAttachSet().isEmpty()) {
										zipAttachments(out, root + submittersName,
												versionFolder, version.getSubmissionAttachSet());
									}
								}
							}
						}
					}
				}
			}
			
			if (assignment.isGraded() && assignment.getGradableObjectId() != null) {
				// the buffer used to store grade information
				StringBuilder gradesBuilder = new StringBuilder();
				gradesBuilder.append(
						bundle.getFormattedMessage("assignment2.assignment_grade-assignment.downloadall.header",
								new Object[] {assignmentTitle}))
								.append("\n");

				// now iterate through all viewable students in this class to create the grades file
				
				if (viewableStudents != null && !viewableStudents.isEmpty()) {
					// get the grade information
					Map<String, GradeInformation> userIdGradeMap = gradebookLogic.getGradeInformationForStudents(contextId, viewableStudents, assignment);
					
					for (String studentId : viewableStudents) {
						// get their User info
						User student = userIdUserMap.get(studentId);
						if (student != null) {
							gradesBuilder.append("\"");
							gradesBuilder.append(student.getDisplayId());
							gradesBuilder.append("\"");
							gradesBuilder.append(",");
							gradesBuilder.append("\"");
							gradesBuilder.append(student.getSortName());
							gradesBuilder.append("\"");
							gradesBuilder.append(",");
							
							// now check for grade information
							GradeInformation gradeInfo = userIdGradeMap.get(studentId);
							if (gradeInfo != null) {
								String gradebookGrade = "";
								String gradebookComment = "";
								
								if (gradeInfo.getGradebookGrade() != null) {
									gradebookGrade = gradeInfo.getGradebookGrade();
								}
								if (gradeInfo.getGradebookComment() != null) {
									gradebookComment = gradeInfo.getGradebookComment();
								}
								gradesBuilder.append("\"");
								gradesBuilder.append(gradebookGrade);
								gradesBuilder.append("\"");
								gradesBuilder.append(",");
								gradesBuilder.append("\"");
								gradesBuilder.append(gradebookComment);
								gradesBuilder.append("\"");
								gradesBuilder.append(",");
							}
							
							gradesBuilder.append("\n");
						}
					}
			
					// create a grades.csv file into zip
					ZipEntry gradesCSVEntry = new ZipEntry(root + assignmentTitle + "-" + contextId + "_" + df_noTime.format(new Date()) + ".csv");
					out.putNextEntry(gradesCSVEntry);
					byte[] grades = gradesBuilder.toString().getBytes();
					out.write(grades);
					gradesCSVEntry.setSize(grades.length);
					out.closeEntry();
				}
			}

			// Complete the ZIP file
			out.finish();
			out.flush();
			out.close();
		}
		catch (IOException e)
		{
			exceptionMessage.append("Cannot establish the IO to create zip file. ");
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
