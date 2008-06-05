package org.sakaiproject.assignment2.tool.handlers;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
import org.sakaiproject.content.api.ContentHostingService;
import org.sakaiproject.content.api.ContentResource;
import org.sakaiproject.entity.api.ResourceProperties;
import org.sakaiproject.exception.IdUnusedException;
import org.sakaiproject.exception.PermissionException;
import org.sakaiproject.exception.TypeException;
import org.sakaiproject.util.StringUtil;

public class SubmissionHandler extends Asnn2HandlerBase
{
	private AssignmentSubmissionLogic subLogic = null;
	private ContentHostingService chs = null;
	private ExternalLogic externalLogic = null;
	private AssignmentPermissionLogic permissionLogic = null;

	@Override
	public void postInit(Map<String, String> config) throws ServletException
	{
		subLogic = (AssignmentSubmissionLogic) getService(AssignmentSubmissionLogic.class.getName());
		chs = (ContentHostingService) getService(ContentHostingService.class.getName());
		externalLogic = (ExternalLogic) getService(ExternalLogic.class.getName());
		permissionLogic = (AssignmentPermissionLogic) getService(AssignmentPermissionLogic.class
				.getName());
	}

	@Override
	public void handlePost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{

	}

	@Override
	public void handleGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		// capture the inputs
		String assignmentId = StringUtil.trimToNull(request.getParameter("asnnId"));
		String submissionId = StringUtil.trimToNull(request.getParameter("subId"));

		// convert as needed
		Long asnnId = 0L;
		Long subId = 0L;
		try {asnnId = Long.parseLong(assignmentId);} catch (NumberFormatException nfe) {}
		try {subId = Long.parseLong(submissionId);} catch (NumberFormatException nfe) {}

		try
		{
			if (subId != 0)
			{
				AssignmentSubmission submission = subLogic.getAssignmentSubmissionById(Long
						.parseLong(submissionId));
				Assignment2 assignment = submission.getAssignment();
				HashMap<String, Object> content = new HashMap<String, Object>();

				parseAssignment(assignment, content);
				parseSubmission(submission, content);

				sendMap(request, response, content);
			}
			else if (asnnId != 0)
			{
				HashMap<String, Object> content = new HashMap<String, Object>();
				Assignment2 asnn = asnnLogic.getAssignmentById(Long.parseLong(assignmentId));
				parseAssignment(asnn, content);
				sendMap(request, response, content);
			}
		}
		catch (TypeException te)
		{
			sendError(request, response, te);
		}
		catch (PermissionException pe)
		{
			sendError(request, response, pe);
		}
		catch (IdUnusedException iue)
		{
			sendError(request, response, iue);
		}
	}

	private void parseSubmission(AssignmentSubmission submission, HashMap<String, Object> content)
			throws PermissionException, IdUnusedException, TypeException
	{
		content.put("id", submission.getId());
		List<HashMap<String, Object>> drafts = new ArrayList<HashMap<String, Object>>();
		List<HashMap<String, Object>> submHist = new ArrayList<HashMap<String, Object>>();
		Set<AssignmentSubmissionVersion> versions = submission.getSubmissionHistorySet();
		for (AssignmentSubmissionVersion asv : versions)
		{
			if (asv.isDraft())
			{
				HashMap<String, Object> d = new HashMap<String, Object>();
				d.put("id", asv.getId());
				d.put("content", asv.getSubmittedText());
				drafts.add(d);
			}
			else
			{
				HashMap<String, Object> h = new HashMap<String, Object>();
				h.put("id", asv.getId());
				h.put("date", asv.getSubmittedTime().toString());
				h.put("content", asv.getSubmittedText());
				submHist.add(h);
			}
		}
		content.put("drafts", drafts);
		AssignmentSubmissionVersion sv = submission.getCurrentSubmissionVersion();
		content.put("submission", sv.getSubmittedText());
		List<HashMap<String, Object>> attachments = new ArrayList<HashMap<String, Object>>();
		Set<SubmissionAttachment> attachSet = sv.getSubmissionAttachSet();
		for (SubmissionAttachment sa : attachSet)
		{
			HashMap<String, Object> a = new HashMap<String, Object>();

			ContentResource resource = chs.getResource(sa.getAttachmentReference());
			ResourceProperties props = resource.getProperties();

			a.put("id", props.getPropertyFormatted(props.getNamePropDisplayName()));
			a.put("type", resource.getContentType());
			a.put("url", resource.getUrl());
			attachments.add(a);
		}
		content.put("attachments", attachments);
		HashMap<String, Object> feedback = new HashMap<String, Object>();
		feedback.put("note", sv.getFeedbackNotes());
		List<HashMap<String, Object>> feedbackAttachments = new ArrayList<HashMap<String, Object>>();
		Set<SubmissionAttachment> fbAttachSet = sv.getSubmissionAttachSet();
		for (SubmissionAttachment sa : fbAttachSet)
		{
			HashMap<String, Object> a = new HashMap<String, Object>();

			ContentResource resource = chs.getResource(sa.getAttachmentReference());
			ResourceProperties props = resource.getProperties();

			a.put("id", props.getPropertyFormatted(props.getNamePropDisplayName()));
			a.put("type", resource.getContentType());
			a.put("url", resource.getUrl());
			feedbackAttachments.add(a);
		}
		feedback.put("attachments", feedbackAttachments);
		content.put("feedback", feedback);
		List<HashMap<String, Object>> history = new ArrayList<HashMap<String, Object>>();
	}

	private void parseAssignment(Assignment2 asnn, HashMap<String, Object> content)
	{
		content.put("title", asnn.getTitle());
		content.put("type", asnn.getSubmissionType());
		content.put("assignment", asnn.getInstructions());
		boolean canSubmit = permissionLogic.isUserAbleToMakeSubmissionForAssignment(asnn);
		boolean canEditFeedback = permissionLogic.isUserAllowedToProvideFeedbackForAssignment(asnn);
		content.put("editSub", canSubmit && asnn.isDraft());
		content.put("editFeedback", canEditFeedback && !asnn.isDraft());
	}
}
