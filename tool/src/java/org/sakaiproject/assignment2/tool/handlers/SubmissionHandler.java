package org.sakaiproject.assignment2.tool.handlers;

import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
	private AssignmentSubmissionLogic subLogic;
	private ContentHostingService chs;
	private AssignmentPermissionLogic permLogic;
	private ExternalLogic extLogic;
	private DateFormat dateFormat;

	@Override
	public void postInit(Map<String, String> config) throws ServletException
	{
		subLogic = (AssignmentSubmissionLogic) getService(AssignmentSubmissionLogic.class.getName());
		chs = (ContentHostingService) getService(ContentHostingService.class.getName());
		permLogic = (AssignmentPermissionLogic) getService(AssignmentPermissionLogic.class
				.getName());
		extLogic = (ExternalLogic) getService(ExternalLogic.class);
		dateFormat = new SimpleDateFormat("MM/dd");
	}

	@Override
	public void handlePost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String asnnId = request.getParameter("asnnId");
		String verId = request.getParameter("verId");

		if (asnnId == null && verId == null)
		{
			sendError(request, response, new IllegalArgumentException(
					"Must provide assignment or version Id"));
		}
		else
		{
			String draft = request.getParameter("draft");
			String submission = request.getParameter("submission");
			boolean isDraft = (draft != null);

			Assignment2 asnn = null;
			if (verId != null)
			{
				AssignmentSubmissionVersion asv = subLogic.getSubmissionVersionById(Long
						.parseLong(verId));
				asnn = asv.getAssignmentSubmission().getAssignment();
			}
			else if (asnnId != null)
			{
				asnn = asnnLogic.getAssignmentById(Long.parseLong(asnnId));
			}

			// Note: saving as draft should cause a new version to be created
			// posting should only move the version to a posted state
			String userId = extLogic.getCurrentUserId();
			subLogic.saveStudentSubmission(userId, asnn, isDraft, submission, null);

			// send back to submission list
			response.sendRedirect("/sakai-assignment2-tool/content/templates/inst_sub_list.html?context="
					+ asnn.getContextId() + "&asnnId=" + asnnId);
		}
	}

	@Override
	public void handleGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		// capture the inputs
		String assignmentId = StringUtil.trimToNull(request.getParameter("asnnId"));
		String submissionId = StringUtil.trimToNull(request.getParameter("subId"));
		String versionId = StringUtil.trimToNull(request.getParameter("verId"));

		// convert as needed
		Long asnnId = 0L;
		try
		{
			asnnId = Long.parseLong(assignmentId);
		}
		catch (NumberFormatException nfe)
		{
		}
		Long subId = 0L;
		try
		{
			subId = Long.parseLong(submissionId);
		}
		catch (NumberFormatException nfe)
		{
		}
		Long verId = 0L;
		try
		{
			verId = Long.parseLong(versionId);
		}
		catch (NumberFormatException nfe)
		{
		}

		try
		{
			HashMap<String, Object> content = new HashMap<String, Object>();
			// get needed objects from known data
			AssignmentSubmission submission = null;
			if (verId != 0)
			{
				AssignmentSubmissionVersion version = subLogic.getSubmissionVersionById(verId);
				submission = version.getAssignmentSubmission();
				content.put("versionId", verId);
				content.put("submission", version.getSubmittedText());
				content.put("feedback", version.getFeedbackNotes());
			}
			if (subId != 0 && submission == null)
			{
				submission = subLogic.getAssignmentSubmissionById(subId);
			}

			// process a submission request
			if (submission != null)
			{
				content.put("submissionId", submission.getId());
				parseAssignment(submission.getAssignment(), content);
				parseSubmission(submission, verId, content);
			}
			// process an assignment request
			else if (asnnId != 0)
			{
				Assignment2 asnn = asnnLogic.getAssignmentById(Long.parseLong(assignmentId));
				parseAssignment(asnn, content);
			}
			sendMap(request, response, content);
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

	private void parseSubmission(AssignmentSubmission submission, Long versionId,
			HashMap<String, Object> content) throws PermissionException, IdUnusedException,
			TypeException
	{
		content.put("versionId", versionId);
		content.put("submissionId", submission.getId());

		// history list
		List<HashMap<String, Object>> submHist = new ArrayList<HashMap<String, Object>>();
		List<AssignmentSubmissionVersion> versions = subLogic
				.getVersionHistoryForSubmission(submission);
		for (AssignmentSubmissionVersion asv : versions)
		{
			HashMap<String, Object> h = new HashMap<String, Object>();
			h.put("id", asv.getId());
			h.put("content", asv.getSubmittedText());
			h.put("createdDate", dateFormat.format(asv.getCreatedTime()));
			String submittedDate = "";
			if (asv.getSubmittedTime() != null)
				submittedDate = dateFormat.format(asv.getSubmittedTime());
			h.put("submittedDate", submittedDate);
			submHist.add(h);
		}
		content.put("history", submHist);
		// AssignmentSubmissionVersion sv = submission.getCurrentSubmissionVersion();

		// submission text & attachments
		// content.put("submission", sv.getSubmittedText());
		// List<HashMap<String, Object>> attachments = new ArrayList<HashMap<String, Object>>();
		// Set<SubmissionAttachment> attachSet = sv.getSubmissionAttachSet();
		// for (SubmissionAttachment sa : attachSet)
		// {
		// HashMap<String, Object> a = new HashMap<String, Object>();
		//
		// ContentResource resource = chs.getResource(sa.getAttachmentReference());
		// ResourceProperties props = resource.getProperties();
		//
		// a.put("id", props.getPropertyFormatted(props.getNamePropDisplayName()));
		// a.put("type", resource.getContentType());
		// a.put("url", resource.getUrl());
		// attachments.add(a);
		// }
		// content.put("attachments", attachments);

		// feedback & feedback attachments
		// HashMap<String, Object> feedback = new HashMap<String, Object>();
		// feedback.put("note", sv.getFeedbackNotes());
		// List<HashMap<String, Object>> feedbackAttachments = new ArrayList<HashMap<String,
		// Object>>();
		// Set<SubmissionAttachment> fbAttachSet = sv.getSubmissionAttachSet();
		// for (SubmissionAttachment sa : fbAttachSet)
		// {
		// HashMap<String, Object> a = new HashMap<String, Object>();
		//
		// ContentResource resource = chs.getResource(sa.getAttachmentReference());
		// ResourceProperties props = resource.getProperties();
		//
		// a.put("id", props.getPropertyFormatted(props.getNamePropDisplayName()));
		// a.put("type", resource.getContentType());
		// a.put("url", resource.getUrl());
		// feedbackAttachments.add(a);
		// }
		// feedback.put("attachments", feedbackAttachments);
		// content.put("feedback", feedback);
	}

	private void parseAssignment(Assignment2 asnn, HashMap<String, Object> content)
	{
		HashMap<String, Object> a = new HashMap<String, Object>();
		a.put("id", asnn.getId());
		a.put("context", asnn.getContextId());
		a.put("title", asnn.getTitle());
		a.put("type", asnn.getSubmissionType());
		a.put("instructions", asnn.getInstructions());
		content.put("assignment", a);

		// TEMP SETTING FOR TESTING AND LOADING
		content.put("editSub", true);
		content.put("editFeedback", false);

		// boolean canSubmit = permLogic.isUserAbleToMakeSubmissionForAssignment(asnn);
		// boolean canEditFeedback = permLogic.isUserAllowedToProvideFeedbackForAssignment(asnn);
		// content.put("editSub", canSubmit && asnn.isDraft());
		// content.put("editFeedback", canEditFeedback && !asnn.isDraft());
	}
}
