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

import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
import org.sakaiproject.component.api.ComponentManager;
import org.sakaiproject.content.api.ContentHostingService;
import org.sakaiproject.content.api.ContentResource;
import org.sakaiproject.entity.api.ResourceProperties;
import org.sakaiproject.exception.IdUnusedException;
import org.sakaiproject.exception.PermissionException;
import org.sakaiproject.exception.TypeException;

public class SubmissionHandler extends Assn2HandlerBase
{
	private ComponentManager compMgr = null;
	private AssignmentSubmissionLogic subLogic = null;
	private ContentHostingService chs = null;
	
	@Override
	public void postInit(Map<String, String> config) throws ServletException
	{
		compMgr = org.sakaiproject.component.cover.ComponentManager.getInstance();
		subLogic = (AssignmentSubmissionLogic) compMgr.get(AssignmentSubmissionLogic.class
				.getName());
		chs = (ContentHostingService) compMgr.get(ContentHostingService.class.getName());
	}

	@Override
	public void handleGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		try
		{
			String submissionId = request.getParameter("submissionId");
			AssignmentSubmission submission = subLogic.getAssignmentSubmissionById(new Long(submissionId));
			Assignment2 assignment = submission.getAssignment();
			HashMap<String, Object> content = new HashMap<String, Object>();
			
			content.put("id", submission.getId());
			content.put("title", assignment.getTitle());
			content.put("type", assignment.getSubmissionType());
			content.put("assignment", assignment.getInstructions());
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
}
