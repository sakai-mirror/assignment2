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

import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;

public class SubmissionListHandler extends Asnn2HandlerBase
{
	private AssignmentSubmissionLogic subLogic = null;
	private DateFormat dateFormat;

	@Override
	public void postInit(Map<String, String> config) throws ServletException
	{
		subLogic = (AssignmentSubmissionLogic) getService(AssignmentSubmissionLogic.class);
		dateFormat = new SimpleDateFormat("MM/dd");
	}

	@Override
	public void handleGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		HashMap<String, Object> content = new HashMap<String, Object>();

		String asnnId = request.getParameter("asnnId");
		String context = request.getParameter("context");
		if (asnnId != null)
		{
			content.put("asnnId", asnnId);
			List<Map<String, Object>> subs = parseSubmissions(Long.parseLong(asnnId));
			content.put("submissions", subs);
		}
		else
		{
			content.put("context", context);
			List<Map<String, Object>> asnns = parseAssignments(context);
			content.put("assignments", asnns);
		}

		sendMap(request, response, content);
	}

	private List<Map<String, Object>> parseAssignments(String context)
	{
		List<Map<String, Object>> assignments = new ArrayList<Map<String, Object>>();
		List<Assignment2> asnns = asnnLogic.getViewableAssignments(context);
		for (Assignment2 asnn : asnns)
		{
			// get specific elements of data
			HashMap<String, Object> a = new HashMap<String, Object>();
			a.put("id", asnn.getId());
			a.put("title", asnn.getTitle());
			String type = "electronic";
			if (asnn.getSubmissionType() == AssignmentConstants.SUBMIT_NON_ELECTRONIC)
				type = "non-electronic";
			a.put("type", type);
			assignments.add(a);
		}
		return assignments;
	}

	/**
	 * TODO get submission by submitted/completed
	 * 
	 * @param asnnId
	 * @return
	 */
	private List<Map<String, Object>> parseSubmissions(Long asnnId)
	{
		Assignment2 asnn = asnnLogic.getAssignmentByIdWithGroupsAndAttachments(asnnId);
		List<Map<String, Object>> subs = new ArrayList<Map<String, Object>>();
		for (AssignmentSubmission sub : asnn.getSubmissionsSet())
		{
			HashMap<String, Object> s = new HashMap<String, Object>();
			if (asnn.getSubmissionType() == AssignmentConstants.SUBMIT_NON_ELECTRONIC)
			{
				s.put("name", sub.getUserId());
				s.put("sections", "");
				s.put("feedback", sub.getCurrentSubmissionVersion().getFeedbackNotes());
			}
			else
			{
				AssignmentSubmissionVersion ver = sub.getCurrentSubmissionVersion();
				s.put("name", sub.getUserId());
				s.put("submittedDate", ver.getCreatedTime());
				s.put("dueDate", dateFormat.format(asnn.getDueDate()));
				s.put("sections", "");
				s.put("feedback", ver.getFeedbackNotes());
			}
			subs.add(s);
		}
		return subs;
	}
}
