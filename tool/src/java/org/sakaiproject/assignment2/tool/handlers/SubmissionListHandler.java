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
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.user.api.User;
import org.sakaiproject.user.api.UserDirectoryService;
import org.sakaiproject.user.api.UserNotDefinedException;

public class SubmissionListHandler extends Asnn2HandlerBase
{
	private AssignmentSubmissionLogic subLogic = null;
	private DateFormat dateFormat;
	private UserDirectoryService uds;

	@Override
	public void postInit(Map<String, String> config) throws ServletException
	{
		subLogic = (AssignmentSubmissionLogic) getService(AssignmentSubmissionLogic.class);
		uds = (UserDirectoryService) getService(UserDirectoryService.class);
		dateFormat = new SimpleDateFormat("MM/dd");
	}

	@Override
	public void handleGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String asnnId = request.getParameter("asnnId");
		String context = request.getParameter("context");
		HashMap<String, Object> content = new HashMap<String, Object>();
		if (asnnId != null)
		{
			Long id = Long.parseLong(asnnId);
			Assignment2 asnn = asnnLogic.getAssignmentById(id);

			content.put("asnnId", asnnId);
			content.put("context", asnn.getContextId());
			List<Map<String, Object>> vers = parseSubmissionVersions(id);
			content.put("versions", vers);
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
	private List<Map<String, Object>> parseSubmissionVersions(Long asnnId)
	{
		List<AssignmentSubmissionVersion> versions = subLogic.getLatestSubmissionsForAssignment(asnnId);
		List<Map<String, Object>> subs = new ArrayList<Map<String, Object>>();
		for (AssignmentSubmissionVersion version : versions)
		{
			Assignment2 asnn = version.getAssignmentSubmission().getAssignment();
			HashMap<String, Object> s = new HashMap<String, Object>();
			s.put("id", version.getId());
			try
			{
				User user = uds.getUser(version.getCreatedBy());
				s.put("name", user.getDisplayName());
			}
			catch (UserNotDefinedException unde)
			{
				s.put("name", "User not found");
			}
			s.put("sections", "");
			s.put("feedback", version.getFeedbackNotes());

			if (asnn.getSubmissionType() != AssignmentConstants.SUBMIT_NON_ELECTRONIC)
			{
				String dueDate = "";
				String submittedDate = "";
				if (asnn.getDueDate() != null)
					dueDate = dateFormat.format(asnn.getDueDate());
				submittedDate = dateFormat.format(version.getCreatedTime());
				s.put("submittedDate", submittedDate);
				s.put("dueDate", dueDate);
			}
			subs.add(s);
		}
		return subs;
	}
}
