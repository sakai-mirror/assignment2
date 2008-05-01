package org.sakaiproject.assignment2.tool.handlers;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.component.api.ComponentManager;
import org.sakaiproject.sdata.tool.json.JSONServiceHandler;

public class SubmissionListHandler extends JSONServiceHandler
{
	ComponentManager compMgr = null;
	AssignmentLogic assnLogic = null;
	AssignmentSubmissionLogic subLogic = null;

	@Override
	public void init(Map<String, String> config) throws ServletException
	{
		compMgr = org.sakaiproject.component.cover.ComponentManager.getInstance();
		assnLogic = (AssignmentLogic) compMgr.get(AssignmentLogic.class.getName());
		subLogic = (AssignmentSubmissionLogic) compMgr.get(AssignmentSubmissionLogic.class
				.getName());
	}

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		HashMap<String, Object> content = new HashMap<String, Object>();
		ArrayList<HashMap<String, Object>> assignments = new ArrayList<HashMap<String, Object>>();

		List<Assignment2> assns = assnLogic.getViewableAssignments();
		for (Assignment2 assn : assns)
		{
			// get specific elements of data
			HashMap<String, Object> a = new HashMap<String, Object>();
			a.put("id", assn.getId());
			a.put("title", assn.getTitle());
			a.put("type", assn.getSubmissionType());

			// get submissions
			// TODO get submission by submitted/completed
			List<Map<String, Object>> subs = parseSubmissions(assn);
			a.put("submissions", subs);
			assignments.add(a);
		}

		content.put("assignments", assignments);
		sendMap(request, response, content);
	}

	@Override
	public void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{

	}

	private List<Map<String, Object>> parseSubmissions(Assignment2 assn)
	{
		List<Map<String, Object>> subs = new ArrayList<Map<String, Object>>();
		for (AssignmentSubmission sub : assn.getSubmissionsSet())
		{
			HashMap<String, Object> s = new HashMap<String, Object>();
			if (assn.getSubmissionType() == AssignmentConstants.SUBMIT_NON_ELECTRONIC)
			{
				s.put("name", sub.getUserId());
				s.put("sections", "");
				s.put("feedback", sub.getCurrentSubmissionVersion().getFeedbackNotes());
			}
			else
			{
				AssignmentSubmissionVersion ver = sub.getCurrentSubmissionVersion();
				s.put("name", sub.getUserId());
				s.put("submittedOn", ver.getCreatedTime());
				s.put("dueDate", assn.getDueDate());
				s.put("sections", "");
				s.put("feedback", ver.getFeedbackNotes());
			}
			subs.add(s);
		}
		return subs;
	}
}
