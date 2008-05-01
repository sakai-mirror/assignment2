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

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.component.api.ComponentManager;
import org.sakaiproject.sdata.tool.json.JSONServiceHandler;

public class AssignmentListHandler extends JSONServiceHandler
{
	private ComponentManager compMgr = null;
	private AssignmentLogic assnLogic = null;
	private AssignmentSubmissionLogic subLogic = null;
	private DateFormat dateFormat;

	@Override
	public void init(Map<String, String> config) throws ServletException
	{
		compMgr = org.sakaiproject.component.cover.ComponentManager.getInstance();
		assnLogic = (AssignmentLogic) compMgr.get(AssignmentLogic.class.getName());
		dateFormat = new SimpleDateFormat("MM/dd");
	}

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		HashMap<String, Object> content = new HashMap<String, Object>();
		ArrayList<HashMap<String, Object>> drafts = new ArrayList<HashMap<String, Object>>();
		ArrayList<HashMap<String, Object>> posted = new ArrayList<HashMap<String, Object>>();

		List<Assignment2> assns = assnLogic.getViewableAssignments();
		for (Assignment2 assn : assns)
		{
			// get specific elements of data
			HashMap<String, Object> a = new HashMap<String, Object>();
			a.put("id", assn.getId());
			a.put("title", assn.getTitle());
			a.put("sections", "");
			a.put("openDate", dateFormat.format(assn.getOpenTime()));
			a.put("dueDate", dateFormat.format(assn.getDueDate()));

			if (assn.isDraft())
				drafts.add(a);
			else
				posted.add(a);
		}
		content.put("drafts", drafts);
		content.put("posted", posted);
		sendMap(request, response, content);
	}

	@Override
	public void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{

	}
}
