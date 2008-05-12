package org.sakaiproject.assignment2.tool.handlers;

import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.component.api.ComponentManager;
import org.sakaiproject.sdata.tool.json.JSONServiceHandler;

public class NewAssn2Handler extends Assn2HandlerBase
{
	private ComponentManager compMgr;
	private AssignmentLogic assnLogic;
	private DateFormat dateFormat;

	@Override
	public void postInit(Map<String, String> config) throws ServletException
	{
		compMgr = org.sakaiproject.component.cover.ComponentManager.getInstance();
		assnLogic = (AssignmentLogic) compMgr.get(AssignmentLogic.class.getName());
		dateFormat = new SimpleDateFormat("MM/dd");
	}

	@Override
	public void handleGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String id = request.getParameter("id");
		Assignment2 assn = assnLogic.getAssignmentById(Long.parseLong(id));
		HashMap<String, Object> map = new HashMap<String, Object>();
		map.put("submissionType", assn.getSubmissionType());
		map.put("openDate", dateFormat.format(assn.getInstructions()));
		map.put("dueDate", dateFormat.format(assn.getDueDate()));
		map.put("acceptUntil", dateFormat.format(assn.getAcceptUntilTime()));
		map.put("whoWillSubmit", "");
		map.put("grading", "");

		sendMap(request, response, map);
	}

	@Override
	public void handlePost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String id = request.getParameter("id");
		String submissionType = request.getParameter("submissionType");
		String openDate = request.getParameter("openDate");
		String dueDate = request.getParameter("dueDate");
		String acceptUntil = request.getParameter("acceptUntil");
		String whoWillSubmit = request.getParameter("whoWillSubmit");
		String grading = request.getParameter("grading");

		try
		{
			Assignment2 assn = assnLogic.getAssignmentById(Long.parseLong(id));
			assn.setSubmissionType(Integer.parseInt(submissionType));
			assn.setOpenTime(dateFormat.parse(openDate));
			assn.setDueDate(dateFormat.parse(dueDate));
			assn.setAcceptUntilTime(dateFormat.parse(acceptUntil));

			String draft = request.getParameter("draft");
			String step = request.getParameter("step");
			String next = "/sakai-assingment2-tool/sdata/newAssn3?id=" + assn.getId();
			if (draft != null)
			{
				assn.setDraft(true);
				next = "/sakai-assingment2-tool/content/templates/close.html";
			}
			else if ("prev".equals(step))
			{
				next = "/sakai-assingment2-tool/sdata/newAssn1?id=" + assn.getId();
			}
			assnLogic.saveAssignment(assn);

			response.sendRedirect(next);
		}
		catch (ParseException pe)
		{
			sendError(request, response, pe);
		}
	}
}
