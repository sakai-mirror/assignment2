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

public class NewAsnn2Handler extends Asnn2HandlerBase
{
	private AssignmentLogic assnLogic;
	private DateFormat dateFormatter;
	private DateFormat timeFormatter;
	private DateFormat dateTimeFormatter;
	private String dateFormat = "MM/dd/yyyy";
	private String timeFormat = "hh:mm";
	private String dateTimeFormat = dateFormat + " " + timeFormat;

	@Override
	public void postInit(Map<String, String> config) throws ServletException
	{
		assnLogic = (AssignmentLogic) getService(AssignmentLogic.class);
		dateFormatter = new SimpleDateFormat(dateFormat);
		timeFormatter = new SimpleDateFormat(timeFormat);
		dateTimeFormatter = new SimpleDateFormat(dateTimeFormat);
	}

	@Override
	public void handleGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String id = request.getParameter("id");
		Assignment2 asnn = assnLogic.getAssignmentById(Long.parseLong(id));
		HashMap<String, Object> map = new HashMap<String, Object>();
		map.put("submissionType", asnn.getSubmissionType());
		if (asnn.getOpenTime() != null)
		{
			map.put("openDate", dateFormatter.format(asnn.getOpenTime()));
			map.put("openTime", timeFormatter.format(asnn.getOpenTime()));
		}
		else
		{
			map.put("openDate", "");
			map.put("openTime", "");
		}
		if (asnn.getDueDate() != null)
		{
			map.put("dueDate", dateTimeFormatter.format(asnn.getDueDate()));
			map.put("dueTime", timeFormatter.format(asnn.getDueDate()));
		}
		else
		{
			map.put("dueDate", "");
			map.put("dueTime", "");
		}
		if (asnn.getAcceptUntilTime() != null)
		{
			map.put("acceptUntilDate", dateTimeFormatter.format(asnn.getAcceptUntilTime()));
			map.put("acceptUntilTime", timeFormatter.format(asnn.getAcceptUntilTime()));
		}
		else
		{
			map.put("acceptUntilDate", "");
			map.put("acceptUntilTime", "");
		}
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
		String openDate = request.getParameter("openDate") + " " + request.getParameter("openTime");
		String dueDate = request.getParameter("dueDate") + " " + request.getParameter("dueTime");
		String acceptUntilDate = request.getParameter("acceptUntilDate") + " "
				+ request.getParameter("acceptUntilTime");
		String whoWillSubmit = request.getParameter("whoWillSubmit");
		String grading = request.getParameter("grading");

		try
		{
			Assignment2 asnn = assnLogic.getAssignmentById(Long.parseLong(id));
			asnn.setSubmissionType(Integer.parseInt(submissionType));
			asnn.setOpenTime(dateTimeFormatter.parse(openDate));
			asnn.setDueDate(dateTimeFormatter.parse(dueDate));
			asnn.setAcceptUntilTime(dateTimeFormatter.parse(acceptUntilDate));

			String draft = request.getParameter("draft");
			String step = request.getParameter("step");
			String next = "/sakai-assingment2-tool/sdata/newassignment3.html?context="
					+ asnn.getContextId() + "id=" + asnn.getId();
			if (draft != null)
			{
				asnn.setDraft(true);
				next = "/sakai-assingment2-tool/content/templates/close.html";
			}
			else if ("prev".equals(step))
			{
				next = "/sakai-assingment2-tool/sdata/newassignment1.html?context="
						+ asnn.getContextId() + "id=" + asnn.getId();
			}
			assnLogic.saveAssignment(asnn);

			response.sendRedirect(next);
		}
		catch (ParseException pe)
		{
			sendError(request, response, pe);
		}
	}
}
