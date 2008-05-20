package org.sakaiproject.assignment2.tool.handlers;

import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.model.Assignment2;

public class NewAsnn2Handler extends Asnn2HandlerBase
{
	private ExternalGradebookLogic gradebookLogic;
	private DateFormat dateFormatter;
	private DateFormat timeFormatter;
	private DateFormat dateTimeFormatter;
	private String dateFormat = "MM/dd/yyyy";
	private String timeFormat = "hh:mm";
	private String dateTimeFormat = dateFormat + " " + timeFormat;

	@Override
	public void postInit(Map<String, String> config) throws ServletException
	{
		gradebookLogic = (ExternalGradebookLogic) getService(ExternalGradebookLogic.class);
		dateFormatter = new SimpleDateFormat(dateFormat);
		timeFormatter = new SimpleDateFormat(timeFormat);
		dateTimeFormatter = new SimpleDateFormat(dateTimeFormat);
	}

	@Override
	public void handleGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String id = request.getParameter("id");
		Assignment2 asnn = asnnLogic.getAssignmentById(Long.parseLong(id));
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
		String draft = request.getParameter("draft");
		String prev = request.getParameter("prev");
		String submissionType = request.getParameter("submissionType");
		boolean hasOpenDate = Boolean.parseBoolean(request.getParameter("openDateRadio"));
		String openDate = request.getParameter("openDate") + " " + request.getParameter("openTime");
		boolean hasDueDate = Boolean.parseBoolean(request.getParameter("dueDateRadio"));
		String dueDate = request.getParameter("dueDate") + " " + request.getParameter("dueTime");
		String acceptUntilDate = request.getParameter("acceptUntilDate") + " "
				+ request.getParameter("acceptUntilTime");
		String whoWillSubmit = request.getParameter("whoWillSubmit");
		String grading = request.getParameter("grading");
		String gradebookId = request.getParameter("gradebookId");

		try
		{
			Assignment2 asnn = asnnLogic.getAssignmentByIdWithGroupsAndAttachments(Long
					.parseLong(id));
			asnn.setSubmissionType(Integer.parseInt(submissionType));
			if (hasOpenDate && openDate.trim().length() > 0)
				asnn.setOpenTime(dateTimeFormatter.parse(openDate));
			else
				asnn.setOpenTime(new Date());
			if (hasDueDate && dueDate.trim().length() > 0)
				asnn.setDueDate(dateTimeFormatter.parse(dueDate));
			else
				asnn.setDueDate(null);
			if (acceptUntilDate.trim().length() > 0)
				asnn.setAcceptUntilTime(dateTimeFormatter.parse(acceptUntilDate));
			else
				asnn.setAcceptUntilTime(null);

			if ("add".equals(grading))
			{
				// create new gradebook entry and set id back to assignment
				// gradebookLogic.createGbItemInGradebook(asnn.getContextId(), asnn.getTitle(), 100,
				// , false, countedInCourseGrade)
			}
			else if ("link".equals(grading))
			{
				// link to submitted gradebook entry id
				// asnn.setGradableObjectId(Long.parseLong(gradebookId));
			}
			else if ("none".equals(grading))
			{
				asnn.setGradableObjectId(null);
			}
			String next = "/sakai-assignment2-tool/content/templates/newassignment3.html?id="
					+ asnn.getId();
			if (draft != null)
			{
				asnn.setDraft(true);
				next = "/sakai-assignment2-tool/content/templates/close.html";
			}
			else if (prev != null)
			{
				next = "/sakai-assignment2-tool/content/templates/newassignment1.html?id="
						+ asnn.getId();
			}
			asnnLogic.saveAssignment(asnn);

			response.sendRedirect(next);
		}
		catch (ParseException pe)
		{
			sendError(request, response, pe);
		}
	}
}
