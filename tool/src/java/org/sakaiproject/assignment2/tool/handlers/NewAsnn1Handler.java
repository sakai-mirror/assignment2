package org.sakaiproject.assignment2.tool.handlers;

import java.io.IOException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;

public class NewAsnn1Handler extends Asnn2HandlerBase
{
	private AssignmentLogic assnLogic;
	private ExternalLogic extLogic;

	@Override
	public void postInit(Map<String, String> config) throws ServletException
	{
		assnLogic = (AssignmentLogic) getService(AssignmentLogic.class);
		extLogic = (ExternalLogic) getService(ExternalLogic.class);
	}

	@Override
	public void handleGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String id = request.getParameter("id");
		if (id != null)
		{
			Assignment2 assn = assnLogic.getAssignmentById(Long.parseLong(id));
			HashMap<String, Object> map = new HashMap<String, Object>();
			map.put("title", assn.getTitle());
			map.put("instructions", assn.getInstructions());

			sendMap(request, response, map);
		}
	}

	@Override
	public void handlePost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String id = request.getParameter("id");
		String context = request.getParameter("context");
		String title = request.getParameter("title");
		String instructions = request.getParameter("instructions");
		String draft = request.getParameter("draft");
		String next = "/sakai-assignment2-tool/content/templates/newassignment2.html?context="
				+ context + "&id=" + id;

		Assignment2 asnn = new Assignment2();
		// set the data that is submitted
		if (id != null && id.length() > 0)
			asnn.setId(Long.parseLong(id));
		asnn.setContextId(context);
		asnn.setTitle(title);
		if (draft != null)
		{
			asnn.setDraft(true);
			next = "/sakai-assignment2-tool/content/templates/close.html";
		}
		else
		{
			asnn.setDraft(true);
		}
		asnn.setInstructions(instructions);

		// set the required fields to reasonable defaults
		asnn.setSortIndex(-1);
		asnn.setOpenTime(new Date());
		asnn.setUngraded(true);
		asnn.setHonorPledge(false);
		asnn.setSubmissionType(AssignmentConstants.SUBMIT_INLINE_ONLY);
		asnn.setNotificationType(AssignmentConstants.NOTIFY_NONE);
		asnn.setHasAnnouncement(false);
		asnn.setAddedToSchedule(false);
		asnn.setCreator(extLogic.getCurrentUserId());
		asnn.setCreateTime(new Date());
		asnn.setRemoved(false);

		assnLogic.saveAssignment(asnn, context);

		response.sendRedirect(next);
	}
}
