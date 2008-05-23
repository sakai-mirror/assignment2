package org.sakaiproject.assignment2.tool.handlers;

import java.io.IOException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;

public class NewAsnn1Handler extends Asnn2HandlerBase
{
	private ExternalLogic extLogic;

	@Override
	public void postInit(Map<String, String> config) throws ServletException
	{
		extLogic = (ExternalLogic) getService(ExternalLogic.class);
	}

	@Override
	public void handleGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String id = request.getParameter("id");
		if (id != null)
		{
			Assignment2 assn = asnnLogic.getAssignmentById(Long.parseLong(id));
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
		if (id == null || id.trim().length() == 0)
		{
			response.sendRedirect("/sakai-assignment2-tool/content/templates/newassignment2.html?context="
				+ context);
		}
		else
		{
			String title = request.getParameter("title");
			String instructions = request.getParameter("instructions");
			String draft = request.getParameter("draft");
			String end = request.getParameter("end");
			String next = "/sakai-assignment2-tool/content/templates/newassignment2.html?id=" + id;
	
			Assignment2 asnn = null;
			if (id != null && id.length() > 0)
				asnn = asnnLogic.getAssignmentByIdWithGroupsAndAttachments(Long.parseLong(id));
			else
			{
				asnn = new Assignment2();
				asnn.setContextId(context);
				asnn.setCreator(extLogic.getCurrentUserId());
				asnn.setCreateTime(new Date());
				// set the required fields to reasonable defaults
				asnn.setOpenTime(new Date());
				asnn.setUngraded(true);
				asnn.setHonorPledge(false);
				asnn.setHasAnnouncement(false);
				asnn.setAddedToSchedule(false);
				asnn.setRemoved(false);
			}
			// set the data that is submitted
			asnn.setTitle(title);
			if (draft != null)
			{
				asnn.setDraft(true);
				next = "/sakai-assignment2-tool/content/templates/close.html?refresh=true";
			}
			else if (end != null)
			{
				next = "/sakai-assignment2-tool/content/templates/newassignment3.html?id=" + id;
			}
			if (asnn.isDraft() == null)
			{
				asnn.setDraft(false);
			}
			asnn.setInstructions(instructions);
	
			asnnLogic.saveAssignment(asnn);

			response.sendRedirect(next);
		}
	}
}
