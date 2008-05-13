package org.sakaiproject.assignment2.tool.handlers;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.model.Assignment2;

public class NewAsnn3Handler extends Asnn2HandlerBase
{
	private AssignmentLogic assnLogic;

	@Override
	public void postInit(Map<String, String> config) throws ServletException
	{
		assnLogic = (AssignmentLogic) getService(AssignmentLogic.class);
	}

	@Override
	public void handleGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String id = request.getParameter("id");
		Assignment2 assn = assnLogic.getAssignmentById(Long.parseLong(id));

		HashMap<String, Object> map = new HashMap<String, Object>();
		map.put("resubmissions", assn.getNumSubmissionsAllowed());
		map.put("notifications", assn.getInstructions());
		map.put("honorCode", assn.getInstructions());

		sendMap(request, response, map);
	}

	@Override
	public void handlePost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String id = request.getParameter("id");
		String step = request.getParameter("step");
		String resubmissions = request.getParameter("resubmissions");
		String notifications = request.getParameter("notifications");
		String honorCode = request.getParameter("honorCode");

		Assignment2 assn = assnLogic.getAssignmentById(Long.parseLong(id));
		assn.setNumSubmissionsAllowed(Integer.parseInt(resubmissions));
		assn.setNotificationType(Integer.parseInt(notifications));
		assn.setHonorPledge(Boolean.parseBoolean(honorCode));

		String draft = request.getParameter("draft");
		String next = "assnList";
		if (draft != null)
		{
			assn.setDraft(true);
			next = "/sakai-assingment2-tool/content/templates/close.html";
		}
		else if ("prev".equals(step))
			next = "/sakai-assingment2-tool/sdata/newAssn2?id=" + assn.getId();
		// check if the 'post' button was clicked
		else if (request.getParameter("post") != null)
			assn.setDraft(false);
		assnLogic.saveAssignment(assn);

		response.sendRedirect(next);
	}
}
