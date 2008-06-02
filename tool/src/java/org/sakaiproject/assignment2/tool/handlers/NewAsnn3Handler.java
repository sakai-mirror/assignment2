package org.sakaiproject.assignment2.tool.handlers;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sakaiproject.assignment2.model.Assignment2;

public class NewAsnn3Handler extends Asnn2HandlerBase
{
	@Override
	public void postInit(Map<String, String> config) throws ServletException
	{
	}

	@Override
	public void handleGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String id = request.getParameter("id");
		Assignment2 asnn = asnnLogic.getAssignmentById(Long.parseLong(id));

		HashMap<String, Object> map = new HashMap<String, Object>();
		map.put("resubmissions", asnn.getNumSubmissionsAllowed());
		map.put("notifications", asnn.getNotificationType());
		map.put("honorCode", asnn.isHonorPledge());

		sendMap(request, response, map);
	}

	@Override
	public void handlePost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String id = request.getParameter("id");
		String begin = request.getParameter("begin");
		String prev = request.getParameter("prev");
		String post = request.getParameter("post");
		String resubmissions = request.getParameter("resubmissions");
		String numResubmit = request.getParameter("numResubmit");
		String notifications = request.getParameter("notifications");
		String honorCode = request.getParameter("honorcode");
		String draft = request.getParameter("draft");

		Assignment2 asnn = asnnLogic.getAssignmentByIdWithGroupsAndAttachments(Long.parseLong(id));
		if (resubmissions != null)
			if (numResubmit != null && Integer.parseInt(resubmissions) >= 2)
				asnn.setNumSubmissionsAllowed(Integer.parseInt(numResubmit));
			else
				asnn.setNumSubmissionsAllowed(Integer.parseInt(resubmissions));
		asnn.setNotificationType(Integer.parseInt(notifications));
		asnn.setHonorPledge(Boolean.parseBoolean(honorCode));
		if (draft != null)
			asnn.setDraft(true);
		// check if the 'post' button was clicked
		else if (post != null)
			asnn.setDraft(false);
		asnnLogic.saveAssignment(asnn);

		String next = "/sakai-assignment2-tool/content/templates/close.html?refresh=true";
		
		if (prev != null)
		{
			next = "/sakai-assignment2-tool/content/templates/newassignment2.html?id="
					+ asnn.getId();
		}
		else if (begin != null)
		{
			next = "/sakai-assignment2-tool/content/templates/newassignment1.html?id="
					+ asnn.getId();
		}

		response.sendRedirect(next);
	}
}
