package org.sakaiproject.assignment2.tool.handlers;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sakaiproject.assignment2.model.Assignment2;

/**
 * Handler to move posted assignments to drafts.
 */
public class AssignmentDraft extends Asnn2HandlerBase
{
	@Override
	public void handleGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		process(request, response);
	}

	@Override
	public void handlePost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		process(request, response);
	}

	private void process(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String[] ids = request.getParameterValues("id");
		String context = request.getParameter("context");
		for (String id : ids)
		{
			Assignment2 asnn = asnnLogic.getAssignmentByIdWithGroupsAndAttachments(Long
					.parseLong(id));
			asnn.setDraft(true);
			asnnLogic.saveAssignment(asnn);
		}
		response
				.sendRedirect("/sakai-assignment2-tool/content/templates/inst_asnn_list.html?context="
						+ context);
	}
}
