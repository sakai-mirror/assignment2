package org.sakaiproject.assignment2.tool.handlers;

import java.io.IOException;
import java.util.Date;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sakaiproject.assignment2.model.Assignment2;

public class CopyAsnnHandler extends Asnn2HandlerBase
{
	@Override
	public void handlePost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String asnnId = request.getParameter("asnnId");
		String context = request.getParameter("context");
		String redirect = request.getParameter("redirect");
		Date now = new Date();

		// clear certain values before saving back to make the copy
		Assignment2 asnn = asnnLogic.getAssignmentByIdWithGroupsAndAttachments(Long
				.parseLong(asnnId));
		asnn.setId(null);
		asnn.setDraft(true);
		// - a copy should have no due date by default
		asnn.setDueDate(null);
		// - a copy should have an open date of the moment of copying by default
		asnn.setOpenTime(now);
		// - the copy should not be assigned to any particular group or section
		// (it should be for "all site participants" by default)
		asnn.setAssignmentGroupSet(null);
		// - I'm assuming we keep some sort of creation timestamp, even if it's
		// not displayed. That of course should be set to the moment of the copy
		asnn.setCreateTime(now);
		// - everything else should be the same as the original
		asnnLogic.saveAssignment(asnn);

		if (Boolean.parseBoolean(redirect))
			response
					.sendRedirect("/sakai-assignment2-tool/content/templates/inst_asnn_list.html?context="
							+ context);
	}
}