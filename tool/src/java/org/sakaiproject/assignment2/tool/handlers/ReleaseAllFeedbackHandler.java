package org.sakaiproject.assignment2.tool.handlers;

import java.io.IOException;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sakaiproject.assignment2.exception.AssignmentNotFoundException;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;

/**
 * @author Stuart Freeman
 */
public class ReleaseAllFeedbackHandler extends Asnn2HandlerBase
{
	private AssignmentSubmissionLogic asnnSubLogic = null;

	@Override
	public void postInit(Map<String, String> config) throws ServletException
	{
		asnnSubLogic = (AssignmentSubmissionLogic) compMgr
				.get(AssignmentSubmissionLogic.class.getName());
	}

	@Override
	public void handlePost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		try
		{
			Long asnnId = Long.valueOf(request.getParameter("asnnId"));
			asnnSubLogic.releaseAllFeedbackForAssignment(asnnId);
			response.encodeRedirectURL("/sakai-assignment2-tool/content/templates/inst_sub_list.html?context="
							+ asnnLogic.getAssignmentById(asnnId).getContextId()
							+ "&asnnId=" + asnnId);
		}
		catch (NumberFormatException e)
		{
			response.sendError(404, e.getLocalizedMessage());
		}
		catch (AssignmentNotFoundException e)
		{
			response.sendError(404, e.getLocalizedMessage());
		}
		catch (SecurityException e)
		{
			response.sendError(403, e.getLocalizedMessage());
		}
	}
}
