package org.sakaiproject.assignment2.tool.handlers;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sakaiproject.assignment2.model.Assignment2;

/**
 * Handles the deleting of assignments
 */
public class DeleteAsnnHandler extends Asnn2HandlerBase
{
	@Override
	public void handlePost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String[] asnnIds = request.getParameterValues("asnnId");
		if (asnnIds != null && asnnIds.length > 0)
		{
			for (String asnnId : asnnIds)
			{
				Assignment2 asnn = asnnLogic.getAssignmentById(Long.parseLong(asnnId));
				asnnLogic.deleteAssignment(asnn);
			}
		}
	}
}