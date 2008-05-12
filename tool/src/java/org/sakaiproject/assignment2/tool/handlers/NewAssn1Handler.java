package org.sakaiproject.assignment2.tool.handlers;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.component.api.ComponentManager;

public class NewAssn1Handler extends Assn2HandlerBase
{
	private ComponentManager compMgr;
	private AssignmentLogic assnLogic;

	@Override
	public void postInit(Map<String, String> config) throws ServletException
	{
		compMgr = org.sakaiproject.component.cover.ComponentManager.getInstance();
		assnLogic = (AssignmentLogic) compMgr.get(AssignmentLogic.class.getName());
	}

	@Override
	public void handleGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String id = request.getParameter("id");
		Assignment2 assn = assnLogic.getAssignmentById(Long.parseLong(id));
		HashMap<String, Object> map = new HashMap<String, Object>();
		map.put("title", assn.getTitle());
		map.put("instructions", assn.getInstructions());

		sendMap(request, response, map);
	}

	@Override
	public void handlePost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		String title = request.getParameter("title");
		String instructions = request.getParameter("instructions");
		Assignment2 assn = new Assignment2();
		assn.setTitle(title);
		assn.setInstructions(instructions);

		String draft = request.getParameter("draft");
		String next = "/sakai-assingment2-tool/sdata/newAssn2?id=" + assn.getId();
		if (draft != null)
		{
			assn.setDraft(true);
			next = "/sakai-assingment2-tool/content/templates/close.html";
		}

		assnLogic.saveAssignment(assn);

		response.sendRedirect(next);
	}
}
