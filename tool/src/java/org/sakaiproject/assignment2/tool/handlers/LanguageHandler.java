package org.sakaiproject.assignment2.tool.handlers;

import java.io.IOException;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sakaiproject.assignment2.logic.AssignmentBundleLogic;

public class LanguageHandler extends Assn2HandlerBase
{
	private AssignmentBundleLogic bundle;
	private Map<Locale, Map<String, Object>> languages = new HashMap<Locale, Map<String, Object>>();

	@Override
	public void postInit(Map<String, String> config) throws ServletException
	{
		bundle = (AssignmentBundleLogic) getService(AssignmentBundleLogic.class);
		// preload the active bundle
		getActiveBundle();
	}

	@Override
	public void handleGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		sendMap(request, response, getActiveBundle());
	}

	private Map<String, Object> getActiveBundle()
	{
		Locale locale = bundle.getLocale();
		Map<String, Object> keysValues = null;
		if (languages.containsKey(locale))
			keysValues = languages.get(locale);
		else
		{
			Set<Map.Entry<String, String>> entries = bundle.entrySet();
			keysValues = new HashMap<String, Object>();
			for (Map.Entry<String, String> entry : entries)
				keysValues.put(entry.getKey(), entry.getValue());
			languages.put(bundle.getLocale(), keysValues);
		}
		return keysValues;
	}
}
