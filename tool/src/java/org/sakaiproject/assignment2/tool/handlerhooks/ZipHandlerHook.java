/**********************************************************************************
 * $URL$
 * $Id$
 ***********************************************************************************
 *
 * Copyright (c) 2007, 2008 The Sakai Foundation.
 *
 * Licensed under the Educational Community License, Version 1.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.opensource.org/licenses/ecl1.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 **********************************************************************************/

package org.sakaiproject.assignment2.tool.handlerhooks;

import java.io.IOException;
import java.io.OutputStream;

import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.ZipExportLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.tool.params.ZipViewParams;

import uk.org.ponder.rsf.processor.HandlerHook;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.stringutil.URLEncoder;
import uk.org.ponder.util.UniversalRuntimeException;

/**
 * Handles the generation of zip files for download all
 * 
 * @author Stuart Freeman
 */
public class ZipHandlerHook implements HandlerHook
{
	private static Log log = LogFactory.getLog(ZipHandlerHook.class);
	private HttpServletResponse response;
	private ZipExportLogic zipExporter;
	private ViewParameters viewparams;
	private AssignmentLogic assignmentLogic;

	public void setResponse(HttpServletResponse response)
	{
		this.response = response;
	}

	public void setZipExporter(ZipExportLogic zipExporter)
	{
		this.zipExporter = zipExporter;
	}

	public void setViewparams(ViewParameters viewparams)
	{
		this.viewparams = viewparams;
	}

	public void setAssignmentLogic (AssignmentLogic assignmentLogic)
	{
		this.assignmentLogic = assignmentLogic;
	}

	public boolean handle()
	{
		ZipViewParams zvp;
		if (viewparams instanceof ZipViewParams)
		{
			zvp = (ZipViewParams) viewparams;
		}
		else
		{
			return false;
		}
		log.debug("Handling zip");
		OutputStream resultsOutputStream = null;
		try
		{
			resultsOutputStream = response.getOutputStream();
		}
		catch (IOException ioe)
		{
			throw UniversalRuntimeException.accumulate(ioe,
					"Unable to get response stream for Download All Zip");
		}

		Assignment2 assignment = assignmentLogic.getAssignmentById(zvp.assignmentId);
		String title = assignment.getTitle() + "-" + assignment.getContextId();
		// replace spaces with "-" first for readability
		title = title.replaceAll(" " , "-");
		title = URLEncoder.encode(title);
		response.setHeader("Content-disposition", "inline; filename="+ title +".zip");
		response.setContentType("application/zip");

		zipExporter.getSubmissionsZip(resultsOutputStream, zvp.assignmentId);

		return true;
	}
}