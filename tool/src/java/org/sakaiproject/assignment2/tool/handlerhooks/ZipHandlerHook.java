package org.sakaiproject.assignment2.tool.handlerhooks;

import java.io.IOException;
import java.io.OutputStream;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.ZipExportLogic;
import org.sakaiproject.assignment2.tool.params.ZipViewParams;
import org.sakaiproject.exception.PermissionException;

import uk.org.ponder.rsf.processor.HandlerHook;
import uk.org.ponder.rsf.viewstate.ViewParameters;
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

		response.setHeader("Content-disposition", "inline; filename=bulk_download.zip");
		response.setContentType("application/zip");

		try
		{
			zipExporter.getSubmissionsZip(resultsOutputStream, zvp.assignmentId);
		}
		catch (PermissionException pe)
		{
			throw UniversalRuntimeException.accumulate(pe, "User doesn't have permission");
		}
		return true;
	}
}