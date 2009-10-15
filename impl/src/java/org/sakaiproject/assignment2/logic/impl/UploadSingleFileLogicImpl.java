package org.sakaiproject.assignment2.logic.impl;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.vfs.FileContent;
import org.apache.commons.vfs.FileDepthSelector;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileSystemManager;
import org.apache.commons.vfs.FileType;
import org.apache.commons.vfs.VFS;
import org.sakaiproject.assignment2.exception.AssignmentNotFoundException;
import org.sakaiproject.assignment2.exception.UploadException;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalContentLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.UploadSingleFileLogic;
import org.sakaiproject.assignment2.logic.UploadGradesLogic;
import org.sakaiproject.assignment2.logic.ZipExportLogic;
import org.sakaiproject.assignment2.logic.UploadSingleFileLogic.UploadSingleFileInfo;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.FeedbackAttachment;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
import org.sakaiproject.assignment2.model.UploadAllOptions;
import org.springframework.web.multipart.MultipartFile;
import org.sakaiproject.entity.api.Reference;
import org.sakaiproject.entity.api.ResourcePropertiesEdit;
import org.sakaiproject.entity.cover.EntityManager;
import org.sakaiproject.content.api.ContentHostingService;
import org.sakaiproject.content.api.ContentResource;
import org.sakaiproject.content.api.ContentTypeImageService;
import org.sakaiproject.entity.api.ResourceProperties;
import org.sakaiproject.exception.IdInvalidException;
import org.sakaiproject.exception.IdUsedException;
import org.sakaiproject.exception.InconsistentException;
import org.sakaiproject.exception.OverQuotaException;
import org.sakaiproject.exception.PermissionException;
import org.sakaiproject.exception.ServerOverloadException;
import org.sakaiproject.util.FormattedText;
import org.sakaiproject.util.Validator;
import org.sakaiproject.authz.api.SecurityAdvisor;
import org.sakaiproject.authz.api.SecurityAdvisor.SecurityAdvice;
import org.sakaiproject.authz.api.SecurityService;

/**
 * Functionality for uploading and downloading an archive bundle
 * 
 * @author <a href="mailto:carl.hall@et.gatech.edu">Carl Hall</a>
 */
public class UploadSingleFileLogicImpl implements UploadSingleFileLogic
{
    private static final Log log = LogFactory.getLog(UploadSingleFileLogicImpl.class);

    private SecurityService securityService;
    public void setSecurityService(SecurityService securityService)
    {
        this.securityService = securityService;
    }
    
    private ExternalContentLogic externalContentLogic;
    public void setExternalContentLogic(ExternalContentLogic externalContentLogic)
    {
        this.externalContentLogic = externalContentLogic;
    }
    
    private AssignmentLogic assignmentLogic;
    public void setAssignmentLogic(AssignmentLogic assignmentLogic)
    {
        this.assignmentLogic = assignmentLogic;
    }

    private ContentHostingService contentHostingService;
    public void setContentHostingService(ContentHostingService contentHostingService)
    {
        this.contentHostingService = contentHostingService;
    }
    
    private AssignmentSubmissionLogic assignmentSubmissionLogic;
    public void setAssignmentSubmissionLogic(
            AssignmentSubmissionLogic assignmentSubmissionLogic) {
        this.assignmentSubmissionLogic = assignmentSubmissionLogic;
    }

    private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic)
    {
        this.externalLogic = externalLogic;
    }

    public List<String> uploadSingleFile(Assignment2 assignment, AssignmentSubmission assignmentSubmission, AssignmentSubmissionVersion studentSubmissionPreviewVersion, AssignmentSubmissionVersion asv, Map<String, MultipartFile> uploads) throws UploadException
    {
    	List<String> uploadErrors = new Vector<String>();
    	
        MultipartFile uploadedFile = uploads.get("file");

        long uploadedFileSize = uploadedFile.getSize();
        if (uploadedFileSize == 0)
        {
        	// reject on empty upload file
        	addToUploadInfoList(uploadErrors, UploadSingleFileInfo.EMPTY_FILE);
        }
        else
        {
	        // double check that the file doesn't exceed our upload limit
	        int maxFileSizeInMB = externalContentLogic.getMaxUploadFileSizeInMB();
	        int maxFileSizeInBytes = maxFileSizeInMB * 1024 * 1024;
	
	        if (uploadedFileSize > maxFileSizeInBytes) {
	        	// reject on giant upload file
	        	addToUploadInfoList(uploadErrors, UploadSingleFileInfo.EMPTY_FILE);
	        }
	        else
	        {
				if (uploadedFile.getOriginalFilename() == null || uploadedFile.getOriginalFilename().length() == 0)
				{
					addToUploadInfoList(uploadErrors, UploadSingleFileInfo.EMPTY_FILE_TITLE);
				}
			    else if (uploadedFile.getOriginalFilename().length() > 0)
				{
					String filename = Validator.getFileName(uploadedFile.getOriginalFilename());
					try
					{
						byte[] bytes = uploadedFile.getBytes();
						String contentType = uploadedFile.getContentType();
				
						if(bytes.length >= maxFileSizeInBytes)
						{
							addToUploadInfoList(uploadErrors, UploadSingleFileInfo.EMPTY_FILE);
						}
						else if(bytes.length > 0)
						{
							Set<SubmissionAttachment> attachments = new HashSet<SubmissionAttachment>();
							
							// we just want the file name part - strip off any drive and path stuff
							String name = filename;
							String resourceId = Validator.escapeResourceName(name);
				
							// make a set of properties to add for the new resource
							ResourcePropertiesEdit props = contentHostingService.newResourceProperties();
							props.addProperty(ResourceProperties.PROP_DISPLAY_NAME, name);
							props.addProperty(ResourceProperties.PROP_DESCRIPTION, filename);
				
							// make an attachment resource for this URL
							try
							{
								String siteId = externalLogic.getCurrentContextId();
				
								String toolName = externalLogic.getToolTitle();
								
								// enable security advisor
								enableSecurityAdvisor();
								// add attachment
								ContentResource attachment = contentHostingService.addAttachmentResource(resourceId, siteId, toolName, contentType, bytes, props);
								// remove security advisors
								disableSecurityAdvisors();
								
								Reference ref = EntityManager.newReference(contentHostingService.getReference(attachment.getId()));
								SubmissionAttachment sAttachment = new SubmissionAttachment();
								sAttachment.setAttachmentReference(ref.getId());
								sAttachment.setSubmissionVersion(asv);
								attachments.add(sAttachment);
							
							}
							catch (PermissionException e)
							{
								addToUploadInfoList(uploadErrors, UploadSingleFileInfo.NO_PERMISSION_TO_ADD_ATTACHMENT);
								log.warn(this + ".uploadSingleFile PermissionException" + e.getMessage());
							}
							catch(RuntimeException e)
							{
								if(contentHostingService.ID_LENGTH_EXCEPTION.equals(e.getMessage()))
								{
									addToUploadInfoList(uploadErrors, UploadSingleFileInfo.FILE_NAME_TOO_LONG);
								}
								else
								{
									addToUploadInfoList(uploadErrors, UploadSingleFileInfo.RUNTIME_EXCEPTION);
								}
								log.warn(this + ".uploadSingleFile RuntimeException" + e.getMessage());
							}
							
					        //save the submission
					        assignmentSubmissionLogic.saveStudentSubmission(assignmentSubmission.getUserId(), assignment, false, 
					             "", attachments, true);
						}
					}
					catch (IOException ioException)
					{
						addToUploadInfoList(uploadErrors, UploadSingleFileInfo.IO_EXCEPTION);
						log.warn(this + ".uploadSingleFile ioException" + ioException.getMessage());
					}
					catch (Exception exception)
					{
						addToUploadInfoList(uploadErrors, UploadSingleFileInfo.GENERAL_EXCEPTION);
						log.warn(this + ".uploadSingleFile " + exception.getMessage());
					}
				}
	        }
        }
        
        return uploadErrors;
    }

    /**
     * update the error list
     * @param infoList
     * @param info
     */
    private void addToUploadInfoList(List<String> infoList, UploadSingleFileInfo info) {
        if (infoList == null)
        {
        	infoList= new Vector<String>();
        }
        infoList.add(info.toString());
    }
    
    protected void disableSecurityAdvisors()
    {
    	// remove all security advisors
    	securityService.clearAdvisors();
    }

    /**
     * Establish a security advisor to allow the "embedded" azg work to occur
     * with no need for additional security permissions.
     */
    protected void enableSecurityAdvisor()
    {
      // put in a security advisor so we can create citationAdmin site without need
      // of further permissions
      securityService.pushAdvisor(new SecurityAdvisor() {
        public SecurityAdvice isAllowed(String userId, String function, String reference)
        {
          return SecurityAdvice.ALLOWED;
        }
      });
    }
    
}