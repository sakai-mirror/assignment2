package org.sakaiproject.assignment2.tool.beans;

import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.exception.SubmissionClosedException;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ScheduledNotification;
import org.sakaiproject.content.api.ContentHostingService;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
import org.sakaiproject.assignment2.tool.WorkFlowResult;
import org.sakaiproject.authz.api.SecurityAdvisor;
import org.sakaiproject.authz.api.SecurityAdvisor.SecurityAdvice;
import org.sakaiproject.authz.cover.SecurityService;
import org.springframework.web.multipart.MultipartFile;
import org.sakaiproject.component.cover.ComponentManager;
import org.sakaiproject.component.api.ServerConfigurationService;
import org.sakaiproject.content.api.ContentResource;
import org.sakaiproject.content.api.ResourceTypeRegistry;
import org.sakaiproject.entity.api.Reference;
import org.sakaiproject.entity.api.ResourceProperties;
import org.sakaiproject.entity.api.ResourcePropertiesEdit;
import org.sakaiproject.entity.cover.EntityManager;
import org.sakaiproject.exception.PermissionException;
import org.sakaiproject.tool.api.ToolSession;
import org.sakaiproject.tool.cover.SessionManager;
import org.sakaiproject.tool.cover.ToolManager;
import org.sakaiproject.util.Validator;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.messageutil.TargettedMessage;
import uk.org.ponder.messageutil.TargettedMessageList;



public class StudentSubmissionBean {

    private static final Log log = LogFactory.getLog(StudentSubmissionBean.class);
    
    // Service Application Scope Dependency
    private AssignmentSubmissionLogic submissionLogic;
    public void setSubmissionLogic(AssignmentSubmissionLogic submissionLogic) {
        this.submissionLogic = submissionLogic;
    }

    // Service Application Scope Dependency
    private AssignmentLogic assignmentLogic;
    public void setAssignmentLogic(AssignmentLogic assignmentLogic) {
        this.assignmentLogic = assignmentLogic;
    }

    // Service Application Scope Dependency
    private ScheduledNotification scheduledNotification;
    public void setScheduledNotification(ScheduledNotification scheduledNotification) {
        this.scheduledNotification = scheduledNotification;
    }

    // Flow Scope Bean for Student Submission
    private StudentSubmissionVersionFlowBean studentSubmissionVersionFlowBean;
    public void setStudentSubmissionVersionFlowBean(StudentSubmissionVersionFlowBean studentSubmissionVersionFlowBean) {
        this.studentSubmissionVersionFlowBean = studentSubmissionVersionFlowBean;
    }

    // Property Binding
    public Long assignmentId;

    // Property Binding
    private Boolean honorPledge;
    public void setHonorPledge(Boolean honorPledge) {
        this.honorPledge = honorPledge;
    }

    // Property Binding
    public String ASOTPKey;
    public String ASVOTPKey;

    // Request Scope Dependency
    private TargettedMessageList messages;
    public void setMessages(TargettedMessageList messages) {
        this.messages = messages;
    }

    // Request Scope Dependency (sort of even though it's declared in App Scope)
    private EntityBeanLocator asEntityBeanLocator;
    @SuppressWarnings("unchecked")
    public void setAssignmentSubmissionEntityBeanLocator(EntityBeanLocator entityBeanLocator) {
        this.asEntityBeanLocator = entityBeanLocator;
    }
    

    private Map<String, MultipartFile> uploads;
    public void setMultipartMap(Map<String, MultipartFile> uploads)
    {
        this.uploads = uploads;
    }
    
    private ContentHostingService contentHostingService = (ContentHostingService) ComponentManager.get(ContentHostingService.class.getName());
   
    private ServerConfigurationService serverConfigurationService = (ServerConfigurationService) ComponentManager.get(ServerConfigurationService.class.getName());


    /*
     * STUDENT FUNCTIONS
     */
    public WorkFlowResult processActionSubmit(){
        if (assignmentId == null ) {
            return WorkFlowResult.STUDENT_SUBMISSION_FAILURE;
        }

        AssignmentSubmission assignmentSubmission = (AssignmentSubmission) asEntityBeanLocator.locateBean(ASOTPKey);
        Assignment2 assignment = assignmentLogic.getAssignmentById(assignmentId);

        AssignmentSubmissionVersion asv = studentSubmissionVersionFlowBean.getAssignmentSubmissionVersion();

        //check whether honor pledge was added if required
        if (assignment.isHonorPledge() && !(this.honorPledge != null && Boolean.TRUE.equals(honorPledge))) {
            messages.addMessage(new TargettedMessage("assignment2.student-submit.error.honor_pledge_required",
                    new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_ERROR));
            return WorkFlowResult.STUDENT_SUBMISSION_FAILURE;
        }else {

            submissionLogic.saveStudentSubmission(assignmentSubmission.getUserId(), assignment, false, 
                    asv.getSubmittedText(), asv.getSubmissionAttachSet(), true);

            // just in case submission closed while the student was working on
            // it, double check that the current submission isn't still
            // draft before we take the "success" actions. if the submission was
            // closed when they hit "submit", the backend saved their submission as draft
            AssignmentSubmission newSubmission = submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(assignmentId, assignmentSubmission.getUserId());

            if (!newSubmission.getCurrentSubmissionVersion().isDraft()) {
                // add a sucess message.  the message will change depending on 
                // if this submission is late or not
                if (assignment.getDueDate() != null && assignment.getDueDate().before(new Date())) {
                    messages.addMessage(new TargettedMessage("assignment2.student-submit.info.submission_submitted.late",
                            new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_INFO));
                } else {
                    messages.addMessage(new TargettedMessage("assignment2.student-submit.info.submission_submitted",
                            new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_INFO));
                }

                // Send out notifications
                if (assignment.isSendSubmissionNotifications()) {                 
                    scheduledNotification.notifyInstructorsOfSubmission(newSubmission);
                }

                // students always get a notification
                scheduledNotification.notifyStudentThatSubmissionWasAccepted(newSubmission);
            } else {
                messages.addMessage(new TargettedMessage("assignment2.student-submit.error.submission_save_draft",
                        new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_ERROR));
                return WorkFlowResult.STUDENT_SUBMISSION_FAILURE;
            }
        }

        return WorkFlowResult.STUDENT_SUBMIT_SUBMISSION;
    }
    
    public WorkFlowResult processActionPreview(){
        // save this submission as draft if submission is closed so we don't
        // lose the student's work. this may happen if the user was working
        // on their submission in the UI when the assignment closed and then
        // the student hit "preview"
        Assignment2 assignment = assignmentLogic.getAssignmentById(assignmentId);
        if (assignmentId != null) {
            AssignmentSubmission assignmentSubmission = (AssignmentSubmission) asEntityBeanLocator.locateBean(ASOTPKey);
            assignmentSubmission.setAssignment(assignment);

            if (!submissionLogic.isSubmissionOpenForStudentForAssignment(assignmentSubmission.getUserId(), assignmentId)) {
                AssignmentSubmissionVersion asv = (AssignmentSubmissionVersion) studentSubmissionVersionFlowBean.getAssignmentSubmissionVersion();

                submissionLogic.saveStudentSubmission(assignmentSubmission.getUserId(),
                        assignmentSubmission.getAssignment(), true, asv.getSubmittedText(),
                        asv.getSubmissionAttachSet(), true);
            }
        }

        return WorkFlowResult.STUDENT_PREVIEW_SUBMISSION;
    }

    public WorkFlowResult processActionBackToEdit() {
        return WorkFlowResult.STUDENT_CONTINUE_EDITING_SUBMISSION;
    }

    public WorkFlowResult processActionSaveDraft() {
        Assignment2 assignment = assignmentLogic.getAssignmentById(assignmentId);
        if (assignmentId == null){
            return WorkFlowResult.STUDENT_SUBMISSION_FAILURE;
        }

        AssignmentSubmission assignmentSubmission = (AssignmentSubmission) asEntityBeanLocator.locateBean(ASOTPKey);
        assignmentSubmission.setAssignment(assignment);

        AssignmentSubmissionVersion asv = (AssignmentSubmissionVersion) studentSubmissionVersionFlowBean.getAssignmentSubmissionVersion();

        asv.setAssignmentSubmission(assignmentSubmission);
        asv.setDraft(Boolean.TRUE);

        try {
            submissionLogic.saveStudentSubmission(assignmentSubmission.getUserId(),
                    assignmentSubmission.getAssignment(), true, asv.getSubmittedText(),
                    asv.getSubmissionAttachSet(), true);
            messages.addMessage(new TargettedMessage("assignment2.student-submit.info.submission_save_draft",
                    new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_INFO));
        } catch (SubmissionClosedException sce) {
            messages.addMessage(new TargettedMessage("assignment2.student-submit.error.submission_closed",
                    new Object[] {}, TargettedMessage.SEVERITY_ERROR));
            return WorkFlowResult.STUDENT_SUBMISSION_FAILURE;
        }

        return WorkFlowResult.STUDENT_SAVE_DRAFT_SUBMISSION;
    }

    public WorkFlowResult processActionCancel() {
        return WorkFlowResult.STUDENT_CANCEL_SUBMISSION;
    }
    
    /**
     * For single file update only
     * @return
     */
    public WorkFlowResult processActionSingleFileUploadSubmit()
    {
        if (assignmentId == null){
            return WorkFlowResult.STUDENT_SUBMISSION_FAILURE;
        }
    	
        Assignment2 assignment = assignmentLogic.getAssignmentById(assignmentId);
        // add submission
        AssignmentSubmission assignmentSubmission = (AssignmentSubmission) asEntityBeanLocator.locateBean(ASOTPKey);
        AssignmentSubmissionVersion studentSubmissionPreviewVersion = (AssignmentSubmissionVersion) studentSubmissionVersionFlowBean.locateBean(ASVOTPKey);
		AssignmentSubmissionVersion asv = studentSubmissionVersionFlowBean.getAssignmentSubmissionVersion();
		Set<SubmissionAttachment> attachments = new HashSet<SubmissionAttachment>();

    	MultipartFile uploadedFile = uploads.get("file");

        long uploadedFileSize = uploadedFile.getSize();
        if (uploadedFileSize == 0)
        {
            messages.addMessage(new TargettedMessage("assignment2.student-submission.single-uploaded-file.alert.choose_file", new Object[] {},
                        TargettedMessage.SEVERITY_ERROR));
            return WorkFlowResult.STUDENT_SUBMISSION_FAILURE;
        }
        
        // double check that the file doesn't exceed our upload limit
        String maxFileSizeInMB = serverConfigurationService.getString("content.upload.max", "1");
        int maxFileSizeInBytes = 1024 * 1024;
        try {
            maxFileSizeInBytes = Integer.parseInt(maxFileSizeInMB) * maxFileSizeInBytes;
        } catch(NumberFormatException e) {
            log.warn("Unable to parse content.upload.max retrieved from properties file during upload");
        }

        if (uploadedFileSize > maxFileSizeInBytes) {
            messages.addMessage(new TargettedMessage("assignment2.uploadall.error.file_size", new Object[] {maxFileSizeInMB}, TargettedMessage.SEVERITY_ERROR));
            return WorkFlowResult.STUDENT_SUBMISSION_FAILURE;
        }
        
		if (uploadedFile.getOriginalFilename() == null || uploadedFile.getOriginalFilename().length() == 0)
		{
			messages.addMessage(new TargettedMessage("assignment2.uploadall.error.file_size", new Object[] {maxFileSizeInMB}, TargettedMessage.SEVERITY_ERROR));
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
					messages.addMessage(new TargettedMessage("assignment2.student-submission.single-uploaded-file.alert.big_file", new Object[]{ maxFileSizeInBytes }, TargettedMessage.SEVERITY_ERROR));
					// addAlert(state, hrb.getString("size") + " " + max_file_size_mb + "MB " + hrb.getString("exceeded2"));
				}
				else if(bytes.length > 0)
				{
					// we just want the file name part - strip off any drive and path stuff
					String name = Validator.getFileName(filename);
					String resourceId = Validator.escapeResourceName(name);
		
					// make a set of properties to add for the new resource
					ResourcePropertiesEdit props = contentHostingService.newResourceProperties();
					props.addProperty(ResourceProperties.PROP_DISPLAY_NAME, name);
					props.addProperty(ResourceProperties.PROP_DESCRIPTION, filename);
		
					// make an attachment resource for this URL
					try
					{
						String siteId = ToolManager.getCurrentPlacement().getContext();
		
						String toolName = "Assignment";
						
						// add attachment
						enableSecurityAdvisor();
						ContentResource attachment = contentHostingService.addAttachmentResource(resourceId, siteId, toolName, contentType, bytes, props);
						disableSecurityAdvisors();
						
						try
						{
							Reference ref = EntityManager.newReference(contentHostingService.getReference(attachment.getId()));
							SubmissionAttachment sAttachment = new SubmissionAttachment();
							sAttachment.setAttachmentReference(ref.getId());
							sAttachment.setSubmissionVersion(asv);
							
							// only one attachment/per student is needed for this pupose
							attachments.add(sAttachment);
						}
						catch(Exception ee)
					    {
							log.warn(this + "processActionSingleFileUploadSubmit cannot find reference for " + attachment.getId() + ee.getMessage());
						}
						
						
					}
					catch (PermissionException e)
					{
						messages.addMessage(new TargettedMessage("assignment2.student-submission.single-uploaded-file.alert.notpermis4", new Object[]{name}, TargettedMessage.SEVERITY_ERROR));
					}
					catch(RuntimeException e)
					{
						if(contentHostingService.ID_LENGTH_EXCEPTION.equals(e.getMessage()))
						{
							// couldn't we just truncate the resource-id instead of rejecting the upload?
							messages.addMessage(new TargettedMessage("assignment2.student-submission.single-uploaded-file.alert.toolong", new Object[]{name}, TargettedMessage.SEVERITY_ERROR));
						}
						else
						{
							log.debug(this + ".processActionSingleFileUploadSubmit ***** Runtime Exception ***** " + e.getMessage());
							messages.addMessage(new TargettedMessage("assignment2.student-submission.single-uploaded-file.alert.failed", new Object[]{name}, TargettedMessage.SEVERITY_ERROR));
						}
					}
					catch(Exception ignore)
					{
						// other exceptions should be caught earlier
						log.debug(this + ".processActionSingleFileUploadSubmit ***** Unknown Exception ***** " + ignore.getMessage());
					}
					
			        //save the submission
			        submissionLogic.saveStudentSubmission(assignmentSubmission.getUserId(), assignment, false, 
			             "", attachments, true);
				}
			}
			catch (IOException ioException)
			{
				log.debug(this + ".processActionSingleFileUploadSubmit ioException" + ioException.getMessage());
			}
			catch (Exception exception)
			{
				log.debug(this + ".processActionSingleFileUploadSubmit " + exception.getMessage());
			}
		}
			

        // just in case submission closed while the student was working on
        // it, double check that the current submission isn't still
        // draft before we take the "success" actions. if the submission was
        // closed when they hit "submit", the back end saved their submission as draft
        AssignmentSubmission newSubmission = submissionLogic.getCurrentSubmissionByAssignmentIdAndStudentId(assignmentId, assignmentSubmission.getUserId());

        if (!newSubmission.getCurrentSubmissionVersion().isDraft()) {
            // add a success message.  the message will change depending on 
            // if this submission is late or not
            if (assignment.getDueDate() != null && assignment.getDueDate().before(new Date())) {
                messages.addMessage(new TargettedMessage("assignment2.student-submit.info.submission_submitted.late",
                        new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_INFO));
            } else {
                messages.addMessage(new TargettedMessage("assignment2.student-submit.info.submission_submitted",
                        new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_INFO));
            }

            // Send out notifications
            if (assignment.isSendSubmissionNotifications()) {                 
                scheduledNotification.notifyInstructorsOfSubmission(newSubmission);
            }

            // students always get a notification
            scheduledNotification.notifyStudentThatSubmissionWasAccepted(newSubmission);
        } else {
        	  messages.addMessage(new TargettedMessage("assignment2.student-submit.error.submission_save_draft",
                      new Object[] { assignment.getTitle() }, TargettedMessage.SEVERITY_ERROR));
              return WorkFlowResult.STUDENT_SUBMISSION_FAILURE;
        }
    	return WorkFlowResult.STUDENT_SUBMIT_SUBMISSION;
    }
    
    // cancel submission
    public WorkFlowResult processActionSingleFileUploadCancel()
    {
    	return WorkFlowResult.STUDENT_CANCEL_SUBMISSION;
    }
    
    /**
     * remove all security advisors
     */
    protected void disableSecurityAdvisors()
    {
    	// remove all security advisors
    	SecurityService.clearAdvisors();
    }

    /**
     * Establish a security advisor to allow the "embedded" azg work to occur
     * with no need for additional security permissions.
     */
    protected void enableSecurityAdvisor()
    {
      // put in a security advisor so we can create citationAdmin site without need
      // of further permissions
      SecurityService.pushAdvisor(new SecurityAdvisor() {
        public SecurityAdvice isAllowed(String userId, String function, String reference)
        {
          return SecurityAdvice.ALLOWED;
        }
      });
    }

}
