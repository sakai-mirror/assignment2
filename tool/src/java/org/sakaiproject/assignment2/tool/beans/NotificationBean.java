package org.sakaiproject.assignment2.tool.beans;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.model.AssignmentSubmissionVersion;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;
import org.sakaiproject.authz.api.AuthzGroup;
import org.sakaiproject.authz.api.AuthzGroupService;
import org.sakaiproject.authz.api.SecurityService;
import org.sakaiproject.component.api.ServerConfigurationService;
import org.sakaiproject.content.api.ContentHostingService;
import org.sakaiproject.content.api.ContentResource;
import org.sakaiproject.email.api.DigestService;
import org.sakaiproject.email.api.EmailService;
import org.sakaiproject.entity.api.Entity;
import org.sakaiproject.entity.api.ResourceProperties;
import org.sakaiproject.exception.IdUnusedException;
import org.sakaiproject.exception.PermissionException;
import org.sakaiproject.exception.TypeException;
import org.sakaiproject.site.api.Site;
import org.sakaiproject.site.api.SiteService;
import org.sakaiproject.time.api.Time;
import org.sakaiproject.time.api.TimeService;
import org.sakaiproject.user.api.User;
import org.sakaiproject.user.api.UserDirectoryService;
import org.sakaiproject.user.api.UserNotDefinedException;
import org.sakaiproject.util.StringUtil;
import org.sakaiproject.util.Validator;

import uk.org.ponder.messageutil.MessageLocator;

public class NotificationBean
{
	private static final Log log = LogFactory.getLog(NotificationBean.class);

	private static final String MULTIPART_BOUNDARY = "======sakai-multi-part-boundary======";
	private static final String BOUNDARY_LINE = "\n\n--" + MULTIPART_BOUNDARY + "\n";
	private static final String TERMINATION_LINE = "\n\n--" + MULTIPART_BOUNDARY + "--\n\n";
	private static final String MIME_ADVISORY = "This message is for MIME-compliant mail readers.";

	/** This string starts the references to resources in this service. */
	public static final String REFERENCE_ROOT = "/assignment";
	String relativeAccessPoint = null;

	// injected dependencies
	private MessageLocator messageLocator;
	private ContentHostingService contentHostingService;
	private SecurityService securityService;
	private TimeService timeService;
	private DigestService digestService;
	private AuthzGroupService authzGroupService;
	private SiteService siteService;
	private EmailService emailService;
	private ServerConfigurationService serverConfigurationService;
	private UserDirectoryService userDirectoryService;

	public void setMessageLocator(MessageLocator messageLocator)
	{
		this.messageLocator = messageLocator;
	}

	public void setContentHostingService(ContentHostingService contentHostingService)
	{
		this.contentHostingService = contentHostingService;
	}

	public void setSecurityService(SecurityService securityService)
	{
		this.securityService = securityService;
	}

	public void setTimeService(TimeService timeService)
	{
		this.timeService = timeService;
	}

	public void setDigestService(DigestService digestService)
	{
		this.digestService = digestService;
	}

	public void setAuthzGroupService(AuthzGroupService authzGroupService)
	{
		this.authzGroupService = authzGroupService;
	}

	public void setSiteService(SiteService siteService)
	{
		this.siteService = siteService;
	}

	public void setEmailService(EmailService emailService)
	{
		this.emailService = emailService;
	}

	public void setServerConfigurationService(ServerConfigurationService serverConfigurationService)
	{
		this.serverConfigurationService = serverConfigurationService;
	}

	public void setUserDirectoryService(UserDirectoryService userDirectoryService)
	{
		this.userDirectoryService = userDirectoryService;
	}

	/**
	 * Final initialization, once all dependencies are set.
	 */
	public void init()
	{
		relativeAccessPoint = REFERENCE_ROOT;
		log.info("init()");
	}

	/**
	 * send notification to student if necessary
	 * 
	 * @param s
	 */
	public void notificationToStudent(AssignmentSubmission s) throws IdUnusedException,
			UserNotDefinedException, PermissionException, TypeException
	{
		if (serverConfigurationService.getBoolean("assignment.submission.confirmation.email", true))
		{
			// send notification
			User u = userDirectoryService.getCurrentUser();

			if (StringUtil.trimToNull(u.getEmail()) != null)
			{
				ArrayList<User> receivers = new ArrayList<User>();
				receivers.add(u);

				emailService.sendToUsers(receivers, buildHeaders(u.getEmail()),
						buildNotificationMessage(s));
			}
		}
	}

	public void notificationToInstructors(AssignmentSubmission s, Assignment2 a)
			throws IdUnusedException, UserNotDefinedException, PermissionException,
			IdUnusedException, TypeException
	{
		int notiType = a.getNotificationType();
		if (notiType != AssignmentConstants.NOTIFY_NONE)
		{
			// need to send notification email
			String context = a.getContextId();

			// compare the list of users with the receive.notifications and list of users who can
			// actually grade this assignment
			List<User> receivers = allowReceiveSubmissionNotificationUsers(context);
			List<User> allowGradeAssignmentUsers = allowGradeAssignmentUsers(a);
			receivers.retainAll(allowGradeAssignmentUsers);

			String submitterId = s.getUserId();

			// filter out users who's not able to grade this submission
			ArrayList<User> finalReceivers = new ArrayList<User>();

			HashSet<String> receiverSet = new HashSet<String>();
			Set<AssignmentGroup> groups = a.getAssignmentGroupSet();
			if (groups.size() > 0)
			{
				for (AssignmentGroup g : groups)
				{
					try
					{
						AuthzGroup aGroup = authzGroupService.getAuthzGroup(g.getGroupId());
						if (aGroup.isAllowed(submitterId,
								AssignmentConstants.SECURE_ADD_ASSIGNMENT_SUBMISSION))
						{
							for (User rUser : receivers)
							{
								String rUserId = rUser.getId();
								if (!receiverSet.contains(rUserId)
										&& aGroup
												.isAllowed(
														rUserId,
														AssignmentConstants.SECURE_GRADE_ASSIGNMENT_SUBMISSION))
								{
									finalReceivers.add(rUser);
									receiverSet.add(rUserId);
								}
							}
						}
					}
					catch (Exception e)
					{
						log.warn("notificationToInstructors, group id =" + g);
					}
				}
			}
			else
			{
				finalReceivers.addAll(receivers);
			}

			String messageBody = buildNotificationMessage(s);

			if (notiType == AssignmentConstants.NOTIFY_FOR_EACH)
			{
				// send the message immidiately
				emailService.sendToUsers(finalReceivers, buildHeaders(null), messageBody);
			}
			else if (notiType == AssignmentConstants.NOTIFY_DAILY_SUMMARY)
			{
				// digest the message to each user
				for (User user : finalReceivers)
				{
					digestService.digest(user.getId(), buildSubject(), messageBody);
				}
			}
		}
	}

	/**
	 * Get the List of Users who can grade submission for this assignment.
	 * 
	 * @param assignmentReference -
	 *            a reference to an assignment
	 * @return the List (User) of users who can grade submission for this assignment.
	 */
	private List<User> allowGradeAssignmentUsers(Assignment2 assignment)
	{
		return allowAssignmentFunctionUsers(assignment,
				AssignmentConstants.SECURE_GRADE_ASSIGNMENT_SUBMISSION);
	}

	/**
	 * Get the list of Users who can perform certain functions on this assignment
	 * 
	 * @inheritDoc
	 */
	private List<User> allowAssignmentFunctionUsers(Assignment2 assignment, String function)
	{
		String resourceString = getAccessPoint(true) + Entity.SEPARATOR + "a" + Entity.SEPARATOR
				+ assignment.getContextId() + Entity.SEPARATOR + assignment.getId().toString();
		List<User> rv = securityService.unlockUsers(function, resourceString);

		// get the list of users who have SECURE_ALL_GROUPS
		List<User> allGroupUsers = new ArrayList<User>();
		String contextRef = siteService.siteReference(assignment.getContextId());
		allGroupUsers = securityService.unlockUsers(AssignmentConstants.SECURE_ALL_GROUPS,
				contextRef);
		// remove duplicates
		allGroupUsers.removeAll(rv);

		// combine two lists together
		rv.addAll(allGroupUsers);

		return rv;
	}

	private List<User> allowReceiveSubmissionNotificationUsers(String context)
	{
		String resourceString = getAccessPoint(true) + Entity.SEPARATOR + "a" + Entity.SEPARATOR
				+ context + Entity.SEPARATOR;
		if (log.isDebugEnabled())
		{
			log.debug("Entering allowReceiveSubmissionNotificationUsers with resource string : "
					+ resourceString + "; context string : " + context);
		}
		List<User> users = securityService.unlockUsers(
				AssignmentConstants.SECURE_ASSIGNMENT_RECEIVE_NOTIFICATIONS, resourceString);
		return users;
	}

	/**
	 * Access the partial URL that forms the root of resource URLs.
	 * 
	 * @param relative -
	 *            if true, form within the access path only (i.e. starting with /msg)
	 * @return the partial URL that forms the root of resource URLs.
	 */
	private String getAccessPoint(boolean relative)
	{
		return (relative ? "" : serverConfigurationService.getAccessUrl()) + relativeAccessPoint;

	}

	private List<String> buildHeaders(String receiverEmail)
	{
		ArrayList<String> rv = new ArrayList<String>();

		rv.add("MIME-Version: 1.0");
		rv.add("Content-Type: multipart/alternative; boundary=\"" + MULTIPART_BOUNDARY + "\"");
		// set the subject
		rv.add(buildSubject());

		// from
		rv.add(buildFrom());

		// to
		if (StringUtil.trimToNull(receiverEmail) != null)
			rv.add("To: " + receiverEmail);

		return rv;
	}

	private String buildSubject()
	{
		return messageLocator.getMessage("noti.subject.label") + " "
				+ messageLocator.getMessage("noti.subject.content");
	}

	private String buildFrom()
	{
		return "From: " + "\"" + serverConfigurationService.getString("ui.service", "Sakai")
				+ "\"<no-reply@" + serverConfigurationService.getServerName() + ">";
	}

	/**
	 * Get the message for the email.
	 * 
	 * @param event
	 *            The event that matched criteria to cause the notification.
	 * @return the message for the email.
	 */
	private String buildNotificationMessage(AssignmentSubmission s) throws IdUnusedException,
			UserNotDefinedException, PermissionException, TypeException
	{
		StringBuilder message = new StringBuilder();
		message.append(MIME_ADVISORY);
		message.append(BOUNDARY_LINE);
		message.append(plainTextHeaders());
		message.append(plainTextContent(s));
		message.append(BOUNDARY_LINE);
		message.append(htmlHeaders());
		message.append(htmlPreamble());
		message.append(htmlContent(s));
		message.append(htmlEnd());
		message.append(TERMINATION_LINE);
		return message.toString();
	}

	private String plainTextHeaders()
	{
		return "Content-Type: text/plain\n\n";
	}

	private String plainTextContent(AssignmentSubmission s) throws IdUnusedException,
			UserNotDefinedException, PermissionException, IdUnusedException, TypeException
	{
		return htmlContent(s);
	}

	private String htmlHeaders()
	{
		return "Content-Type: text/html\n\n";
	}

	private String htmlPreamble()
	{
		StringBuilder buf = new StringBuilder();
		buf.append("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n");
		buf.append("    \"http://www.w3.org/TR/html4/loose.dtd\">\n");
		buf.append("<html>\n");
		buf.append("  <head><title>");
		buf.append(buildSubject());
		buf.append("</title></head>\n");
		buf.append("  <body>\n");
		return buf.toString();
	}

	private String htmlEnd()
	{
		return "\n  </body>\n</html>\n";
	}

	private String htmlContent(AssignmentSubmission s) throws IdUnusedException,
			UserNotDefinedException, PermissionException, TypeException
	{
		String newline = "<br />\n";

		Assignment2 a = s.getAssignment();

		String context = a.getContextId();

		Site site = siteService.getSite(context);
		String siteTitle = site.getTitle();
		String siteId = site.getId();

		StringBuffer buffer = new StringBuffer();
		// site title and id
		buffer.append(messageLocator.getMessage("noti.site.title") + " " + siteTitle + newline);
		buffer.append(messageLocator.getMessage("noti.site.id") + " " + siteId + newline + newline);
		// assignment title and due date
		buffer.append(messageLocator.getMessage("noti.assignment") + " " + a.getTitle() + newline);
		Time time = timeService.newTime();
		if (a.getDueDate() != null)
		{
			time.setTime(a.getDueDate().getTime());
			buffer.append(messageLocator.getMessage("noti.assignment.duedate") + " "
				+ time.toStringLocalFull() + newline + newline);
		}
		// submitter name and id
		AssignmentSubmissionVersion curSubVers = s.getCurrentSubmissionVersion();
		String submitterId = curSubVers.getCreatedBy();
		User submitter = userDirectoryService.getUser(submitterId);
		buffer.append(messageLocator.getMessage("noti.student") + " " + submitter.getDisplayName());
		buffer.append("( " + submitter.getDisplayId() + " )");
		buffer.append(newline + newline);

		// submit time
		buffer.append(messageLocator.getMessage("noti.submit.id") + " " + s.getId() + newline);

		// submit time
		time.setTime(curSubVers.getSubmittedTime().getTime());
		buffer.append(messageLocator.getMessage("noti.submit.time") + " "
				+ time.toStringLocalFull() + newline + newline);

		// submit text
		String text = StringUtil.trimToNull(curSubVers.getSubmittedText());
		if (text != null)
		{
			buffer.append(messageLocator.getMessage("noti.submit.text") + newline + newline
					+ Validator.escapeHtmlFormattedText(text) + newline + newline);
		}

		// attachment if any
		Set<SubmissionAttachment> attachments = curSubVers.getSubmissionAttachSet();
		if (attachments != null && attachments.size() > 0)
		{
			buffer.append(messageLocator.getMessage("noti.submit.attachments") + newline + newline);
			for (SubmissionAttachment attachment : attachments)
			{
				String ref = attachment.getAttachmentReference();
				ContentResource res = contentHostingService.getResource(ref);
				ResourceProperties resProps = res.getProperties();
				buffer.append(resProps.getProperty(ResourceProperties.PROP_DISPLAY_NAME) + "("
						+ res.getContentLength() + ")\n");
			}
		}

		return buffer.toString();
	}
}