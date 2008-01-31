/**********************************************************************************
*
* $Id$
*
***********************************************************************************
*
* Copyright (c) 2005 The Regents of the University of California, The MIT Corporation
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
package org.sakaiproject.assignment2.logic.test;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.sakaiproject.assignment2.dao.AssignmentDao;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.ExternalAnnouncementLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.component.section.support.IntegrationSupport;
import org.sakaiproject.component.section.support.UserManager;
import org.sakaiproject.section.api.SectionAwareness;
//import org.sakaiproject.section.api.coursemanagement.EnrollmentRecord;
//import org.sakaiproject.section.api.facade.Role;
import org.springframework.test.AbstractTransactionalSpringContextTests;
import junit.framework.Assert;


/**
 * Base class for gradebook test classes that provides the spring application
 * context.  The database used is an in-memory hsqldb by default, but this can
 * be overridden to test specific database configurations by setting the "mem"
 * system property to "false".  In the "mem=false" case, the database configuration
 * is set in the hibernate.properties file in the "hibernate.properties.dir" directory.
 *
 * @author <a href="mailto:jholtzman@berkeley.edu">Josh Holtzman</a>
 */
public abstract class Assignment2TestBase extends AbstractTransactionalSpringContextTests {
	protected AssignmentDao dao;
	protected AssignmentLogic assignmentLogic;
	protected AssignmentSubmissionLogic submissionLogic;
	protected ExternalLogic externalogic;
	protected ExternalGradebookLogic gradebookLogic;
	protected ExternalAnnouncementLogic announcementLogic;
	protected AssignmentPermissionLogic permissionLogic;
	
	protected SectionAwareness sectionAwareness;
    //protected UserDirectoryService userDirectoryService;
	protected IntegrationSupport integrationSupport;
	protected UserManager userManager;

	protected void onSetUpBeforeTransaction() throws Exception {
	
	}
    protected void onSetUpInTransaction() throws Exception {
        
        sectionAwareness = (SectionAwareness)applicationContext.getBean("org.sakaiproject.section.api.SectionAwareness");
        //userDirectoryService = (UserDirectoryService)applicationContext.getBean("org_sakaiproject_tool_gradebook_facades_UserDirectoryService");*/
        integrationSupport = (IntegrationSupport)applicationContext.getBean("org.sakaiproject.component.section.support.IntegrationSupport");
        if (integrationSupport == null) {
        	throw new NullPointerException("Sections integration support could not be retrieved from spring");
        }
        userManager = (UserManager)applicationContext.getBean("org.sakaiproject.component.section.support.UserManager");
        if (userManager == null) {
        	throw new NullPointerException("Sections userManager could not be retrieved from spring");
        }
        
    	dao = (AssignmentDao)applicationContext.getBean("org.sakaiproject.assignment2.dao.AssignmentDao");
    	if (dao == null) {
			throw new NullPointerException(
					"DAO could not be retrieved from spring");
		}
    	/*assignmentLogic = (AssignmentLogic)applicationContext.getBean("org.sakaiproject.assignment2.logic.AssignmentLogic");
    	if (assignmentLogic == null) {
			throw new NullPointerException(
					"assignmentLogic could not be retrieved from spring");
		}*/
    	
    	
    }

    /**
     * @see org.springframework.test.AbstractDependencyInjectionSpringContextTests#getConfigLocations()
     */
    protected String[] getConfigLocations() {
    	// point to the needed spring config files, must be on the classpath
		// (add component/src/webapp/WEB-INF to the build path in Eclipse),
		// they also need to be referenced in the pom.xml file
		return new String[] { "hibernate-test.xml", "spring-hibernate.xml",
				"spring-hib-sections-test.xml",

			// SectionAwareness integration support.
			//"classpath*:org/sakaiproject/component/section/support/spring-integrationSupport.xml",
			//"classpath*:org/sakaiproject/component/section/spring-services.xml",
		};

    }

    /*protected List addUsersEnrollments(Gradebook gradebook, Collection studentUids) {
		List enrollments = new ArrayList();
		for (Iterator iter = studentUids.iterator(); iter.hasNext(); ) {
			String studentUid = (String)iter.next();
			userManager.createUser(studentUid, null, null, null);
			EnrollmentRecord sectionEnrollment = (EnrollmentRecord)integrationSupport.addSiteMembership(studentUid, gradebook.getUid(), Role.STUDENT);
			enrollments.add(sectionEnrollment);
		}
		return enrollments;
	}

	public IntegrationSupport getIntegrationSupport() {
		return integrationSupport;
	}
	public void setIntegrationSupport(IntegrationSupport integrationSupport) {
		this.integrationSupport = integrationSupport;
	}
	public UserManager getUserManager() {
		return userManager;
	}
	public void setUserManager(UserManager userManager) {
		this.userManager = userManager;
	}

	protected void setAuthnId(String newUserUid) {
		if (authn instanceof AuthnTestImpl) {
			((AuthnTestImpl)authn).setAuthnContext(newUserUid);
		} else {
			throw new UnsupportedOperationException();
		}
	}*/

    /**
     * Utility method for legacy tests after the "getCourseGradeWithStats" method stopped being used
     * by the application.
     */
	/*protected CourseGrade getCourseGradeWithStats(Long gradebookId) {
    	List gradableObjects = gradebookManager.getAssignmentsAndCourseGradeWithStats(gradebookId, Assignment.DEFAULT_SORT, true);
    	for (Iterator iter = gradableObjects.iterator(); iter.hasNext(); ) {
    		GradableObject gradableObject = (GradableObject)iter.next();
    		if (gradableObject instanceof CourseGrade) {
    			return (CourseGrade)gradableObject;
    		}
    	}
    	return null;
    }

		public GradebookPermissionService getGradebookPermissionService()
		{
			return gradebookPermissionService;
		}

		public void setGradebookPermissionService(
				GradebookPermissionService gradebookPermissionService)
		{
			this.gradebookPermissionService = gradebookPermissionService;
		}*/
    
    public void testUpdateMultipleScores() throws Exception {
    	Assert.assertEquals(1, 1);
    }

}
