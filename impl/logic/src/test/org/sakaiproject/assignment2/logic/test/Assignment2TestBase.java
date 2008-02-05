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

import java.util.Date;

import org.sakaiproject.assignment2.dao.AssignmentDao;
import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.logic.AssignmentSubmissionLogic;
import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.ExternalAnnouncementLogic;
import org.sakaiproject.assignment2.logic.AssignmentPermissionLogic;
import org.sakaiproject.assignment2.logic.impl.AssignmentLogicImpl;
import org.sakaiproject.assignment2.logic.impl.ExternalAnnouncementLogicImpl;
import org.sakaiproject.assignment2.logic.impl.ExternalGradebookLogicImpl;
import org.sakaiproject.assignment2.logic.impl.AssignmentPermissionLogicImpl;
import org.sakaiproject.assignment2.logic.impl.AssignmentSubmissionLogicImpl;
import org.sakaiproject.assignment2.logic.test.stubs.ExternalLogicStub;

import org.sakaiproject.service.gradebook.shared.GradebookFrameworkService;
import org.sakaiproject.service.gradebook.shared.GradebookService;
import org.sakaiproject.tool.gradebook.facades.Authn;
import org.sakaiproject.component.section.support.IntegrationSupport;
import org.sakaiproject.component.section.support.UserManager;
import org.sakaiproject.section.api.SectionAwareness;
import org.springframework.test.AbstractTransactionalSpringContextTests;


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

	protected ExternalLogic externalogic;
	protected ExternalGradebookLogic gradebookLogic;
	protected ExternalAnnouncementLogic announcementLogic;
	
	// these use external logic which we have to replace with stub, so use impl
	protected AssignmentLogicImpl assignmentLogic;
	protected AssignmentSubmissionLogicImpl submissionLogic;
	protected AssignmentPermissionLogicImpl permissionLogic;
	
	protected GradebookFrameworkService gradebookFrameworkService;
	protected GradebookService gradebookService;
	protected Authn authn;
	
	
	protected static final String GB_ITEM1_NAME = "Item 1";
	protected static final Double GB_ITEM1_PTS = new Double(30);
	protected static final Date GB_ITEM1_DUE = null;
	
	protected static final String GB_ITEM2_NAME = "Item 2";
	protected static final Double GB_ITEM2_PTS = new Double(40);
	protected static final Date GB_ITEM2_DUE = new Date();
	
	protected SectionAwareness sectionAwareness;
	//protected UserDirectoryService userDirectoryService;
	protected IntegrationSupport integrationSupport;
	protected UserManager userManager;

	protected void onSetUpBeforeTransaction() throws Exception {
	
	}
    protected void onSetUpInTransaction() throws Exception {
        
        sectionAwareness = (SectionAwareness)applicationContext.getBean("org.sakaiproject.section.api.SectionAwareness");
        if (sectionAwareness == null) {
        	throw new NullPointerException("Sections integration support could not be retrieved from spring");
        }
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
    	
    	gradebookFrameworkService = (GradebookFrameworkService)applicationContext.getBean("org_sakaiproject_service_gradebook_GradebookFrameworkService");
    	if (gradebookFrameworkService == null) {
			throw new NullPointerException(
					"gradebookFrameworkService could not be retrieved from spring");
		} 
    	
    	gradebookService = (GradebookService)applicationContext.getBean("org_sakaiproject_service_gradebook_GradebookService");
    	if (gradebookService == null) {
			throw new NullPointerException(
					"gradebookService could not be retrieved from spring");
		} 
    	
    	authn = (Authn)applicationContext.getBean("org_sakaiproject_tool_gradebook_facades_Authn");
    	if (authn == null) {
			throw new NullPointerException("authn could not be retrieved from spring");
		}
    	
    	gradebookLogic = new ExternalGradebookLogicImpl();
    	announcementLogic = new ExternalAnnouncementLogicImpl();
    	
    	permissionLogic = new AssignmentPermissionLogicImpl();
    	permissionLogic.setDao(dao);
    	permissionLogic.setExternalGradebookLogic(gradebookLogic);
    	permissionLogic.setExternalLogic(new ExternalLogicStub());
    	
    	submissionLogic = new AssignmentSubmissionLogicImpl();
    	submissionLogic.setDao(dao);
    	submissionLogic.setExternalGradebookLogic(gradebookLogic);
    	submissionLogic.setExternalLogic(new ExternalLogicStub());
    	submissionLogic.setPermissionLogic(permissionLogic);
    	
    	assignmentLogic = new AssignmentLogicImpl();
    	assignmentLogic.setDao(dao);
    	assignmentLogic.setExternalAnnouncementLogic(announcementLogic);
    	assignmentLogic.setExternalGradebookLogic(gradebookLogic);
    	assignmentLogic.setExternalLogic(new ExternalLogicStub());
    	assignmentLogic.setAssignmentSubmissionLogic(submissionLogic);
	
    }

    /**
     * @see org.springframework.test.AbstractDependencyInjectionSpringContextTests#getConfigLocations()
     */
    protected String[] getConfigLocations() {
    	// point to the needed spring config files, must be on the classpath
		// (add component/src/webapp/WEB-INF to the build path in Eclipse),
		// they also need to be referenced in the pom.xml file
		return new String[] { "hibernate-test.xml", "spring-hibernate.xml",
				"spring-hib-gb-test.xml",
				"spring-hib-sections-test.xml",

		};

    }

}
