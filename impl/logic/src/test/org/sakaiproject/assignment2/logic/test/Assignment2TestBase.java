/**********************************************************************************
 * $URL$
 * $Id$
 ***********************************************************************************
 *
 * Copyright (c) 2007 The Sakai Foundation.
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
import java.util.Iterator;
import java.util.Set;

import org.sakaiproject.assignment2.dao.AssignmentDao;
import org.sakaiproject.assignment2.logic.ExternalAnnouncementLogic;
import org.sakaiproject.assignment2.logic.impl.AssignmentLogicImpl;
import org.sakaiproject.assignment2.logic.impl.ExternalAnnouncementLogicImpl;
import org.sakaiproject.assignment2.logic.impl.ExternalGradebookLogicImpl;
import org.sakaiproject.assignment2.logic.impl.AssignmentPermissionLogicImpl;
import org.sakaiproject.assignment2.logic.impl.AssignmentSubmissionLogicImpl;
import org.sakaiproject.assignment2.logic.test.stubs.ExternalLogicStub;
import org.sakaiproject.assignment2.model.AssignmentGroup;
import org.sakaiproject.assignment2.test.AssignmentTestDataLoad;
import org.sakaiproject.assignment2.test.PreloadTestData;

import org.sakaiproject.service.gradebook.shared.GradebookFrameworkService;
import org.sakaiproject.service.gradebook.shared.GradebookService;
import org.sakaiproject.tool.gradebook.facades.Authn;
import org.sakaiproject.component.section.support.IntegrationSupport;
import org.sakaiproject.component.section.support.UserManager;
import org.sakaiproject.section.api.SectionAwareness;
import org.springframework.test.AbstractTransactionalSpringContextTests;


/**
 * Base class for assignment test classes that provides the spring application
 * context.  The database used is an in-memory hsqldb by default, but this can
 * be overridden to test specific database configurations by setting the "mem"
 * system property to "false".  In the "mem=false" case, the database configuration
 * is set in the hibernate.properties file in the "hibernate.properties.dir" directory.
 *
 */
public abstract class Assignment2TestBase extends AbstractTransactionalSpringContextTests {
	protected AssignmentDao dao;
	protected AssignmentTestDataLoad testData;

	protected ExternalAnnouncementLogic announcementLogic;
	
	protected ExternalGradebookLogicImpl gradebookLogic;
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
	
	protected static final String GRADED_ASSIGN1_TITLE = "Graded Assignment #1";
	protected static final String GRADED_ASSIGN2_TITLE = "Graded Assignment #2";
	
    protected Long gbItem1Id;
    protected Long gbItem2Id;
    protected String section1Uid;
    protected String section2Uid;
    protected String section3Uid;
	
	protected SectionAwareness sectionAwareness;
	//protected UserDirectoryService userDirectoryService;
	protected IntegrationSupport integrationSupport;
	protected UserManager userManager;
	protected ExternalLogicStub externalLogic;

	protected void onSetUpBeforeTransaction() throws Exception {
	
	}
    protected void onSetUpInTransaction() throws Exception {
    	PreloadTestData ptd = (PreloadTestData) applicationContext.getBean("org.sakaiproject.assignment2.test.PreloadTestData");
		if (ptd == null) {
			throw new NullPointerException("PreloadTestData could not be retrieved from spring");
		}
		
		testData = ptd.getAtdl();
		
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
    	externalLogic = new ExternalLogicStub();
    	externalLogic.setAuthn(authn);
    	externalLogic.setSectionAwareness(sectionAwareness);
    	
    	gradebookLogic = new ExternalGradebookLogicImpl();
    	gradebookLogic.setGradebookService(gradebookService);
    	
    	announcementLogic = new ExternalAnnouncementLogicImpl();
    	
    	permissionLogic = new AssignmentPermissionLogicImpl();
    	permissionLogic.setDao(dao);
    	permissionLogic.setExternalGradebookLogic(gradebookLogic);
    	permissionLogic.setExternalLogic(externalLogic);
    	
    	submissionLogic = new AssignmentSubmissionLogicImpl();
    	submissionLogic.setDao(dao);
    	submissionLogic.setExternalGradebookLogic(gradebookLogic);
    	submissionLogic.setExternalLogic(externalLogic);
    	submissionLogic.setPermissionLogic(permissionLogic);
    	
    	assignmentLogic = new AssignmentLogicImpl();
    	assignmentLogic.setDao(dao);
    	assignmentLogic.setExternalAnnouncementLogic(announcementLogic);
    	assignmentLogic.setExternalGradebookLogic(gradebookLogic);
    	assignmentLogic.setExternalLogic(externalLogic);
    	assignmentLogic.setAssignmentSubmissionLogic(submissionLogic);
    	assignmentLogic.setPermissionLogic(permissionLogic);
	
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
    
    /**
     * this is a bit of a hack until I can figure out how to inject the section and course
     * stuff into the data loader upon initialization. this will make the
     * groupIds consistent with the sections created here
     * @param assignGroupSet
     */
    protected void updateAssignmentGroupId(Set<AssignmentGroup> assignGroupSet) {
    	if (assignGroupSet != null && !assignGroupSet.isEmpty()) {
    		for (AssignmentGroup group : assignGroupSet) {
    			if (group.getGroupId().equals(AssignmentTestDataLoad.GROUP1_NAME)) {
    				group.setGroupId(section1Uid);
    			} else if (group.getGroupId().equals(AssignmentTestDataLoad.GROUP2_NAME)) {
    				group.setGroupId(section2Uid);
    			} else if (group.getGroupId().equals(AssignmentTestDataLoad.GROUP3_NAME)) {
    				group.setGroupId(section3Uid);
    			} 
    			dao.update(group);
    		}
    	}
    }

}
