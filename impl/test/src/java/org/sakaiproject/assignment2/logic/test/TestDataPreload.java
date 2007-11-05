/******************************************************************************
 * TestDataPreload.java - created by Sakai App Builder -AZ
 * 
 * Copyright (c) 2006 Sakai Project/Sakai Foundation
 * Licensed under the Educational Community License version 1.0
 * 
 * A copy of the Educational Community License has been included in this 
 * distribution and is available at: http://www.opensource.org/licenses/ecl1.php
 * 
 *****************************************************************************/

package org.sakaiproject.blogwow.logic.test;

import java.util.Date;

import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.genericdao.api.GenericDao;

/**
 * Contains test data for preloading and test constants
 * @author 
 */
public class TestDataPreload {
	
	private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }
	
    /**
     * Preload a bunch of test data into the database
     * @param dao a generic dao
     */
    public void preloadTestData(GenericDao dao) {
        Assignment2 testAssignment = new Assignment2();
        testAssignment.setAllowResubmitUntilDue(Boolean.FALSE);
        testAssignment.setCreateTime(new Date());
        testAssignment.setCreator("admin");
        testAssignment.setDraft(Boolean.FALSE);
        testAssignment.setOpenTime(new Date());
        testAssignment.setAcceptUntilTime(new Date());
        testAssignment.setGroupSubmission(Boolean.FALSE);
        testAssignment.setRestrictedToGroups(Boolean.FALSE);
        testAssignment.setHonorPledge(Boolean.FALSE);
        testAssignment.setUngraded(Boolean.TRUE);
        testAssignment.setSiteId("location123");
        testAssignment.setNotificationType(0);
        testAssignment.setSubmissionType(0);
        testAssignment.setTitle("Test Assignment");
        
        dao.save(testAssignment);
    }

}
