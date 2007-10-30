package org.sakaiproject.assignment2.tool.beans;

import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.logic.ExternalLogic;

import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

public class Assignment2Creator {

	//public static final String DEFAULT_TITLE = "";
	ExternalLogic externalLogic;

    public Assignment2 create() {
  
    	Assignment2 togo = new Assignment2();
    	togo.setTitle("");
    	togo.setSiteId(externalLogic.getCurrentLocationId());
    	togo.setCreator(externalLogic.getCurrentUserId());
    	togo.setRemoved(Boolean.FALSE);
    	togo.setHonorPledge(Boolean.FALSE);
    	togo.setRestrictedToGroups(Boolean.FALSE);
    	togo.setUngraded(Boolean.FALSE);
    	
    	//Setting up Dates
    	Calendar cal = Calendar.getInstance();
    	cal.set(Calendar.HOUR_OF_DAY, 12);
    	cal.set(Calendar.MINUTE, 0);
    	Date openDate = cal.getTime();
    	cal.add(Calendar.DAY_OF_YEAR, 7);
    	cal.set(Calendar.HOUR_OF_DAY, 17);
    	Date closeDate = cal.getTime();
    	
    	togo.setOpenTime(openDate);
    	togo.setDueDateForUngraded(closeDate);
    	togo.setCloseTime(closeDate);
    	togo.setDropDeadTime(new Date());
    	return togo;
  }

  public void setExternalLogic(ExternalLogic externalLogic) {
	  this.externalLogic = externalLogic;
  }
}