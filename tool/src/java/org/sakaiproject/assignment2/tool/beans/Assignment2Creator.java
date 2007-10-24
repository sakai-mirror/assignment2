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
    	togo.setSiteId(externalLogic.getCurrentLocationId());
    	togo.setCreator(externalLogic.getCurrentUserId());
    	togo.setRemoved(Boolean.FALSE);
    	
    	//Setting up Dates
    	Calendar now = Calendar.getInstance();
    	Calendar cal = Calendar.getInstance();
    	cal.set(now.YEAR, now.MONTH, now.DATE, 12, 0);
    	Date openDate = cal.getTime();
    	cal.add(Calendar.DATE, 7);
    	cal.set(Calendar.HOUR, 17);
    	Date closeDate = cal.getTime();
    	
    	togo.setOpenTime(openDate);
    	togo.setCloseTime(closeDate);
    	togo.setDropDeadTime(closeDate);
    	return togo;
  }

  public void setExternalLogic(ExternalLogic externalLogic) {
	  this.externalLogic = externalLogic;
  }
}