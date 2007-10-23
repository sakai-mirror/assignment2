package org.sakaiproject.assignment2.tool.beans;

import org.sakaiproject.assignment2.model.Assignment2;



public class Assignment2Creator {

  //public static final String DEFAULT_TITLE = "";

  public Assignment2 create() {

    Assignment2 togo = new Assignment2();
    //togo.setHidden(DEFAULT_HIDDEN);
    return togo;
  }

}