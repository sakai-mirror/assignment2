package org.sakaiproject.assignment2.logic.impl;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.ExternalContentReviewLogic;

public class ExternalContentReviewLogicImpl implements ExternalContentReviewLogic {

    private static Log log = LogFactory.getLog(ExternalContentReviewLogicImpl.class);

    public void init(){
        if(log.isDebugEnabled()) log.debug("init");
    }

    public boolean isContentReviewAvailable() {
        return false;
    }

}