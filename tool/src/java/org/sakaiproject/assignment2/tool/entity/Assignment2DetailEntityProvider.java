package org.sakaiproject.assignment2.tool.entity;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import org.sakaiproject.entitybroker.IdEntityReference;
import org.sakaiproject.entitybroker.entityprovider.CoreEntityProvider;
import org.sakaiproject.entitybroker.entityprovider.EntityProvider;
import org.sakaiproject.entitybroker.entityprovider.EntityProviderManager;

import org.sakaiproject.assignment2.tool.params.AssignmentViewParams;
import org.sakaiproject.assignment2.tool.producers.AssignmentDetailProducer;


import uk.ac.cam.caret.sakai.rsf.entitybroker.EntityViewParamsInferrer;
import uk.org.ponder.rsf.viewstate.ViewParameters;

/*
 * This is a provider for looking up and adding/editing Gradebook Items.
 */
public class Assignment2DetailEntityProvider implements EntityProvider, CoreEntityProvider,
EntityViewParamsInferrer {
    private Log log = LogFactory.getLog(Assignment2DetailEntityProvider.class);
    public final static String ENTITY_PREFIX = "assignment2_detail";
    private EntityProviderManager entityProviderManager;
    
    public void init() {
        log.info("init()");
        entityProviderManager.registerEntityProvider(this);
    }
    
    public void destroy() {
        log.info("destroy()");
        entityProviderManager.unregisterEntityProvider(this);
    }
    
    public String getEntityPrefix() {
        return ENTITY_PREFIX;
    }

    public boolean entityExists(String id) {
        return true;
    }

    public String[] getHandledPrefixes() {
        return new String[] {ENTITY_PREFIX};
    }

    public ViewParameters inferDefaultViewParameters(String reference) {
    	
    	IdEntityReference assignmentId = new IdEntityReference(reference);
    	Long id = Long.parseLong(assignmentId.id);
    	return new AssignmentViewParams(AssignmentDetailProducer.VIEW_ID, id);
    }

    public void setEntityProviderManager(EntityProviderManager entityProviderManager) {
        this.entityProviderManager = entityProviderManager;
    }

}
