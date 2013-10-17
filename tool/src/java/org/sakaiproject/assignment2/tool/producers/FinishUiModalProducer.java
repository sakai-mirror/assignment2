package org.sakaiproject.assignment2.tool.producers;

import org.sakaiproject.tool.api.Session;
import org.sakaiproject.tool.api.SessionManager;
import org.sakaiproject.component.api.ServerConfigurationService;
import org.sakaiproject.tool.api.Placement;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;

import uk.org.ponder.rsf.components.UIOutput;


public class FinishUiModalProducer implements ViewComponentProducer {

    private SessionManager sessionManager;
    public void setSessionManager(SessionManager sessionManager) {
        this.sessionManager = sessionManager;
    }
    
    private ServerConfigurationService serverConfigurationService;
    public void setServerConfigurationService(ServerConfigurationService serverConfigurationService) {
        this.serverConfigurationService = serverConfigurationService;
    }

    private Placement placement;
    public void setPlacement(Placement placement) {
        this.placement = placement;
    }
    
    public static final String VIEW_ID = "finish-ui-modal";
    public String getViewID() {
        return VIEW_ID;
    }
    
    
    
    @Override
    public void fillComponents(UIContainer tofill, ViewParameters viewparams, ComponentChecker checker) {
        Session s = sessionManager.getCurrentSession();
        
        if (s != null) {
            s.removeAttribute("osp.review.processorsakai.tool.helper.done.url");
        }        

    }
}
