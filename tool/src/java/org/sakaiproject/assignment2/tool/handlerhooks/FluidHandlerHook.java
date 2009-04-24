package org.sakaiproject.assignment2.tool.handlerhooks;

import java.io.IOException;

import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.tool.api.Placement;

import uk.org.ponder.rsf.processor.HandlerHook;
import uk.org.ponder.rsf.viewstate.ViewParameters;

public class FluidHandlerHook implements HandlerHook {
    private static Log log = LogFactory.getLog(FluidHandlerHook.class);
    
    /**
     * Request Scope Dependency
     */
    private HttpServletResponse response;
    public void setResponse(HttpServletResponse response) {
        this.response = response;
    }
    
    /**
     * Request Scope Dependency
     */
    private ViewParameters viewparams;
    public void setViewparams(ViewParameters viewparams) {
        this.viewparams = viewparams;
    }
    
    /**
     * Request Scope Dependency
     */
    private String context;
    public void setContext(String context) {
        this.context = context;
    }
    
    /**
     * Request Scope Dependency
     */
    private Placement placement;
    public void setPlacement(Placement placement) {
        this.placement = placement;
    }
    
    public boolean handle() {
        boolean handled = false;
        
        if (viewparams.viewID.equals("list")) {
            try {
                response.sendRedirect("/sakai-assignment2-tool/content/templates/inst-asnn-list.jsp?context="+context+"&placement="+placement.getId());
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            handled = true;
        }
        
        return handled;
    }
    
    
}
