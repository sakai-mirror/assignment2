package org.sakaiproject.assignment2.tool.taggable;

import javax.servlet.http.HttpServletRequest;

import uk.org.ponder.rsf.processor.HandlerHook;
import uk.org.ponder.rsf.viewstate.ViewParameters;

/**
 * @author Andrew Thornton
 * 
 */

public class DebugHelperHandlerHook implements HandlerHook {
  private ViewParameters viewParametersProxy;
  private DebugHelperHandlerHookBean hhhb;
 

  public boolean handle() {
    System.out.println("THEVIEW: " + viewParametersProxy.viewID);
    String[] pathInfo = hhhb.getPathInfo();
    System.out.println(pathInfo[0]);
    
    if (pathInfo[0].equals("osp.matrix.link.helper")) {
        return hhhb.handle();
    }
    else if (viewParametersProxy.get() instanceof DebugHelperViewParameters) {
      return hhhb.handle();
    }
    else return false;
  }

  public void setViewParametersProxy(ViewParameters viewParameters) {
    this.viewParametersProxy = viewParameters;
  }

  public void setHelperHandlerHookBean(DebugHelperHandlerHookBean hhhb) {
    this.hhhb = hhhb;
  }

}
