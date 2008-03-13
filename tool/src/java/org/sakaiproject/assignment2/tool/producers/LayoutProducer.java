package org.sakaiproject.assignment2.tool.producers;

import java.util.List;

import uk.org.ponder.beanutil.BeanGetter;
import uk.org.ponder.rsf.producers.NullaryProducer;
import uk.org.ponder.rsf.view.LayoutProducerHolder;
import uk.org.ponder.rsf.view.ViewGroup;
import uk.org.ponder.rsf.view.support.ViewGroupResolver;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UIVerbatim;


public class LayoutProducer implements NullaryProducer {

	private NullaryProducer pageproducer;
	public void setPageProducer(NullaryProducer pageproducer) {
		this.pageproducer = pageproducer;
	}
	
	  private ViewGroupResolver viewGroupResolver;
	  private BeanGetter ELevaluator;
	  private ViewParameters viewParameters;
	  private ViewGroup group;
  
	public void fillComponents(UIContainer tofill) {
		
		LayoutProducerHolder holder = new LayoutProducerHolder();
		if (!viewGroupResolver.isMatch(group, viewParameters)){
			pageproducer.fillComponents(tofill);
		} else {
			
			
			UIJointContainer page = new UIJointContainer(tofill, "page-replace:", "page:");
			
			if (org.sakaiproject.tool.cover.ToolManager.getCurrentPlacement() != null) {
				//Initialize iframeId var -- for a few pages that need it still :-(
		        String frameId = org.sakaiproject.util.Web.escapeJavascript("Main" + org.sakaiproject.tool.cover.ToolManager.getCurrentPlacement().getId());
		        UIVerbatim.make(tofill, "iframeId_init", "var iframeId = \"" + frameId + "\";");
			}
			
			//include the components from the page body into tag "page-replace:"
			pageproducer.fillComponents(page);
		}
	}

	public void setViewGroupResolver(ViewGroupResolver viewGroupResolver) {
		this.viewGroupResolver = viewGroupResolver;
	}
	public void setELEvaluator(BeanGetter levaluator) {
		ELevaluator = levaluator;
	}
	public void setViewParameters(ViewParameters viewParameters) {
		this.viewParameters = viewParameters;
	}
	public void setGroup(ViewGroup group) {
		this.group = group;
	}
    
}
