package org.sakaiproject.assignment2.tool.producers;

import uk.org.ponder.rsf.producers.NullaryProducer;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UIVerbatim;


public class LayoutProducer implements NullaryProducer {

	private NullaryProducer pageproducer;

	public void setPageProducer(NullaryProducer pageproducer) {
		this.pageproducer = pageproducer;
	}
  
	public void fillComponents(UIContainer tofill) {
		UIJointContainer page = new UIJointContainer(tofill, "page-replace:", "page:");
		
		//Initialize iframeId var
        String frameId = org.sakaiproject.util.Web.escapeJavascript("Main" + org.sakaiproject.tool.cover.ToolManager.getCurrentPlacement().getId());
        UIVerbatim.make(tofill, "iframeId_init", "var iframeId = \"" + frameId + "\";");
		
		//include the components from the page body into tag "page-replace:"
		pageproducer.fillComponents(page);
	}
}