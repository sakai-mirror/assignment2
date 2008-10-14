package org.sakaiproject.assignment2.tool.producers.evolvers;

import org.sakaiproject.content.api.ContentHostingService;

import uk.org.ponder.htmlutil.HTMLUtil;
import uk.org.ponder.rsf.components.UIInput;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.evolvers.TextInputEvolver;

public class MarkupEvolver implements TextInputEvolver {
		  public static final String COMPONENT_ID = "assn2-FCKEditor:";
		  private String context;
		  private ContentHostingService contentHostingService;

		  public void setContext(String context) {
		    this.context = context;
		  }

		  public void setContentHostingService(ContentHostingService contentHostingService) {
		    this.contentHostingService = contentHostingService;
		  }
		  
		  public UIJointContainer evolveTextInput(UIInput toevolve) {
		    UIJointContainer joint = new UIJointContainer(toevolve.parent,
		        toevolve.ID, COMPONENT_ID);
		    toevolve.parent.remove(toevolve);

		    toevolve.ID = SEED_ID; // must change ID while unattached
		    joint.addComponent(toevolve);
		    String collectionID = context.equals("")? "" :
		        contentHostingService.getSiteCollection(context);
		    String js = HTMLUtil.emitJavascriptCall("Assn2.fckeditor.initializeMarkupEditor", 
		        new String[] {toevolve.getFullID(), collectionID});
		    UIVerbatim.make(joint, "textarea-js", js);
		    return joint;
		  }
}
