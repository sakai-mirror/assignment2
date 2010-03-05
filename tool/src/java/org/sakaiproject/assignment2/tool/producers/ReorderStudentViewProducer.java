package org.sakaiproject.assignment2.tool.producers;

import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.tool.api.Placement;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UICommand;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIForm;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;

public class ReorderStudentViewProducer implements ViewComponentProducer {
    public static final String VIEW_ID = "reorder-student-view";
    
    private MessageLocator messageLocator;
    private ExternalLogic externalLogic;
    private Placement placement;

    @Override
    public void fillComponents(UIContainer tofill, ViewParameters viewparams,
            ComponentChecker checker) {
        
        UIVerbatim.make(tofill, "asnnlist-decl-js", "var sakai = sakai || {};"
                + "sakai.curPlacement = '"+placement.getId()+"';"
                + "sakai.curContext = '"+externalLogic.getCurrentContextId()+"';");
    
        
        //Breadcrumbs
        UIInternalLink.make(tofill, "breadcrumb", 
                messageLocator.getMessage("assignment2.list.heading"),
                new SimpleViewParameters(ListProducer.VIEW_ID));
        
        UIForm form = UIForm.make(tofill, "reorder-form");
        
        UICommand.make(form, "save-reorder-top", UIMessage.make("assignment2.reorder-student-view.save"), null);
        UICommand.make(form, "save-reorder-bottom", UIMessage.make("assignment2.reorder-student-view.save"), null);
        
        UICommand.make(form, "cancel-reorder-top", UIMessage.make("assignment2.reorder-student-view.cancel"), null);
        UICommand.make(form, "cancel-reorder-bottom", UIMessage.make("assignment2.reorder-student-view.cancel"), null);

    }

    @Override
    public String getViewID() {
        return VIEW_ID;
    }
    
    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }
    
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }

    public void setPlacement(Placement placement) {
        this.placement = placement;
    }
    
}
