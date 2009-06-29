package org.sakaiproject.assignment2.tool.producers;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UICommand;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIForm;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;

public class SettingsProducer implements ViewComponentProducer
{
    private MessageLocator messageLocator;
    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }

    public static final String VIEW_ID = "settings";
    public String getViewID()
    {
        return VIEW_ID;
    }

    public void fillComponents(UIContainer tofill, ViewParameters viewparams,
            ComponentChecker checker)
    {
        // Breadcrumbs
        UIInternalLink.make(tofill, "breadcrumb", 
                messageLocator.getMessage("assignment2.list.heading"),
                new SimpleViewParameters(ListProducer.VIEW_ID));
        UIMessage.make(tofill, "last_breadcrumb", "assignment2.settings.title");

        // Main form for the settings
        UIForm form = UIForm.make(tofill, "settings-form");



        // Post Buttons
        UICommand.make(form, "save_settings", UIMessage.make("assignment2.settings.save"), null);
        UICommand.make(form, "cancel_settings", UIMessage.make("assignment2.settings.cancel"), null);
    }

}