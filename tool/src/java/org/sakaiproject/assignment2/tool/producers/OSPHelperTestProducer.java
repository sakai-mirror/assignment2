package org.sakaiproject.assignment2.tool.producers;

import java.util.ArrayList;
import java.util.List;

import org.sakaiproject.assignment2.taggable.api.AssignmentActivityProducer;
import org.sakaiproject.assignment2.tool.DecoratedTaggingProvider;
import org.sakaiproject.assignment2.tool.params.FilePickerHelperViewParams;
import org.sakaiproject.assignment2.tool.params.TaggableHelperViewParams;
import org.sakaiproject.component.cover.ComponentManager;
import org.sakaiproject.taggable.api.TaggingHelperInfo;
import org.sakaiproject.taggable.api.TaggingManager;
import org.sakaiproject.taggable.api.TaggingProvider;

import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.view.ComponentChecker;
import uk.org.ponder.rsf.view.ViewComponentProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;

public class OSPHelperTestProducer implements ViewComponentProducer {
	public static final String VIEW_ID = "osptest";

	@Override
	public void fillComponents(UIContainer tofill, ViewParameters viewparams,
			ComponentChecker checker) {
		UIInternalLink addAttachLink = UIInternalLink.make(tofill, "add_attachments", UIMessage.make("assignment2.assignment_add.add_attachments"),
				new FilePickerHelperViewParams(AddAttachmentHelperProducer.VIEWID, Boolean.TRUE, 
						Boolean.TRUE, 500, 700, "OTPKey"));

	}
	private void renderMatrixTagging() {
		/*** Removing support for Assignments2 and matrix linking for now */
	        TaggingManager taggingManager = (TaggingManager) ComponentManager.get("org.sakaiproject.taggable.api.TaggingManager");
	        if (taggingManager.isTaggable() && assignment != null){
	                //TODO: optimize?
	                List<DecoratedTaggingProvider> providers = initDecoratedProviders();

	                AssignmentActivityProducer assignmentActivityProducer = (AssignmentActivityProducer) ComponentManager
	                .get("org.sakaiproject.assignment2.taggable.api.AssignmentActivityProducer");

	                for (DecoratedTaggingProvider provider : providers){
	                        UIBranchContainer tagLinks = UIBranchContainer.make(row, "tag_provider_links:");
	                        String ref = assignmentActivityProducer.getActivity(
	                                                assignment).getReference();
	                        TaggingHelperInfo helper = provider.getProvider().getActivityHelperInfo(ref);
	                        if (helper != null){
	                                //String url = ServerConfigurationService.getToolUrl() + "/" + 
	                                //      helper.getPlacement() + "/" + helper.getHelperId() + 
	                                //      ".helper?1=1";
	                                String url = "/?1=1";
	                                for (String key : helper.getParameterMap().keySet()) {
	                                        url = url + "&" + key + "=" + helper.getParameterMap().get(key);
	                                }

	                                //UILink.make(tagLinks, "assignment_view_links", helper.getName(), url);                                        

	                                 //This is commented out until RSF has some better helper support
	                                UIInternalLink.make(tagLinks, "assignment_view_links", helper.getName(),
	                                        new TaggableHelperViewParams(TaggableHelperProducer.VIEWID, 
	                                                        helper.getHelperId(), 
	                                                        helper.getParameterMap().keySet().toArray(new String[0]), 
	                                                        helper.getParameterMap().values().toArray(new String[0])));
	                        }
	                }
	        }

	}
	
	private List<DecoratedTaggingProvider> initDecoratedProviders() {
        TaggingManager taggingManager = (TaggingManager) ComponentManager
        .get("org.sakaiproject.taggable.api.TaggingManager");
        List<DecoratedTaggingProvider> providers = new ArrayList<DecoratedTaggingProvider>();
        for (TaggingProvider provider : taggingManager.getProviders())
        {
            providers.add(new DecoratedTaggingProvider(provider));
        }
        return providers;
    }


	@Override
	public String getViewID() {
		return VIEW_ID;
	}

}
