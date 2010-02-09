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
    public static final String MATRIX_PROV = 
        "org.theospi.portfolio.tagging.api.MatrixTaggingProvider";

    public void fillComponents(UIContainer tofill, ViewParameters viewparams,
            ComponentChecker checker) {
        
        UIInternalLink addAttachLink = UIInternalLink.make(tofill, "add_attachments", UIMessage.make("assignment2.assignment_add.add_attachments"),
                new FilePickerHelperViewParams(AddAttachmentHelperProducer.VIEWID, Boolean.TRUE, 
                        Boolean.TRUE, 500, 700, "OTPKey"));

        renderCopiedA1Test(tofill);
    }


    /**
     * This is a test, with parameters copied straight from an A1 launch of the
     * helper stopped with the debugger.
     */
    private void renderCopiedA1Test(UIContainer tofill) {
        // This is copied from my local instance, need to change if prototyping
        // on a difference box.
        String activityRef = 
            "/assignment/a/cleantools/187af2c9-aae3-42d0-8a83-c223b03e1613";

        TaggingManager taggingManager = (TaggingManager) ComponentManager
        .get("org.sakaiproject.taggable.api.TaggingManager");
        TaggingProvider provider = taggingManager.findProviderById(MATRIX_PROV);

        TaggingHelperInfo helperInfo = provider
            .getActivityHelperInfo(activityRef);

        UIInternalLink.make(tofill, "link_matrix", helperInfo.getName(),
                new TaggableHelperViewParams(TaggableHelperProducer.VIEWID, 
                        helperInfo.getHelperId(), 
                        helperInfo.getParameterMap().keySet().toArray(new String[0]), 
                        helperInfo.getParameterMap().values().toArray(new String[0])));
    }


    private void renderMatrixTagging() {
        /*** Removing support for Assignments2 and matrix linking for now */
        /*
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
         */
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


    public String getViewID() {
        return VIEW_ID;
    }

}
