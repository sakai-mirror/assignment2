/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/branches/ASNN-521/tool/src/java/org/sakaiproject/assignment2/tool/producers/renderers/AttachmentListRenderer.java $
 * $Id: AttachmentListRenderer.java 65323 2009-12-17 20:42:47Z wagnermr@iupui.edu $
 ***********************************************************************************
 *
 * Copyright (c) 2007, 2008 The Sakai Foundation.
 *
 * Licensed under the Educational Community License, Version 1.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.opensource.org/licenses/ecl1.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 **********************************************************************************/
package org.sakaiproject.assignment2.tool.producers.renderers;

import java.util.List;

import org.sakaiproject.assignment2.logic.ExternalTaggableLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.taggable.api.AssignmentActivityProducer;
import org.sakaiproject.taggable.api.Tag;
import org.sakaiproject.taggable.api.TagColumn;
import org.sakaiproject.taggable.api.TagList;
import org.sakaiproject.taggable.api.TaggableActivity;
import org.sakaiproject.taggable.api.TaggingProvider;

import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.components.decorators.UIFreeAttributeDecorator;
import uk.org.ponder.rsf.producers.BasicProducer;

/**
 * Renders the section that displays the tags associated with a given assignment.
 * This renderer is smart enough to not display this section if this site is
 * not taggable or no tags exist, so you don't need to check beforehand.
 *
 */
public class AsnnTagsRenderer implements BasicProducer {
    
    private ExternalTaggableLogic taggableLogic;
    private AssignmentActivityProducer activityProducer;
    
    /**
     * 
     * @param tofill
     * @param divID
     * @param assignment
     */
    public void makeTagInformation(UIContainer tofill, String divID, Assignment2 assignment){

        if (!taggableLogic.isSiteAssociated(assignment.getContextId())) {
            return;
        }
        
        UIJointContainer mainContainer = new UIJointContainer(tofill, divID, "assn2-assignment-tags-widget:");
        
        // retrieve the available providers
        List<TaggingProvider> providers = taggableLogic.getProviders();
        if (providers != null) {
            TaggableActivity activity = activityProducer.getActivity(assignment);
            for (TaggingProvider provider: providers) {
                TagList tags = provider.getTags(activity);
                if (tags != null && !tags.isEmpty()) {
                    // make a section for each provider
                    UIBranchContainer providerContainer = UIBranchContainer.make(mainContainer, "provider-section:");
                    UIOutput.make(providerContainer, "provider-heading", provider.getName());
                    UIOutput.make(providerContainer, "provider-instruction", provider.getSimpleTextLabel());
                    UIOutput description = UIOutput.make(providerContainer, "provider-description", provider.getHelpLabel());
                    description.decorate(new UIFreeAttributeDecorator("title", provider.getHelpDescription()));
                    
                    // make the tag table for this provider
                    // first, render the headers
                    for (TagColumn column : tags.getColumns()) {
                        UIOutput.make(providerContainer, "tag-heading:", column.getDisplayName());
                    }

                    // now, render the tag data
                    for (Tag tag : tags) {
                        UIOutput.make(providerContainer, "tag-data-row:");
                        for (TagColumn column : tags.getColumns()) {
                            UIVerbatim.make(providerContainer, "tag-data:", stripLibraries(tag.getField(column)));
                        }
                    }
                }
            }
        }
    }

    public void fillComponents(UIContainer parent, String clientID) {

    }
    
    public void setExternalTaggableLogic(ExternalTaggableLogic taggableLogic) {
        this.taggableLogic = taggableLogic;
    }
    
    public void setAssignmentActivityProducer(AssignmentActivityProducer activityProducer) {
        this.activityProducer = activityProducer;
    }
    
    /**
     * TODO: find a more robust method for handling this!
     * The tag field returned includes html for creating a link and thickbox for
     * display from assignment2. Unfortunately, this html also includes an old
     * version of jquery and a version of thickbox.js that each collide with
     * the versions of these libraries that we are using.
     * For now, let's just strip out the jquery and thickbox script tags to use
     * assignment2's versions
     * @param html
     * @return
     */
    private String stripLibraries(String html) {
        String librariesInclude = "<script type=\"text/javascript\" language=\"JavaScript\" src=\"http://149.166.143.191:8080/osp-common-tool/js/jquery-1.2.1.js\"></script><script type=\"text/javascript\" language=\"JavaScript\"src=\"http://149.166.143.191:8080/osp-common-tool/js/thickbox.js\"></script><link href=\"http://149.166.143.191:8080/osp-common-tool/css/thickbox.css\" type=\"text/css\"rel=\"stylesheet\" media=\"all\" />";
        if (html != null && html.indexOf(librariesInclude) != -1) {
            // strip this line from the html to avoid colliding versions
            html = html.replaceAll(librariesInclude, "");
        }
        
        return html;
    }

}
