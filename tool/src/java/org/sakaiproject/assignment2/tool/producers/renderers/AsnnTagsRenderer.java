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

import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.ExternalTaggableLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentSubmission;
import org.sakaiproject.assignment2.taggable.api.AssignmentActivityProducer;
import org.sakaiproject.component.api.ServerConfigurationService;
import org.sakaiproject.taggable.api.Evaluation;
import org.sakaiproject.taggable.api.EvaluationContainer;
import org.sakaiproject.taggable.api.EvaluationProvider;
import org.sakaiproject.taggable.api.Tag;
import org.sakaiproject.taggable.api.TagColumn;
import org.sakaiproject.taggable.api.TagList;
import org.sakaiproject.taggable.api.TaggableActivity;
import org.sakaiproject.taggable.api.TaggingProvider;
import org.sakaiproject.tool.api.Placement;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UIMessage;
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
    private ExternalLogic externalLogic;
    private AsnnToggleRenderer toggleRenderer;
    private MessageLocator messageLocator;
    
    private ServerConfigurationService serverConfigurationService;
    public void setServerConfigurationService(ServerConfigurationService serverConfigurationService) {
        this.serverConfigurationService = serverConfigurationService;
    }

    private Placement placement;
    public void setPlacement(Placement placement) {
        this.placement = placement;
    }
    

    
    /**
     * 
     * @param tofill
     * @param divID
     * @param assignment
     * @param submission
     * @param includeToggle true if you want a toggle-able heading for each provider
     * @param includeToggleBar true if you want the toggle to display as a bar. includeToggle must be true
     * @param expandToggle true if you want the toggle to initially display expanded
     */
    public void makeTagInformation(UIContainer tofill, String divID, Assignment2 assignment, AssignmentSubmission submission, boolean includeToggle, boolean includeToggleBar, boolean expandToggle){

        String tempString = null;        
        Evaluation tempEval = null;
        boolean isHeaderRenderedForThisTag = false;
        String tempColumnName = null;
        
        String exitUrl = getFullToolUrl() + "/finish-ui-modal";
                
        if (!taggableLogic.isSiteAssociated(assignment.getContextId()) || !taggableLogic.isProducerEnabled()) {
            return;
        }
        
        // we don't render tag information for new assignments
        if (assignment == null || assignment.getId() == null) {
            return;
        }
        
        UIJointContainer mainContainer = new UIJointContainer(tofill, divID, "assn2-assignment-tags-widget:");
        
        // may not need this if removeEval doesn't need to clear session (directing to exitUrl)
//        UIVerbatim.make(mainContainer, "script-area", "<script type=\"text/javascript\"> A2_exitUrl = \"" + exitUrl + "\";</script>");

        // retrieve the available providers
        List<TaggingProvider> providers = taggableLogic.getProviders();
        if (providers != null) {
        	String userId = externalLogic.getCurrentUserId();
            TaggableActivity activity = activityProducer.getActivity(assignment);
            for (TaggingProvider provider: providers) {
                TagList tags = provider.getTags(activity);
                if (tags != null && !tags.isEmpty()) {
                    // make a section for each provider
                    UIBranchContainer providerContainer = UIBranchContainer.make(mainContainer, "provider-section:");
                    
                    // render the provider details
                    UIBranchContainer detailsContainer = UIBranchContainer.make(providerContainer, "provider_details:");

                    UIOutput.make(detailsContainer, "tag_header");
                    UIOutput.make(detailsContainer, "provider-heading", provider.getName());
                    UIOutput.make(detailsContainer, "provider-instruction", provider.getSimpleTextLabel());
                    UIOutput description = UIOutput.make(detailsContainer, "provider-description", provider.getHelpLabel());
                    description.decorate(new UIFreeAttributeDecorator("title", provider.getHelpDescription()));

                    // now, render the tag data
                    for (Tag tag : tags) {
                        UIBranchContainer recordContainer = UIBranchContainer.make(detailsContainer, "record:");
                        UIBranchContainer tableEven = UIBranchContainer.make(recordContainer, "table:even");
                        UIBranchContainer tableOdd = UIBranchContainer.make(recordContainer, "table:odd");
                        
                        for (TagColumn column : tags.getColumns()) {
                            
                            if (! isHeaderRenderedForThisTag) { // only print tag headings once
                                tempColumnName = column.getDisplayName();
                            }
                            else {
                                tempColumnName = " ";
                            }
                            
                            UIOutput.make(tableEven, "tag-headings:", tempColumnName);
                        }
                        
                        if (! isHeaderRenderedForThisTag) {
                            isHeaderRenderedForThisTag = true;
                        }
                        
                        for (TagColumn column : tags.getColumns()) {
                            String s = tag.getField(column);
                            UIVerbatim.make(tableEven, "tag-data:", stripLibraries(tag.getField(column)));
                            
                        } // end for TagColumn

                        // when I have time I'll try to get toggle to work - dsobiera
/*                        UIBranchContainer toggleRecord = UIBranchContainer.make(recordContainer, "table:toggle");
                        UIBranchContainer toggleDetails = UIBranchContainer.make(recordContainer, "table:details");
                        
                        toggleRenderer.makeToggle(recordContainer, toggleRecord.getFullID(), null, true, 
                                "text 1", "text 2", false, false, false, false, null);
*/


                        if (submission != null && submission.getId() != null) {
                            if (provider instanceof EvaluationProvider) {
                                String submissionRef = submission.getReference() + "@" + submission.getUserId();
                                EvaluationContainer ec = ((EvaluationProvider) provider).getEvaluationContainer(submissionRef, (String)tag.getObject(), userId, submission.getUserId());
                                List<Evaluation> evals = ec.getEvaluations();
                                
                                if (ec.isCanHaveEvaluations() && ! ec.isHideItemLevelEvaluations() ) {
                                    
                                    UIVerbatim.make(tableOdd, "provider-headings:", messageLocator.getMessage("assignment2.tags.evaluations.evaluations.header") + ": " + 
                                                    getRawHtmlForTbUrl(ec.isCanAddEvaluation(), ec.getAddActionURL(), 
                                                                       messageLocator.getMessage("assignment2.tags.evaluations.add"), true, false, exitUrl, true));
                                    UIOutput.make(tableOdd, "provider-headings:", messageLocator.getMessage("assignment2.tags.evaluations.actions.header"));
                                    UIOutput.make(tableOdd, "provider-headings:", messageLocator.getMessage("assignment2.tags.evaluations.createdby.header"));
                                    UIOutput.make(tableOdd, "provider-headings:", messageLocator.getMessage("assignment2.tags.evaluations.lastmodified.header"));
                                
                                    for (Evaluation eval : evals) {
                                        UIBranchContainer providerDataRow = UIBranchContainer.make(tableOdd, "provider-data-row:");

                                        UIVerbatim.make(providerDataRow, "provider-data:", "<img src=\"/library/image/silk/comments.gif\" /> " + getRawHtmlForTbUrl(eval.isCanViewEvaluation(), eval.getEvalItemURL(), eval.getEvalItemTitle(), true, true, exitUrl, false));
                                    
                                        if (eval.isCanModifyEvaluation() && eval.isCanRemoveEvaluation()) {
                                            tempString = " | ";
                                        }
                                        else {
                                            tempString = "";
                                        }
                                        
                                        UIVerbatim.make(providerDataRow, "provider-data:", getRawHtmlForTbUrl(eval.isCanModifyEvaluation(), eval.getEditActionURL(), 
                                                                                                              messageLocator.getMessage("assignment2.tags.evaluations.edit"), 
                                                                                                              true, false, exitUrl, true) + 
                                                                                           tempString + 
                                                                                           getRawHtmlForRemoveUrl(eval.isCanRemoveEvaluation(), eval.getRemoveActionURL(), 
                                                                                                                  messageLocator.getMessage("assignment2.tags.evaluations.remove")));
                                        UIVerbatim.make(providerDataRow, "provider-data:", eval.getCreatedByName());
                                        UIVerbatim.make(providerDataRow, "provider-data:", eval.getLastModDate());
                                    } // for evals
                                } // end isCanHaveEvaluations
                            } // end provider instanceof EvaluationProvider
                        } // end if submission!= null && submission.getId() != null
                    } // end for Tag
                } // end if tags != null
            } // end for TaggingProvider
        } // end if providers !=null
    }
    
    /**
     * 
     * @param tofill
     * @param divID
     * @param assignment
     * @param includeToggle true if you want a toggle-able heading for each provider
     * @param includeToggleBar true if you want the toggle to display as a bar. includeToggle must be true
     * @param expandToggle true if you want the toggle to initially display expanded
     */
    public void makeTagInformation(UIContainer tofill, String divID, Assignment2 assignment, boolean includeToggle, boolean includeToggleBar, boolean expandToggle){
    	makeTagInformation(tofill, divID, assignment, null, includeToggle, includeToggleBar, expandToggle);
    }

    public void fillComponents(UIContainer parent, String clientID) {

    }
    
    public void setExternalTaggableLogic(ExternalTaggableLogic taggableLogic) {
        this.taggableLogic = taggableLogic;
    }
    
    public void setAssignmentActivityProducer(AssignmentActivityProducer activityProducer) {
        this.activityProducer = activityProducer;
    }
    
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }
    
    public void setAsnnToggleRenderer(AsnnToggleRenderer toggleRenderer) {
        this.toggleRenderer = toggleRenderer;
    }
    
    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }
    
    /**
     * TODO: find a more robust method for handling this!
     * The tag field returned includes html for creating a link and thickbox for
     * display from assignment2. Unfortunately, this html also includes a
     * version of jquery and a version of thickbox.js that each collide with
     * the versions of these libraries that we are using.
     * For now, let's just strip out the jquery and thickbox script tags to use
     * assignment2's versions
     * @param html
     * @return
     */
    private String stripLibraries(String html) {
        String serverUrl = externalLogic.getServerUrl();
        
        StringBuilder jQueryInclude = new StringBuilder();
        jQueryInclude.append("<script type=\"text/javascript\" language=\"JavaScript\" src=\"");
        jQueryInclude.append(serverUrl);
        jQueryInclude.append("/library/js/jquery-ui-latest/js/jquery.min.js\"></script>");
        
        if (html != null && html.indexOf(jQueryInclude.toString()) != -1) {
            // strip this line from the html to avoid colliding versions
            html = html.replaceAll(jQueryInclude.toString(), "");
        }
        
        // the thickbox seems to be working properly now, so commenting out
        
        // SWG 2010-04-15 While the thickbox was maybe working for OSP, it was
        // overriding the A2 one and causing problems with our attachments helper.
        // un-un-commenting the code below.
        
        StringBuilder thickboxInclude = new StringBuilder();
        thickboxInclude.append("<script type=\"text/javascript\" language=\"JavaScript\"src=\"");
        thickboxInclude.append(serverUrl);
        thickboxInclude.append("/osp-common-tool/js/thickbox.js\"></script>");
        thickboxInclude.append("<link href=\"");
        thickboxInclude.append(serverUrl);
        thickboxInclude.append("/osp-common-tool/css/thickbox.css\" type=\"text/css\"rel=\"stylesheet\" media=\"all\" />");
        
        if (html != null && html.indexOf(thickboxInclude.toString()) != -1) {
            // strip this line from the html to avoid colliding versions
            html = html.replaceAll(thickboxInclude.toString(), "");
        }
        
        return html;
    }
    
    /**
     * This helper method returns an anchor html tag
     * 
     * @param permission either true or false. Whether the html should be blank (false) or rendered (true)
     * @param url The url to use as the href
     * @param label what text should be displayed for the anchor
     * @param thickboxed whether this tag should have class='thickbox' in it
     * @param displayHeader whether the thickbox should have a header to close itself or not
     * @param destinationUrl url to put at the end of the "real" url 
     * @param returnEmptyString true is empty string is default return value if user doesn't have permsission, false if label is returned back
     * @return html for an anchor (<a href="...")
     */
    private String getRawHtmlForTbUrl(boolean permission, String url, String label, boolean thickboxed, boolean displayHeader, String destinationUrl, boolean returnEmptyString) {
        String html = null;
        String thickboxClassString = null;
        String localUrl = url;
        
        if (! returnEmptyString && label != null) {
            html = label;
        } else {
            html = "";
        }
        
        if (url == null || label == null) {
            return html;
        }
        
        if (permission) {
            
            if (destinationUrl != null) {
                localUrl = localUrl + "&session.osp.review.processorsakai.tool.helper.done.url=" + destinationUrl;
            }
                
            if (thickboxed) {
                thickboxClassString = " class=\"thickbox\" ";
                
                localUrl = localUrl + "&TB_iframe=true&height=500&width=700";
                
                if (! displayHeader) {
                    localUrl = localUrl + "&modal=true";
                }
            } else {
                thickboxClassString = "";
            }
 
            
            html = "<a href=\"" + localUrl + "\"" + thickboxClassString + ">" + label + "</a>";
        }
        
        return html;
    }
    
    /**
     * This helper method returns an anchor tag for eval removal with call to onclick JS event
     * 
     * @param permission either true or false. Whether the html should be blank (false) or rendered (true)
     * @param url what the href's destinatation is
     * @param label what text to display in the anchor tag
     * @return html for anchor tag
     */
    private String getRawHtmlForRemoveUrl(boolean permission, String url, String label) {
        String html = "";
        
        
        if (url == null  || label == null) {
            return html;
        }
        
        if (permission) {
            html = "<a href=\"#\" onClick=\"A2_callUrl('" + url + "');\" >" + label + "</a>"; 
        }
        
        return html;
    }

    /**
     * This helper method returns the URL for the tool
     * 
     * @return
     */
    private String getFullToolUrl() {
        return serverConfigurationService.getToolUrl() + "/" + placement.getId();
    }
    
}
