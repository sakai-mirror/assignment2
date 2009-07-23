/**********************************************************************************
 * $URL$
 * $Id$
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

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.logic.AttachmentInformation;
import org.sakaiproject.assignment2.logic.ExternalContentLogic;
import org.sakaiproject.assignment2.logic.ExternalContentReviewLogic;
import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;
import org.sakaiproject.assignment2.model.FeedbackAttachment;
import org.sakaiproject.assignment2.model.SubmissionAttachment;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;

import uk.org.ponder.beanutil.entity.EntityBeanLocator;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UILink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.decorators.DecoratorList;
import uk.org.ponder.rsf.components.decorators.UIFreeAttributeDecorator;
import uk.org.ponder.rsf.components.decorators.UITooltipDecorator;

/**
 * Contains a number of convenience methods for rendering different kinds of
 * Attachments (ie. Supporting Materials). Note that there are different methods
 * to be used depending on whether the attachments are on an Assignment,
 * Submission, Feedback, etc.
 * 
 * TODO FIXME I'm still not sure why some of these methods require a viewid.
 * I'm guessing they might need to generate return URL's or something.
 * It would be cool to have the option of passing null for the viewid, in 
 * which case it would look it up in the context. But that would also allow
 * you to script this in case you were generating markup for future events.
 * 
 * @author rjlowe
 * @author sgithens
 *
 */
public class AttachmentListRenderer {
    private static final Log LOG = LogFactory.getLog(AttachmentListRenderer.class);

    private ExternalContentLogic contentLogic;
    public void setExternalContentLogic(ExternalContentLogic contentLogic) {
        this.contentLogic = contentLogic;
    }

    private EntityBeanLocator assignment2EntityBeanLocator;
    public void setAssignment2EntityBeanLocator(EntityBeanLocator assignment2EntityBeanLocator) {
        this.assignment2EntityBeanLocator = assignment2EntityBeanLocator;
    }
    
    private ExternalContentReviewLogic contentReviewLogic;
    public void setExternalContentReviewLogic(ExternalContentReviewLogic contentReviewLogic) {
        this.contentReviewLogic = contentReviewLogic;
    }

    /**
     * Use this for rendering attachments from an Assignment2 assignment
     * object. 
     * 
     * @param tofill
     * @param divID
     * @param currentViewID
     * @param aaSet
     */
    public void makeAttachmentFromAssignmentAttachmentSet(UIContainer tofill, String divID, String currentViewID, Set<AssignmentAttachment> aaSet) {
        Map<String, Map> attRefPropertiesMap = new HashMap<String, Map>();
        if (aaSet != null){
            for (AssignmentAttachment aa : aaSet) {
                attRefPropertiesMap.put(aa.getAttachmentReference(), aa.getProperties());
            }
        }
        makeAttachment(tofill, divID, currentViewID, attRefPropertiesMap);
    }

    public void makeAttachmentFromAssignment2OTPAttachmentSet(UIContainer tofill, String divID, String currentViewID, String a2OTPKey) {
        Assignment2 assignment = (Assignment2)assignment2EntityBeanLocator.locateBean(a2OTPKey);
        Map<String, Map> attRefPropertiesMap = new HashMap<String, Map>();
        if (assignment != null && assignment.getAttachmentSet() != null){
            for (AssignmentAttachment aa : assignment.getAttachmentSet()) {
                attRefPropertiesMap.put(aa.getAttachmentReference(), aa.getProperties());
            }
        }
        makeAttachment(tofill, divID, currentViewID, attRefPropertiesMap);
    }

    public void makeAttachmentFromSubmissionAttachmentSet(UIContainer tofill, String divID, String currentViewID,
            Set<SubmissionAttachment> asaSet) {
        Map<String, Map> attRefPropertiesMap = new HashMap<String, Map>();
        if (asaSet != null) {
            for (SubmissionAttachment asa : asaSet) {
                attRefPropertiesMap.put(asa.getAttachmentReference(), asa.getProperties());
            }
        }
        makeAttachment(tofill, divID, currentViewID, attRefPropertiesMap);
    }

    public void makeAttachmentFromFeedbackAttachmentSet(UIContainer tofill, String divID, String currentViewID,
            Set<FeedbackAttachment> afaSet) {
        Map<String, Map> attRefPropertiesMap = new HashMap<String, Map>();
        if (afaSet != null) {
            for (FeedbackAttachment afa : afaSet) {
                attRefPropertiesMap.put(afa.getAttachmentReference(), afa.getProperties());
            }
        }
        makeAttachment(tofill, divID, currentViewID, attRefPropertiesMap);
    }

    /**
     * 
     * @param tofill
     * @param divID
     * @param currentViewID
     * @param attRefPropertiesMap a map of the attachment reference to its associated properties map
     */
    private void makeAttachment(UIContainer tofill, String divID, String currentViewID, Map<String, Map> attRefPropertiesMap) {


        int i = 1;
        if (attRefPropertiesMap.size() == 0) {
            UIJointContainer joint = new UIJointContainer(tofill, divID, "attachments:", ""+1);
            UIMessage.make(joint, "no_attachments_yet", "assignment2.no_attachments_yet");
            return;
        }

        for (String ref : attRefPropertiesMap.keySet()){
            UIJointContainer joint = new UIJointContainer(tofill, divID, "attachments:", ""+(i++));

            //TODO FIXME For some reason, when there are no attachments, we 
            // still getting a single item in the Set<String> ref that is 
            // just an empty string.  This is on previewing an assignment.
            // To reproduce, just put in a title and hit preview.
            if (ref != null && !ref.equals("")) {
                AttachmentInformation attach = contentLogic.getAttachmentInformation(ref);
                if (attach != null) {
                    String file_size = "(" + attach.getContentLength() + ")";

                    UILink.make(joint, "attachment_image", attach.getContentTypeImagePath());
                    UILink.make(joint, "attachment_link", attach.getDisplayName(), attach.getUrl());  
                    UIOutput.make(joint, "attachment_size", file_size);
                    
                    // check for properties
                    Map properties = attRefPropertiesMap.get(ref);
                    if (properties != null && !properties.isEmpty()) {
                        
                        // we may need to display plagiarism checking results
                        if (properties.containsKey(AssignmentConstants.PROP_REVIEW_STATUS)) {
                            UIOutput.make(joint, "review_report_info");
                            String status = (String)properties.get(AssignmentConstants.PROP_REVIEW_STATUS);
                            if (status.equals(AssignmentConstants.REVIEW_STATUS_ERROR)) {
                                String errorText = contentReviewLogic.getErrorMessage((Long)properties.get(AssignmentConstants.PROP_REVIEW_ERROR_CODE));
                                UIOutput errorDisplay = UIOutput.make(joint, "review_error");
                                DecoratorList errorDisplayDecorators = new DecoratorList();
                                errorDisplayDecorators.add(new UITooltipDecorator(errorText));
                                errorDisplay.decorators = errorDisplayDecorators;
                            } else if (status.equals(AssignmentConstants.REVIEW_STATUS_SUCCESS)) {
                                // create the container
                                UIOutput.make(joint, "review_report_status");
                                
                                String score = (String)(properties.get(AssignmentConstants.PROP_REVIEW_SCORE));
                                String statusCssClass = getCssClassForReviewScore(score);
                                
                                // create the link
                                UILink reportLink = UILink.make(joint, "review_report_link", score, (String)properties.get(AssignmentConstants.PROP_REVIEW_URL));
                                DecoratorList reportLinkDecorators = new DecoratorList();
                                reportLinkDecorators.add(new UITooltipDecorator("Click to view originality report"));
                                reportLinkDecorators.add(new UIFreeAttributeDecorator("class", statusCssClass));
                                reportLink.decorators = reportLinkDecorators;
                                
                            }
                        }
                    }
                }
            }

        } //Ending for loop
    }
    
    /**
     * Given the score in the {@link AssignmentConstants#PROP_REVIEW_SCORE} property,
     * returns the appropriate style class for displaying this score
     * @param score
     * @return
     */
    private String getCssClassForReviewScore(String score) {
        String cssClass = "reportStatus4";
        if (score != null) {
            // strip out the %
            String modScore = score.replace("%", "");
            try {
                long scoreAsNum = Long.parseLong(modScore);
                if (scoreAsNum == 0) {
                    cssClass = "reportStatus0";
                } else if (scoreAsNum < 25) {
                    cssClass = "reportStatus1";
                } else if (scoreAsNum < 50) {
                    cssClass = "reportStatus2";
                } else if (scoreAsNum < 75) {
                    cssClass = "reportStatus3";
                } else {
                    cssClass = "reportStatus4";
                }
            } catch (NumberFormatException nfe) {
                // default to worst case
                cssClass = "reportStatus4";
            }
        }
        
        return cssClass;
    }

}
