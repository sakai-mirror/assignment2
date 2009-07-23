/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/tool/src/java/org/sakaiproject/assignment2/tool/producers/renderers/GradebookDetailsRenderer.java $
 * $Id: GradebookDetailsRenderer.java 61484 2009-06-29 19:01:16Z swgithen@mtu.edu $
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

import java.util.Map;

import org.sakaiproject.assignment2.logic.ExternalContentReviewLogic;
import org.sakaiproject.assignment2.model.constants.AssignmentConstants;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UILink;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.decorators.DecoratorList;
import uk.org.ponder.rsf.components.decorators.UIFreeAttributeDecorator;
import uk.org.ponder.rsf.components.decorators.UITooltipDecorator;


/**
 * 
 * Renders the review status indicator for items run through a content review service
 *
 */
public class ReviewStatusRenderer {
    
    private ExternalContentReviewLogic contentReviewLogic;

    public void makeReviewStatusIndicator(UIContainer tofill, String divID, Map properties){

        UIJointContainer joint = new UIJointContainer(tofill, divID, "review_report_info:");

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
    
    public void setExternalContentReviewLogic(ExternalContentReviewLogic contentReviewLogic) {
        this.contentReviewLogic = contentReviewLogic;
    }
}