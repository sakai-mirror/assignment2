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

package org.sakaiproject.assignment2.tool.params;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

/**
 * This currently holds params for a number of seperate Ajax operations in the
 * application, such as reordering assignments, and removing attachments.
 * 
 * 
 * @author rjlowe
 * @author sgithens
 *
 */
public class AjaxCallbackViewParams extends SimpleViewParameters {

    public String[] sortable;
    public Boolean removeAttachment;
    public String refId;

    public AjaxCallbackViewParams() {}

    //For sorting
    public AjaxCallbackViewParams(String viewId, String[] sortable){
        super(viewId);
        this.sortable = sortable;
    }

    //for removing attachments
    public AjaxCallbackViewParams(String viewId, Boolean removeAttachment, String refId) {
        super(viewId);
        this.removeAttachment = removeAttachment;
        this.refId = refId;
    }
}