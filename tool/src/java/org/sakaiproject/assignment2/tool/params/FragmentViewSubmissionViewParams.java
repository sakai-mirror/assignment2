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

public class FragmentViewSubmissionViewParams extends SimpleViewParameters {

    public Long submissionVersionId;

    public FragmentViewSubmissionViewParams(){}

    public FragmentViewSubmissionViewParams(String viewId, Long submissionVersionId) {
        super(viewId);
        this.submissionVersionId = submissionVersionId;
    }

    public String getParseSpec() {
        return super.getParseSpec() + ",@1:submissionVersionId";
    }
}