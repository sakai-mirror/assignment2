/**********************************************************************************
 * $URL: https://source.sakaiproject.org/svn/assignment/branches/oncourse_osp_enhancements/assignment-impl/impl/src/java/org/sakaiproject/assignment/taggable/impl/AssignmentActivityImpl.java $
 * $Id: AssignmentActivityImpl.java 41064 2008-02-06 20:29:06Z chmaurer@iupui.edu $
 ***********************************************************************************
 *
 * Copyright (c) 2007 The Sakai Foundation.
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

package org.sakaiproject.assignment2.taggable.impl;

import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.component.cover.ServerConfigurationService;
import org.sakaiproject.taggable.api.TaggableActivity;
import org.sakaiproject.taggable.api.TaggableActivityProducer;

public class AssignmentActivityImpl implements TaggableActivity {

	protected Assignment2 assignment;

	protected TaggableActivityProducer producer;

	public AssignmentActivityImpl(Assignment2 assignment,
			TaggableActivityProducer producer) {
		this.assignment = assignment;
		this.producer = producer;
	}

	public boolean equals(Object object) {
		if (object instanceof TaggableActivity) {
			TaggableActivity activity = (TaggableActivity) object;
			return activity.getReference().equals(this.getReference());
		}
		return false;
	}

	public String getContext() {
		return assignment.getContextId();
	}

	public String getDescription() {
		return assignment.getInstructions();
	}

	public Object getObject() {
		return assignment;
	}

	public TaggableActivityProducer getProducer() {
		return producer;
	}

	public String getReference() {
		return assignment.getReference();
	}

	public String getTitle() {
		return assignment.getTitle();
	}

	public String getActivityDetailUrl()
	{
		//TODO use constants
		// the assignment2_detail part is in tool...maybe it can be moved elsewhere?
		
		String url = ServerConfigurationService.getServerUrl() + 
			"/direct/assignment2_detail/" + Long.toString(assignment.getId());
		return url;
	}
	
	
}
