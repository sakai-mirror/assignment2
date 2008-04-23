/**********************************************************************************
 * $URL$
 * $Id$
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
package org.sakaiproject.assignment2.logic.impl;

import java.io.Externalizable;
import java.util.Collection;

import org.sakaiproject.assignment2.logic.entity.AssignmentDefinition;


public class AssignmentToolDefinition extends VersionedExternalizable implements Externalizable {	
	private static final long serialVersionUID = 1L;
	public static final String EXTERNALIZABLE_VERSION = "1";

	private Collection<AssignmentDefinition> assignments;
	
    public AssignmentToolDefinition() {
	}
	
	public String getExternalizableVersion() {
		return EXTERNALIZABLE_VERSION;
	}

	public Collection<AssignmentDefinition> getAssignments() {
		return assignments;
	}
	
	public void setAssignments(Collection<AssignmentDefinition> assignments) {
		this.assignments = assignments;
	}
	
}
