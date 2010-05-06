/**********************************************************************************
 * $URL: https://source.sakaiproject.org/contrib/assignment2/trunk/impl/src/java/org/sakaiproject/assignment2/logic/impl/ImportExportLogicImpl.java $
 * $Id: ImportExportLogicImpl.java 67172 2010-04-14 19:33:06Z bahollad@indiana.edu $
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

package org.sakaiproject.assignment2.tool.entity;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.assignment2.tool.params.AssignmentDetailsViewParams;
import org.sakaiproject.assignment2.tool.producers.ViewAssignmentProducer;
import org.sakaiproject.entitybroker.EntityReference;
import org.sakaiproject.entitybroker.entityprovider.CoreEntityProvider;
import org.sakaiproject.entitybroker.entityprovider.capabilities.RequestStorable;
import org.sakaiproject.entitybroker.entityprovider.extension.RequestStorage;
import org.sakaiproject.entitybroker.util.AbstractEntityProvider;

import uk.ac.cam.caret.sakai.rsf.entitybroker.EntityViewParamsInferrer;
import uk.org.ponder.rsf.viewstate.ViewParameters;


/**
 * Entity Provider for viewing assignment details. 
 *
 */
public class ViewAssignment2EntityProvider extends AbstractEntityProvider implements
CoreEntityProvider, EntityViewParamsInferrer, RequestStorable {
    private static Log log = LogFactory.getLog(ViewAssignment2EntityProvider.class);

    public static final String ENTITY_PREFIX="view-assignment2";
    public boolean entityExists(String id) {
        return true;
    }

    public String getEntityPrefix() {
        return ENTITY_PREFIX;
    }
    
    public String[] getHandledPrefixes() {
        return new String[] {ENTITY_PREFIX};
     }
    
    public ViewParameters inferDefaultViewParameters(String reference) {
        EntityReference ref = new EntityReference(reference);
        Long assignId = Long.parseLong(ref.getId());
        
        // if the request includes a tagReference, we need to pass it along
        // in case it gives expanded permissions for viewing the assignment
        String tagReference = (String) requestStorage.getStoredValue("tagReference");

        return new AssignmentDetailsViewParams(ViewAssignmentProducer.VIEW_ID, assignId, null, tagReference);
     }
     
     private RequestStorage requestStorage;
     public void setRequestStorage(RequestStorage requestStorage) {
         this.requestStorage = requestStorage;
     }

}
