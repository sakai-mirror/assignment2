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

package org.sakaiproject.assignment2.logic.entity;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.sakaiproject.entity.api.ContextObserver;
import org.sakaiproject.entity.api.EntityTransferrer;
import org.sakaiproject.importer.api.HandlesImportable;
import org.sakaiproject.importer.api.Importable;

import org.sakaiproject.assignment2.logic.ExternalGradebookLogic;
import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.logic.ImportExportLogic;

/**
 * Implements the Sakai EntityProducer approach to integration of tool-specific
 * storage with site management.
 */
public class AssignmentEntityProducer extends BaseEntityProducer implements ContextObserver, EntityTransferrer, HandlesImportable {
    private static final Log log = LogFactory.getLog(AssignmentEntityProducer.class);

    private String[] toolIdArray;

    public void setToolIds(List toolIds) {
        if (log.isDebugEnabled()) log.debug("setToolIds(" + toolIds + ")");
        if (toolIds != null) {
            toolIdArray = (String[])toolIds.toArray(new String[0]);
        }
    }

    public String[] myToolIds() {
        return toolIdArray;
    }

    public void contextCreated(String context, boolean toolPlacement) {
        // If the Assignment2 tool is part of this site, we may need to
        // add the backend for the gradebook, as well (not the actual tool) if
        // it doesn't already exist
        if (context != null && toolPlacement) {
            gradebookLogic.createGradebookDataIfNecessary(context);
        }

    }

    public void contextUpdated(String context, boolean toolPlacement) {
        // If the Assignment2 tool is part of this site, we may need to
        // add the backend for the gradebook, as well (not the actual tool) if
        // it doesn't already exist
        if (context != null && toolPlacement) {
            gradebookLogic.createGradebookDataIfNecessary(context);
        }
    }

    public void contextDeleted(String context, boolean toolPlacement) {
        // do nothing
    }

    public void transferCopyEntities(String fromContext, String toContext, List ids) {
        // if the site we are importing from has the "new" assignment2 tool,
        // import from that tool. Otherwise, check to see if that site has the
        // "old" assignments tool.  If it does, we are importing from the old tool
        // to the new tool
        if (externalLogic.siteHasTool(fromContext, ExternalLogic.TOOL_ID_ASSIGNMENT2)) {
            String fromAssignment2ToolXml = importExportLogic.getAssignmentToolDefinitionXML(fromContext);
            importExportLogic.mergeAssignmentToolDefinitionXml(toContext, fromAssignment2ToolXml);
        } else if (externalLogic.siteHasTool(fromContext, ExternalLogic.TOOL_ID_OLD_ASSIGN)) {
            String fromOldAssignmentToolXml =
                importExportLogic.getAssignmentToolDefinitionXmlFromOriginalAssignmentsTool(fromContext, toContext);
            importExportLogic.mergeAssignmentToolDefinitionXml(toContext, fromOldAssignmentToolXml);
        }
    }

    public void transferCopyEntities(String fromContext, String toContext, List ids, boolean cleanup) {
        if (cleanup) {
            // we need to remove all assignments in the current site
            importExportLogic.cleanToolForImport(toContext);
        }

        transferCopyEntities(fromContext, toContext, ids);
    }

    private ExternalGradebookLogic gradebookLogic;
    public void setExternalGradebookLogic(ExternalGradebookLogic gradebookLogic) {
        this.gradebookLogic = gradebookLogic;
    }

    private ImportExportLogic importExportLogic;
    public void setImportExportLogic(ImportExportLogic importExportLogic) {
        this.importExportLogic = importExportLogic;
    }

    private ExternalLogic externalLogic;
    public void setExternalLogic(ExternalLogic externalLogic) {
        this.externalLogic = externalLogic;
    }

    ////////////////////////////////////////////////////////////////
    // TODO Speculative support for future migration / import / export starts here.

    public static final String A2_DEFINITION_TYPE = "sakai-assignment2";

    public boolean canHandleType(String typeName) {
        return (typeName.equals(A2_DEFINITION_TYPE));
    }

    public void handle(Importable importable, String siteId) {
        if (importable.getTypeName().equals(A2_DEFINITION_TYPE)) {
            //TODO implement import
        }
    }

    public List<Importable> getAllImportables(String contextId) {
        List<Importable> importables = new ArrayList<Importable>();
        importables.add(new XmlImportable(A2_DEFINITION_TYPE, importExportLogic.getAssignmentToolDefinitionXML(contextId)));
        return importables;
    }
}
