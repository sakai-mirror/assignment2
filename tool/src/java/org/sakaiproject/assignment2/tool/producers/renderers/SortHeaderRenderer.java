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

import org.sakaiproject.assignment2.logic.AssignmentLogic;
import org.sakaiproject.assignment2.tool.params.SortPagerViewParams;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.decorators.DecoratorList;
import uk.org.ponder.rsf.components.decorators.UIStyleDecorator;
import uk.org.ponder.rsf.viewstate.ViewParameters;

public class SortHeaderRenderer {
	
	public static final String BULLET_UP_IMG_SRC = "/sakai-assignment2-tool/content/images/bullet_arrow_up.png";
    public static final String BULLET_DOWN_IMG_SRC = "/sakai-assignment2-tool/content/images/bullet_arrow_down.png";
    
    public void makeSortingLink (UIContainer tofill, String divID, ViewParameters viewparams, String sort_by, String link_text){
    	SortPagerViewParams params = (SortPagerViewParams) viewparams;
    	UIJointContainer joint = new UIJointContainer(tofill, divID, "sortHeader:", ""+1);

    	
    	//Link Text
    	UIMessage.make(joint, "text", link_text);
    	//if (params.sort_by.equals(sort_by)){
    		//UILink.make(joint, "arrow", (params.sort_dir.equals(AssignmentLogic.SORT_DIR_ASC) ? BULLET_UP_IMG_SRC : BULLET_DOWN_IMG_SRC));
    	//}

    	//Add Link and modify params
    	String newSortDir = (params.sort_by.equals(sort_by) ? (params.sort_dir.equals(AssignmentLogic.SORT_DIR_ASC) 
    			? AssignmentLogic.SORT_DIR_DESC 
    			: AssignmentLogic.SORT_DIR_ASC) : AssignmentLogic.SORT_DIR_ASC);
    
    	
    	
    	ViewParameters new_params = viewparams.copyBase();
    	((SortPagerViewParams)new_params).sort_by = sort_by;
    	((SortPagerViewParams)new_params).sort_dir = newSortDir;

    	UIInternalLink link = UIInternalLink.make(joint, "link", new_params);
    	link.decorators = new DecoratorList(new UIStyleDecorator(sort_by));
    }

}