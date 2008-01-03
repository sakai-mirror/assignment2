package org.sakaiproject.assignment2.tool.producers;

import org.sakaiproject.assignment2.tool.params.SortPagerViewParams;
import org.sakaiproject.assignment2.logic.AssignmentLogic;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UILink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.viewstate.ViewParameters;

public class SortHeaderRenderer {
	
	public static final String BULLET_UP_IMG_SRC = "/sakai-assignment2-tool/content/images/bullet_arrow_up.png";
    public static final String BULLET_DOWN_IMG_SRC = "/sakai-assignment2-tool/content/images/bullet_arrow_down.png";
    
    public void makeSortingLink (UIContainer tofill, String divID, ViewParameters viewparams, String sort_by, String link_text){
    	SortPagerViewParams params = (SortPagerViewParams) viewparams;
    	UIJointContainer joint = new UIJointContainer(tofill, divID, "sortHeader:", ""+1);

    	
    	//Link Text
    	UIMessage.make(joint, "text", link_text);
    	if (params.sort_by.equals(sort_by)){
    		UILink.make(joint, "arrow", (params.sort_dir.equals(AssignmentLogic.SORT_DIR_ASC) ? BULLET_UP_IMG_SRC : BULLET_DOWN_IMG_SRC));
    	}

    	//Add Link and modify params
    	String newSortDir = (params.sort_by.equals(sort_by) ? (params.sort_dir.equals(AssignmentLogic.SORT_DIR_ASC) 
    			? AssignmentLogic.SORT_DIR_DESC 
    			: AssignmentLogic.SORT_DIR_ASC) : AssignmentLogic.SORT_DIR_ASC);
    	
    	ViewParameters new_params = viewparams.copyBase();
    	((SortPagerViewParams)new_params).sort_by = sort_by;
    	((SortPagerViewParams)new_params).sort_dir = newSortDir;

    	UIInternalLink.make(joint, "link", new_params);
    }

}