package org.sakaiproject.assignment2.tool.producers;

import org.sakaiproject.assignment2.tool.params.SortPagerViewParams;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UILink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.viewstate.ViewParameters;

public class SortHeaderRenderer {
	
	public static final String BULLET_UP_IMG_SRC = "/sakai-assignment2-tool/content/images/bullet_arrow_up.png";
    public static final String BULLET_DOWN_IMG_SRC = "/sakai-assignment2-tool/content/images/bullet_arrow_down.png";
    public static final String SORT_DIR_ASC = "asc";
    public static final String SORT_DIR_DESC = "desc";
    
    public void makeSortingLink (UIContainer tofill, String divID, ViewParameters viewparams, String sort_by, String link_text){
    	SortPagerViewParams params = (SortPagerViewParams) viewparams;
    	UIJointContainer joint = new UIJointContainer(tofill, divID, "sortHeader:", ""+1);

    	//Link Text
    	UIMessage.make(joint, "text", link_text);
    	if (params.sort_by.equals(sort_by)){
    		UILink.make(joint, "arrow", (params.sort_dir.equals(SORT_DIR_ASC) ? BULLET_UP_IMG_SRC : BULLET_DOWN_IMG_SRC));
    	}

    	//Add Link and modify params
    	params.sort_dir = (params.sort_by.equals(sort_by) ? (params.sort_dir.equals(SORT_DIR_ASC) ? SORT_DIR_DESC : SORT_DIR_ASC) : SORT_DIR_ASC);
    	params.sort_by = sort_by;
    	UIInternalLink.make(joint, "link", params);
    }
	
}