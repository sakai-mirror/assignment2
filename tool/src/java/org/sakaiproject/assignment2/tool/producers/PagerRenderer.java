package org.sakaiproject.assignment2.tool.producers;

import org.sakaiproject.assignment2.tool.producers.*;
import org.sakaiproject.assignment2.tool.params.PagerViewParams;
import org.sakaiproject.assignment2.tool.beans.PagerBean;

import uk.org.ponder.rsf.components.UIBoundList;
import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UICommand;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIForm;
import uk.org.ponder.rsf.components.UIInput;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UILink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UISelect;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;

public class PagerRenderer {

	private PagerBean pagerBean;
	
	private Integer pager_start;
	private Integer pager_count;
	
	private static final Integer DEFAULT_PAGER_START = 1;
	private static final Integer DEFAULT_PAGER_COUNT = 50;
	
    public void makePager(UIContainer tofill, String divID, String currentViewID, ViewParameters viewparams) {
    	PagerViewParams params = (PagerViewParams) viewparams;
    	//pager_start = (params.pager_start == null ? DEFAULT_PAGER_START : params.pager_start);
    	//pager_count = (params.pager_count == null ? DEFAULT_PAGER_COUNT : params.pager_count);
    	//PagerBean pagerBean = new PagerBean(pager_start.toString(), pager_count.toString());
    	
        UIJointContainer joint = new UIJointContainer(tofill, divID, "pagerDivContainer:", ""+1);
        
        //Currently Viewing
        UIMessage.make(joint, "pager_viewing", "assignment2.navbar.viewing", 
        		new Object[] {pagerBean.getViewingStart(), pagerBean.getViewingEnd(), pagerBean.getViewingTotal()} );
        
        //Form
        UIForm form = UIForm.make(joint, "pager_form");
                
        //Select Box
        UISelect select_box = UISelect.make(form, "pager_select_box");
        select_box.selection = new UIInput();
        select_box.selection.updateValue("#{pagerBean.currentSelect}");
        UIBoundList comboValues = new UIBoundList();
        comboValues.setValue(new String[] {"5","10","20","50","100","200"});
        select_box.optionlist = comboValues;
		UIBoundList comboNames = new UIBoundList();
		comboNames.setValue(new String[] {"Show 5 items", "Show 10 items", "Show 20 items", "Show 50 items", "Show 100 items", "Show 200 items"});
		select_box.optionnames = comboNames;
		
		//Paging Buttons
		UICommand first_page = UICommand.make(form, "pager_first_page", "#{pagerBean.goToFirstPage}");
		UICommand prev_page = UICommand.make(form, "pager_prev_page", "#{pagerBean.goToPrevPage}");
		UICommand next_page = UICommand.make(form, "pager_next_page", "#{pagerBean.goToNextPage}");
		UICommand last_page = UICommand.make(form, "pager_last_page", "#{pagerBean.goToLastPage}");
        
    }
    
    public void setPagerBean(PagerBean pagerBean){
    	this.pagerBean = pagerBean;
    }
}
