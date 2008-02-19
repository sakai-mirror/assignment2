package org.sakaiproject.assignment2.tool.producers.renderers;

import org.sakaiproject.assignment2.logic.ExternalLogic;
import org.sakaiproject.assignment2.tool.params.PagerViewParams;
import org.sakaiproject.assignment2.tool.params.AssignmentListSortViewParams;
import org.sakaiproject.assignment2.tool.producers.AssignmentListSortViewProducer;

import uk.org.ponder.messageutil.MessageLocator;
import uk.org.ponder.rsf.components.ELReference;
import uk.org.ponder.rsf.components.UIBoundList;
import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UICommand;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIELBinding;
import uk.org.ponder.rsf.components.UIForm;
import uk.org.ponder.rsf.components.UIInput;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UILink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.components.UISelect;
import uk.org.ponder.rsf.components.decorators.DecoratorList;
import uk.org.ponder.rsf.components.decorators.UIFreeAttributeDecorator;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;
import uk.org.ponder.rsf.viewstate.ViewParameters;
import java.util.HashMap;
import java.util.Map;

public class PagerRenderer {

	private MessageLocator messageLocator;
	private ExternalLogic externalLogic;
	
	public static final int DEFAULT_START_COUNT = 50;
	
	public Integer currentStart = 0;
	public Integer currentCount = DEFAULT_START_COUNT;		//actually set in the pager view parameters :-)
	public Integer totalCount = 16;
	
	@SuppressWarnings("unchecked")
	public void makePager(UIContainer tofill, String divID, String currentViewID, ViewParameters viewparams, Integer totalCount) {
    	PagerViewParams pagerparams = (PagerViewParams) viewparams;

    	if (totalCount <= PagerRenderer.DEFAULT_START_COUNT){
    		//Do not show
    		return;
    	}
    	
    	//set vars
    	this.currentCount = pagerparams.current_count;
    	this.currentStart = pagerparams.current_start;
    	this.totalCount = totalCount;
    	
        UIJointContainer joint = new UIJointContainer(tofill, divID, "pagerDivContainer:", ""+1);
        
        //Currently Viewing
        UIMessage.make(joint, "pager_viewing", "assignment2.pager.viewing", 
        		new Object[] {this.getViewingStart(), this.getViewingEnd(), this.getViewingTotal()} );
        
        //Form
        UIForm form = UIForm.make(joint, "pager_form");
                
		//Paging Buttons
		////////////////////
		String href_params = "";
		//If we are on the Assignment_list-sortview page... add in the view params to keep the sorting accurate
		if (AssignmentListSortViewProducer.VIEW_ID.equals(currentViewID)){
			AssignmentListSortViewParams sortparams = (AssignmentListSortViewParams) viewparams;
			href_params = "sort_dir=" + sortparams.sort_dir + "&sort_by=" + sortparams.sort_by + "&";
		}		
		//build url
		String url = externalLogic.getAssignmentViewUrl(currentViewID);

		//Select Box
        UISelect select_box = UISelect.make(form, "pager_select_box");
        UIInput selection = new UIInput();
        selection.setValue(this.getCurrentSelect());
        select_box.selection = selection;
        //select_box.selection.valuebinding = new ELReference("#{PagerBean.currentSelect}");
        UIBoundList comboValues = new UIBoundList();
        comboValues.setValue(new String[] {"5","10","20","50","100","200"});
        select_box.optionlist = comboValues;
		UIBoundList comboNames = new UIBoundList();
		comboNames.setValue(new String[] {"Show 5 items", "Show 10 items", "Show 20 items", "Show 50 items", "Show 100 items", "Show 200 items"});
		select_box.optionnames = comboNames;
		Map attrmap = new HashMap(); 
		attrmap.put("onchange", "location.href=\"" + url + "?" + href_params + "current_start=" + 
				currentStart.toString() + "&current_count=\" + jQuery(this).val()");
		select_box.decorators = new DecoratorList(new UIFreeAttributeDecorator(attrmap)); 
		
		
		//first page
		UIInput first_page = UIInput.make(form, "pager_first_page", null, messageLocator.getMessage("assignment2.pager.pager_first_page"));
		attrmap = new HashMap();
		attrmap.put("onclick", "location.href=\"" + url + "?" + href_params + "current_start=" + 
					this.goToFirstPage() + "&current_count=" + this.getCurrentSelect() + "\"");
		first_page.decorators = new DecoratorList(new UIFreeAttributeDecorator(attrmap));
				
		//previous page
		UIInput prev_page = UIInput.make(form, "pager_prev_page", null, messageLocator.getMessage("assignment2.pager.pager_prev_page"));
		attrmap = new HashMap();
		attrmap.put("onclick", "location.href=\"" + url + "?" + href_params + "current_start=" + 
				this.goToPrevPage() + "&current_count=" + this.getCurrentSelect() + "\"");
		prev_page.decorators = new DecoratorList(new UIFreeAttributeDecorator(attrmap));

		//next page
		UIInput next_page = UIInput.make(form, "pager_next_page", null, messageLocator.getMessage("assignment2.pager.pager_next_page"));
		attrmap = new HashMap();
		attrmap.put("onclick", "location.href=\"" + url + "?" + href_params + "current_start=" + 
				this.goToNextPage() + "&current_count=" + this.getCurrentSelect() + "\"");
		next_page.decorators = new DecoratorList(new UIFreeAttributeDecorator(attrmap));
		
		//last button
		UIInput last_page = UIInput.make(form, "pager_last_page", null, messageLocator.getMessage("assignment2.pager.pager_last_page"));
		attrmap = new HashMap();
		attrmap.put("onclick", "location.href=\"" + url + "?" + href_params + "current_start=" + 
				this.goToLastPage() + "&current_count=" + this.getCurrentSelect() + "\"");
		last_page.decorators = new DecoratorList(new UIFreeAttributeDecorator(attrmap));
		
		
		//Disable buttons not in use
		Map disabledAttr = new HashMap();
		disabledAttr.put("disabled", "disabled");
		DecoratorList disabledDecoratorList = new DecoratorList(new UIFreeAttributeDecorator(disabledAttr));
		
		if (currentStart == 0){
			//disable if on first page
			first_page.decorators = disabledDecoratorList;
			prev_page.decorators = disabledDecoratorList;
		}
		if ((currentStart + currentCount) >= totalCount){
			//disable if on last page
			next_page.decorators = disabledDecoratorList;
			last_page.decorators = disabledDecoratorList;
		}
        
    }
    
    public void setMessageLocator(MessageLocator messageLocator) {
        this.messageLocator = messageLocator;
    }
    
    public void setExternalLogic(ExternalLogic externalLogic) {
    	this.externalLogic = externalLogic;
    }
    
    
    //Private Methods
	private String getViewingStart(){
		return Integer.toString(currentStart + 1);
	}
	
	private String getViewingEnd(){
		return Integer.toString((totalCount < (currentStart + currentCount)) ? totalCount : (currentStart + currentCount));
	}
	
	private String getViewingTotal(){
		return totalCount.toString();
	}
	
	private String getCurrentSelect(){
		return currentCount.toString();
	}
	
	//Form Submit Methods
	private void changePageSize(){
		//do nothing
	}
	
	private String goToFirstPage(){
		Integer newCurrentStart = 0;
		return newCurrentStart.toString();
	}
	
	private String goToPrevPage(){
		Integer newCurrentStart = currentStart - currentCount;
		if (newCurrentStart < 0) return this.goToFirstPage();
		return newCurrentStart.toString();
	}
	
	private String goToNextPage(){
		Integer newCurrentStart = currentStart + currentCount;
		if (newCurrentStart > totalCount) return this.goToLastPage();
		return newCurrentStart.toString();
	}
	
	private String goToLastPage(){
		Integer newCurrentStart = 0;
		if (totalCount > currentCount){
			newCurrentStart = totalCount - (totalCount % currentCount);
		} else {
			newCurrentStart = 0;
		}
		return newCurrentStart.toString(); 
	}
}
