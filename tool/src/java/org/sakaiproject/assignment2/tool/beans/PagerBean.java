
package org.sakaiproject.assignment2.tool.beans;

public class PagerBean {
	
	private Integer currentStart = 0;
	private Integer currentCount = 5;
	private Integer totalCount = 16;
	
	private static final String PAGER_NAVIGATION = "pager_navigation";
	
	public Integer getCurrentStart(){
		return currentStart;
	}
	
	public void setCurrentStart(Integer currentStart){
		this.currentStart = currentStart;
	}
	
	public Integer getCurrentCount(){
		return currentCount;
	}
	
	public void setCurrentCount(Integer currentCount){
		this.currentCount = currentCount;
	}
	
	public Integer getTotalCount(){
		return totalCount;
	}
	
	public void setTotalCount(Integer totalCount){
		this.totalCount = totalCount;
	}
	
	public String getCurrentSelect(){
		return currentCount.toString();
	}
	
	public void setCurrentSelect(String currentSelect){
		this.currentCount = Integer.valueOf(currentSelect);
	}
	
	public String getViewingStart(){
		return Integer.toString(currentStart + 1);
	}
	
	public String getViewingEnd(){
		return Integer.toString((totalCount < (currentStart + currentCount)) ? totalCount : (currentStart + currentCount));
	}
	
	public String getViewingTotal(){
		return totalCount.toString();
	}
	
	
	//Form Submit Methods
	public void changePageSize(){
		//do nothing
	}
	
	public String goToFirstPage(){
		Integer newCurrentStart = 0;
		return newCurrentStart.toString();
	}
	
	public String goToPrevPage(){
		Integer newCurrentStart = currentStart - currentCount;
		if (newCurrentStart < 0) return this.goToFirstPage();
		return newCurrentStart.toString();
	}
	
	public String goToNextPage(){
		Integer newCurrentStart = currentStart + currentCount;
		if (newCurrentStart > totalCount) return this.goToLastPage();
		return newCurrentStart.toString();
	}
	
	public String goToLastPage(){
		Integer newCurrentStart = 0;
		if (totalCount > currentCount){
			newCurrentStart = totalCount - (totalCount % currentCount);
		} else {
			newCurrentStart = 0;
		}
		return newCurrentStart.toString(); 
	}
}