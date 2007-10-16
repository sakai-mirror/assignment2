
package org.sakaiproject.assignment2.tool.beans;

public class PagerBean {
	
	private Integer currentStart = 0;
	private Integer currentCount = 5;
	private Integer totalCount = 0;
	
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
	
	public void goToFirstPage(){
		currentStart = 0;
	}
	
	public void goToPrevPage(){
		currentStart = currentStart - currentCount;
		if (currentStart < 0) this.goToFirstPage();
	}
	
	public void goToNextPage(){
		currentStart = currentStart + currentCount;
		if (currentStart > currentCount) this.goToLastPage();
	}
	
	public void goToLastPage(){
		if (totalCount > currentCount){
			currentStart = totalCount - (totalCount % currentCount);
		} else {
			currentStart = 0;
		}
	}
}