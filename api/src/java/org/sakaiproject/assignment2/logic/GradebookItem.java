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

package org.sakaiproject.assignment2.logic;

import java.util.Date;

/**
 * A GradebookItem object for supplying data from the gradebook
 * to the UI. This object is not persisted.
 * 
 * @author <a href="mailto:wagnermr@iupui.edu">michelle wagner</a>
 */
public class GradebookItem {

    private Long gradableObjectId;
    private String title;
    private Double pointsPossible;
    private Date dueDate;
    private String externalId;
    private boolean released;
    
    public GradebookItem() {
    	
    }
    
	public GradebookItem(Long gradableObjectId, String title,
			Double pointsPossible, Date dueDate, boolean released) {
		this.gradableObjectId = gradableObjectId;
		this.title = title;
		this.pointsPossible = pointsPossible;
		this.dueDate = dueDate;
		this.released = released;
	}

	/**
	 * 
	 * @return the gradableObjectId for this gradebook item
	 */
	public Long getGradableObjectId() {
		return gradableObjectId;
	}

	/**
	 * set the gradableObjectId for this gradebook item
	 * @param gradableObjectId
	 */
	public void setGradableObjectId(Long gradableObjectId) {
		this.gradableObjectId = gradableObjectId;
	}

	/**
	 * 
	 * @return the title of this gradebook item
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * set the title of this gradebook item
	 * @param title
	 */
	public void setTitle(String title) {
		this.title = title;
	}

	/**
	 * 
	 * @return the points possible for this gradebook item
	 */
	public Double getPointsPossible() {
		return pointsPossible;
	}

	/**
	 * set the points possible for this gradebook item
	 * @param pointsPossible
	 */
	public void setPointsPossible(Double pointsPossible) {
		this.pointsPossible = pointsPossible;
	}

	/**
	 * 
	 * @return the due date for this gradebook item
	 */
	public Date getDueDate() {
		return dueDate;
	}

	/**
	 * set the due date for this gradebook item
	 * @param dueDate
	 */
	public void setDueDate(Date dueDate) {
		this.dueDate = dueDate;
	}

	/**
	 * 
	 * @return the id representing the external maintainer of this gb item.
	 * used ONLY for import from old assignments tool
	 */
	public String getExternalId()
	{
		return externalId;
	}

	/**
	 * the id representing the external maintainer of this gb item.
	 * used ONLY for import from old assignments tool
	 * @param externalId
	 */
	public void setExternalId(String externalId)
	{
		this.externalId = externalId;
	}

	/**
	 * 
	 * @return true if the grades in this gradebook item have been released
	 * to the students
	 */
    public boolean isReleased()
    {
        return released;
    }

    /**
     * true if the grades in this gradebook item have been released
     * to the students
     * @param released
     */
    public void setReleased(boolean released)
    {
        this.released = released;
    }
}
