package org.sakaiproject.assignment2.logic.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.sakaiproject.assignment2.logic.ExternalTaggableLogic;
import org.sakaiproject.component.cover.ComponentManager;
import org.sakaiproject.taggable.api.TaggableActivityProducer;
import org.sakaiproject.taggable.api.TaggingHelperInfo;
import org.sakaiproject.taggable.api.TaggingManager;
import org.sakaiproject.taggable.api.TaggingProvider;

/**
 * This class contains logic that is responsible for interacting with some 
 * taggable helpers.
 * @author chrismaurer
 *
 */
public class ExternalTaggableLogicImpl implements ExternalTaggableLogic {


	private TaggingManager taggingManager = null;
	private TaggableActivityProducer activityProducer = null;

	public ExternalTaggableLogicImpl() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public boolean isTaggable() {
		return getTaggingManager().isTaggable();
	}

	@Override
	public TaggableActivityProducer getMyProducer() {
		if (activityProducer == null) {
			activityProducer = (TaggableActivityProducer) ComponentManager.get(
					ACTIVITY_PRODUCER_ID);
		}
		return activityProducer;		 
	}

	@Override
	public List<TaggingProvider> getProviders() {
		return getTaggingManager().getProviders();
	}	

	private TaggingManager getTaggingManager() {
		if (taggingManager == null) {
			taggingManager = (TaggingManager) ComponentManager
			.get(TAGGING_MANAGER_ID);
		}
		return taggingManager;
	}

	@Override
	public List<TaggingHelperInfo> getActivityHelperInfo(String activityRef) {
		List<TaggingHelperInfo> activityHelpers = new ArrayList<TaggingHelperInfo>();
		for (TaggingProvider provider : getProviders()) {
			TaggingHelperInfo helper = provider.getActivityHelperInfo(activityRef);
			if (helper != null)
			{
				activityHelpers.add(helper);
			}
		}
		return activityHelpers;
	}

	public Map<String, List<TaggingHelperInfo>> getActivityHelperInfo(String siteId, List<String> activityRefs) {
		Map<String, List<TaggingHelperInfo>> returnMap = new HashMap<String, List<TaggingHelperInfo>>();
		for (TaggingProvider provider : getProviders()) {
			Map<String, TaggingHelperInfo> providerMap = new HashMap<String, TaggingHelperInfo>();
			providerMap = provider.getActivityHelperInfo(siteId, activityRefs);
			for (String key : providerMap.keySet()) {
				returnMap.get(key).add(providerMap.get(key));
			}			
		}
		return returnMap;
	}

	@Override
	public boolean isSiteAssociated(String context) {
		boolean isAllowed = false;
		for (TaggingProvider provider : getProviders()) {
			isAllowed = provider.allowViewTags(context);
			break;
		}
		return isAllowed;
	}
}