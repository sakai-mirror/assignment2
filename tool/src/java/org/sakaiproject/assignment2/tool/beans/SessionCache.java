package org.sakaiproject.assignment2.tool.beans;

import java.util.List;

import org.sakaiproject.assignment2.tool.entity.Assignment2SubmissionEntityProvider;
import org.sakaiproject.memory.api.Cache;
import org.sakaiproject.memory.api.MemoryService;

/**
 * This will cache information for various things during a users session that
 * we don't want in session storage, and don't have a database table for.
 * 
 * @author sgithens
 *
 */
public class SessionCache {

    private MemoryService memoryService;
    public void setMemoryService(MemoryService memoryService) {
        this.memoryService = memoryService;
    }
    
    private Assignment2SubmissionEntityProvider assignment2SubmissionEntityProvider;
    public void setAssignment2SubmissionEntityProvider(
            Assignment2SubmissionEntityProvider assignment2SubmissionEntityProvider) {
        this.assignment2SubmissionEntityProvider = assignment2SubmissionEntityProvider;
    }
    
    private Cache sortedStudentIdsCache = null;
    
    public List<String> getSortedStudentIds(String curUserID, long asnnId, String placementId) {
        String key = curUserID+asnnId+placementId;
        List<String> studentIds = null;
        if (sortedStudentIdsCache.containsKey(key)) {
            studentIds = (List<String>) sortedStudentIdsCache.get(key);
        }
        if (studentIds == null) {
            assignment2SubmissionEntityProvider.getEntitiesWithStoredSessionState(asnnId, placementId);
            studentIds = (List<String>) sortedStudentIdsCache.get(key);
        }
        return studentIds;
    }
    
    public void setSortedStudentIds(String curUserID, long asnnId, List<String> studentIds, String placementId) {
        sortedStudentIdsCache.put(curUserID+asnnId+placementId, studentIds);
    }

    public void init() {
        sortedStudentIdsCache = memoryService.newCache(this.getClass().getCanonicalName()+".sortedStudentIdsCache");
    }
}
