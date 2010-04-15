package org.sakaiproject.assignment2.tool.beans;

import java.util.List;

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
    
    private Cache sortedStudentIdsCache = null;
    
    public List<String> getSortedStudentIds(String curUserID, long asnnId) {
        String key = curUserID+asnnId;
        List<String> togo = null;
        if (sortedStudentIdsCache.containsKey(key)) {
            return (List<String>) sortedStudentIdsCache.get(key);
        }
        return togo;
    }
    
    public void setSortedStudentIds(String curUserID, long asnnId, List<String> studentIds) {
        sortedStudentIdsCache.put(curUserID+asnnId, studentIds);
    }

    public void init() {
        sortedStudentIdsCache = memoryService.newCache(this.getClass().getCanonicalName()+".sortedStudentIdsCache");
    }
}
