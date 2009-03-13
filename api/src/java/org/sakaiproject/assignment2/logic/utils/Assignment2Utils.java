package org.sakaiproject.assignment2.logic.utils;

public class Assignment2Utils {

    public static final String MOZILLA_BR = "<br type=\"_moz\" />";

    /**
     * Attempts to remove all unnecessary tags from html strings.
     * 
     * @param cleanup an html string to clean up. may be null
     * @return the cleaned up string
     */
    public static String cleanupHtmlText(String cleanup) {
        if (cleanup == null) {
            // nulls are ok
            return null;
        } else if (cleanup.trim().length() == 0) {
            // nothing to do
            return cleanup;
        }
        cleanup = cleanup.trim();

        //remove the unnecessary <br type="_moz" />
        cleanup = cleanup.replace(MOZILLA_BR, "");

        return cleanup;
    }

}
