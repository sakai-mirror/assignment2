package org.sakaiproject.assignment2.tool.taggable;

import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

/**
 * @author Andrew Thornton
 */

public class DebugHelperViewParameters extends SimpleViewParameters {
  /**
   * Name of the component, the value of which is the name of the sakai helper
   * to call
   */
  public static final String HELPER_ID = "helper-id";

  /**
   * Name of the component whose value is the method binding to call after the
   * helper has returned. This is in order to infer the action result, if any is
   * required.
   */
  public static final String POST_HELPER_BINDING = "helper-binding";

  public DebugHelperViewParameters() {
    super();
  }

  public DebugHelperViewParameters(String viewID) {
    super(viewID);
  }
}
