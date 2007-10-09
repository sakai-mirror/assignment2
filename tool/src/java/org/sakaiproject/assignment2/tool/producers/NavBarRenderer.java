package org.sakaiproject.assignment2.tool.producers;

import uk.org.ponder.rsf.components.UIBranchContainer;
import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIInternalLink;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UILink;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIOutput;
import uk.org.ponder.rsf.viewstate.SimpleViewParameters;

public class NavBarRenderer {

    public void makeNavBar(UIContainer tofill, String divID, String currentViewID) {
        UIJointContainer joint = new UIJointContainer(tofill, divID, "navigation-ul:", ""+1);

    }
}
