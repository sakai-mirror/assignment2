package org.sakaiproject.assignment2.tool.producers.renderers;

import java.util.Set;

import org.sakaiproject.assignment2.model.Assignment2;
import org.sakaiproject.assignment2.model.AssignmentAttachment;

import uk.org.ponder.rsf.components.UIContainer;
import uk.org.ponder.rsf.components.UIJointContainer;
import uk.org.ponder.rsf.components.UIMessage;
import uk.org.ponder.rsf.components.UIVerbatim;
import uk.org.ponder.rsf.producers.BasicProducer;
import uk.org.ponder.rsf.viewstate.ViewParameters;

/**
 * Renders the read only view of the Assignment Instructions. This currently
 * involves the Text and Attachments. It does not render the title of the 
 * assignment currently, as it is tooled to be used inside pages and displays.
 * 
 * TODO Currently is also tooled for the student submit view, but can probably 
 * be used on the Instructor side, as well as replacing some of the Thickbox
 * Fragments.
 * 
 * @author sgithens
 *
 */
public class AsnnInstructionsRenderer implements BasicProducer {

    // Dependency
    private ViewParameters viewParameters;
    public void setViewParameters(ViewParameters viewParameters) {
        this.viewParameters = viewParameters;
    }

    // Dependency
    private AttachmentListRenderer attachmentListRenderer;
    public void setAttachmentListRenderer (AttachmentListRenderer attachmentListRenderer) {
        this.attachmentListRenderer = attachmentListRenderer;
    }

    public void fillComponents(UIContainer parent, String clientID, Assignment2 assignment) {
        UIJointContainer joint = new UIJointContainer(parent, clientID, "assn2-assignment-instructions-widget:");

        /*
         * Assignments Instructions
         * 
         * If there are no instructions show the "No Instructions... etc" 
         * message.
         * 
         * If there are attachments render the supporting materials header and
         * contents. Otherwise don't show any attachments stuff.
         * 
         * TODO FIXME At the moment there are ghost attachments showing up on
         * all the assignments, I think is related to ASNN-204
         * 
         */
        if (assignment.getInstructions() == null || assignment.getInstructions().equals("")) {
            UIMessage.make(joint, "instructions", "assignment2.student-submit.no_instructions");
        }
        else {
            UIVerbatim.make(joint, "instructions", assignment.getInstructions());
        }

        if (assignment.getAttachmentSet() != null && assignment.getAttachmentSet().size() > 0) {
            UIMessage.make(joint, "attachments-header", "assignment2.student-submit.additional_resources");

            Set<AssignmentAttachment> attachments = (assignment != null) ? assignment.getAttachmentSet() : null;

            // TODO FIXME Why does this require the viewid?
            attachmentListRenderer.makeAttachmentFromAssignmentAttachmentSet(joint, "attachment_list:", viewParameters.viewID, 
                    attachments);
        }

    }

    public void fillComponents(UIContainer parent, String clientID) {

    }

}
