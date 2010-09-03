package org.sakaiproject.assignment2.tool;

import uk.org.ponder.messageutil.TargettedMessage;

/**
 * This is a custom version of TargettedMessage that always returns false when
 * asked if it is an error. This is so we can use the same html templates and
 * what not for the error code, but in certain situations, especially in wizards
 * with flow beans, we don't want the error processing cycle to stop. We want to
 * still present a styled 'error' message, but we want the full bean cycle to 
 * still run so we can stay on that page of the wizard and do extra processing.
 * 
 * There are also complex elements outside of regular html forms that don't get
 * automatically filled back in by the browser if we don't populate the beans
 * and do them ourselves.
 * 
 * There is support for overriding the message error renderer in RSF which we 
 * may want to do if our total number of message types grows.
 * 
 * It is worth going back to look at the original implementation of isError which
 * is really cunning. Essentially, the idea going forward till the end of time
 * is that odd message types are errors, and even ones are informational.
 * Hence the bitwise AND.
 * 
 * @author sgithens
 *
 */
public class NoErrorTargettedMessage extends TargettedMessage {
    
    public NoErrorTargettedMessage(String messagecode) {
        super(messagecode);
    }
    
    public NoErrorTargettedMessage(String messagecode, Object[] args) {
        super(messagecode, args);
    }
    
    public NoErrorTargettedMessage(String messagecode, Object[] args, int severity) {
        super(messagecode, args, severity);
    }
    
    public NoErrorTargettedMessage(String messagecode, Object[] args, String target) {
        super(messagecode, args, target);
    }

    public boolean isError() {
        return false;
    }
}
