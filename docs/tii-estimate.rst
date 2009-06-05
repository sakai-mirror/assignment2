
Configuration and Setup
=======================

Task: Create the DDL for altering the database from our schema changes.

Turn In It Admin and Provisioning
=================================

Task: Determine how we will pick the contact instructor for each TII course. This
is mostly social engineering and IU process. TII can only have One contact instructor
per course.  The rest of the provisioning, such as students and courses is already
implemented in the TII-ContentReview-impl, and while it may need small tweaking
works pretty good. This may also be a dummy/ghost user?

ContentReview-Impl
==================

Task: Modify and extend the service to specify which instructor account to sync.
Currently a property controls one user to set as the instructor for the entire 
university.

Task: Modify and add signatures to ContentReview API to take in settings for the assignment.
This is likely going to be all the items in the Add/Edit assignment drop down.  This could
potentially be done, by adding a Map as the last argument for implementation specific 
properties.

Task: Modify the TurnitinContentImplementation.java to actually use those extra properties.
Basically this means settings like which Repository to use, time to submit originality report.

Model Layer
===========

Task:  Create a 1:1 table or extra columns on the AssignmentSubmissionAttachment
object to track the ContentReview/TII/Other review ID. This may also end up being
a 1:many table if we decide that that a single Attachment can be reviewed by 
multiple things.  This table may have a column that specified the review Type
such as TII, or generic ContentReview, or Kuali Workflow.

Task: We anticipate there to be no changes to teh AssignmentSubmissionVersion object, 
unless we decide that the Submitted Text can be reviewed as well.

Task: For the Assignment2 object and table we will need a custom table to capture
the TII Assignment specific settings. We have the discussed that this may be able to 
go in the TII-ContentReview-impl area, that would require some more poke throughs in the
ContentReview API.

Task: Create a model notation for specifying the TII options that are present when you add/edit/save
an assignment.  This could either be a model object, such as TIIOptions, or just a set of Key names
for a Map of properties.  We don't really want to put this on the Assignment2 object as a property (
ex. class Assignment2 {
  private Long id;
  etc
  etc
  TIIOptions tiiOptions;
}
)
However, it would be better to have an external logic utility to build this up. We might have to do this
as 2 hibernate queries starting out. Maybe this should be in a properties table too.

Task: Update ER Diagram with Highlighted changes

Service Layer
=============

Task: Saving a new assignment
1) ContentReviewService.isSiteAcceptable(site), show error if not
2) Save assignment as usual
   This will require sending in a list of TII properties in addition to the regular save items.
   TODO: Reread Steve Yegge's essay on the the Ultimate Design Pattern

Task: Deleting/Editing an assignment
1) We have no idea yet how changing the properties of a TII assignment will affect TII if assignments
have already been submitted.
2) If we use properties to capture the Assignment TII settings, we will include an explicit property detailing
whether or not TII is in use, rather than depend solely on the absence of a property.

Task: Fetching an assignment
1) It seems like we are going to go the properties route, so fetching assignments will now require
querying for their properties, and setting the options property on the Assignment2 object. 

Task: Make a graph or comparsison of how our versions with match against TII versioning

Task: Submittting an assignment
0.5) What do we do if it's text only assignment and they try to use TII??
1) Save the Submission and Version as usual
2) Queue the version in ContentReviewService
3) Figure out if we have to persist the return ID from CRS ourselves or not. Look into that API.


GUI Layer
=========

Task: Determine exactly the algorithm for calculating the barometers or stacks of
paper icons for the Instructor Assignment Submissions. The problem is that, there can
be multiple attachments, etc, and we are not sure how to aggregate those into 1 
value for the student listing submissions screen. May require consulting with Lynn.


Task: Where will we capture the originality scores. Will we go to the ContentReview 
service each time we need them, or mirror them on the AssignmentSubAttachment objects.
It could be costly to get them each time. Perhaps we could register a listener so that
the A2 tables are updated when the quartz job runs.
