// Populate the test data
var testdata = {
	"id" : "3",
	"title" : "Assignment 3",
	"type" : "electronic",
	"editSub" : "false",
	"editFeedback" : "true",
	"assignment" : "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
	"drafts" : [
		{ "id" : "first draft", "content" : "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."}
	],
	"submission" : "Description: Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.",
	"attachments" : [
		{ "id" : "Untitled.doc", "url" : "http://example.com/Untitled.doc" }
	],
	"resources" : [
		{ "id" : "MyFeed", "url" : "http://lipsum.com/feed/lipsum.com/feed/html" }
	],
	"feedback" : "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.",
	"history" : [
		{ "revision" : "1", "date" : "1/1/1970", "url" : "http://example.com/rev/1" }
	]
};

var InstSubmView = {
	addToggle : function(id, alreadyClosed)
	{
		var contentId = id + '-content';
		var toggleId = id + '-toggle';
		if (alreadyClosed) {
				jQuery(toggleId).toggle(
				function()
				{
					jQuery(this).text("–");
					jQuery(contentId).slideDown('fast');
				},
				function()
				{
					jQuery(this).text('+');
					jQuery(contentId).slideUp('fast');
				});
		} else {
			jQuery(toggleId).toggle(
				function()
				{
        	jQuery(this).text('+');
        	jQuery(contentId).slideUp('fast');
				},
 				function()
				{
					jQuery(this).text("–");
					jQuery(contentId).slideDown('fast');
				});
			}
	}
};

jQuery(document).ready(function()
{
	var qs = new Querystring();

	var template = TrimPath.parseDOMTemplate('template');
	var asnnId = qs.get('asnnId');
	var subId = qs.get('subId');

	// if an assignment or submission Id is provided, get the data from the server
	if (asnnId || subId)
	{
		var url = '/sakai-assignment2-tool/sdata/subView?asnnId=' + asnnId + '&subId=' + subId;
		jQuery.getJSON(url, function(data)
		{
//			alert(template.process(data));
			jQuery('#output').html(template.process(data));
		});
	}
	// with no context, use test data
	else
	{
		jQuery('#output').html(template.process(testdata));
	}

	InstSubmView.addToggle('#assignment', true);
	InstSubmView.addToggle('#drafts', true);
	InstSubmView.addToggle('#submission', false);
	InstSubmView.addToggle('#feedback', false);
	InstSubmView.addToggle('#history', true);
});
