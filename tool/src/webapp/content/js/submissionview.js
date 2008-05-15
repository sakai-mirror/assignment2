var InstSubmView = {
	var dataUrl = '/sakai-assignment2-tool/sdata/subView?context=';
	var qs = new Querystring();

	var template = new EJS({element: 'template'});
	var context = qs.get('context');

	// if a context is provided, get the data from the server
	if (context)
	{
		var url = dataUrl + context;
		template.update('output', url);
	}
	// with no context, use test data
	else
	{
		// Populate the test data
		var testdata = {
			"id" : "3", "title" : "Assignment 3", "type" : "electronic", "editSub" : "false", "editFeedback" : "true",
			"assignment" : "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
			"drafts" : [
				{ "id" : "first draft", "content" : "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
				}
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

		template.update('output', testdata);
	}	

	addToggle : function(id, alreadyClosed)
	{
		var contentId = id + '-content';
		var toggleId = id + '-toggle';
		if (alreadyClosed) {
				$(toggleId).toggle(
				function()
				{
					$(this).text("–");
					$(contentId).slideDown('fast');
				},
				function()
				{
					$(this).text('+');
					$(contentId).slideUp('fast');
				});
		} else {
			$(toggleId).toggle(
				function()
				{
        	$(this).text('+');
        	$(contentId).slideUp('fast');
				},
 				function()
				{
					$(this).text("–");
					$(contentId).slideDown('fast');
				});
			}
	},
	init : function()
	{
		InstSubmView.addToggle('#assignment', true);
		InstSubmView.addToggle('#drafts', true);
		InstSubmView.addToggle('#submission', false);
		InstSubmView.addToggle('#feedback', false);
		InstSubmView.addToggle('#history', true);
	}
};

$(document).ready(function()
{
	InstSubmView.init();
});