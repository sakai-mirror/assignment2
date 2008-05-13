$(document).ready(function()
{
	var dataUrl = '/sakai-assignment2-tool/sdata/assnList?context=';
	var qs = new Querystring();

	var draftTemplate = new EJS({element: 'draft_template'});
	var postedTemplate = new EJS({element: 'posted_template'});
	var context = qs.get('context');

	// if a context is provided, get the data from the server
	if (context)
	{
		var url = dataUrl + context;
		draftTemplate.update('draft_out', url);
		postedTemplate.update('posted_out', url);
	}
	// with no context, use test data
	else
	{
		// Populate the test data
		var testdata = {
			"drafts" : [
				{ "id" : "3", "title" : "Assignment 3", "sections" : "A1", "openDate" : "05/21/2008", "dueDate" : "06/21/2008" },
				{ "id" : "4", "title" : "Assignment 4", "sections" : "A2", "openDate" : "05/22/2008", "dueDate" : "06/22/2008" }
			],
			"posted" : [
				{ "id" : "1", "title" : "Assignment 1", "sections" : "A3", "openDate" : "04/21/2008",  "dueDate" : "05/21/2008" },
				{ "id" : "2", "title" : "Assignment 2", "sections" : "A4", "openDate" : "04/22/2008", "dueDate" : "05/22/2008" }
			]
		};

		draftTemplate.update('draft_out', testdata);
		postedTemplate.update('posted_out', testdata);
	}

	// add a hover effect to each row of data
	ListCommon.addHover('.dataRow', 'dataRowOn');

	// add the toggle events to the twisties
	ListCommon.addToggle('#postedTwisty', '#postedList', true);
	ListCommon.addToggle('#draftsTwisty', '#draftsList', true);

	// Make the tables sortable
	$("#draftAssns").tablesorter({headers: { 0: { sorter: false}}});
	$("#postedAssns").tablesorter();

});
