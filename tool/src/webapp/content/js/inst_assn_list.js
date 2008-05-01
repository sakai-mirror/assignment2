$(document).ready(function()
{
	// Populate the tables
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

	new EJS({element: 'draft_template'}).update('draft_out', testdata);
	new EJS({element: 'posted_template'}).update('posted_out', testdata);

	// add a hover effect to each row of data
	ListCommon.addHover('.dataRow', 'dataRowOn');

	// add the toggle events to the twisties
	ListCommon.addToggle('#postedTwisty', '#postedList', true);
	ListCommon.addToggle('#draftsTwisty', '#draftsList', true);

	// Make the tables sortable
	$("#draftAssns").tablesorter({headers: { 0: { sorter: false}}});
	$("#postedAssns").tablesorter();

});
