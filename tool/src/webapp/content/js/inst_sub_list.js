$(document).ready(function()
{
	// Populate the tables
	var testdata = {
		"assignments" : [
			{
				"id" : "3", "title" : "Assignment 3", "type" : "electronic",
				"submitted" : [
					{ "name" : "Carl Hall", "submittedOn" : "04/20/2008", "dueDate" : "04/21/2008", "sections" : "A1" },
					{ "name" : "Stuart Freeman", "submittedOn" : "04/19/2008", "dueDate" : "04/21/2008", "sections" : "A1" }
				],
				"completed" : [
					{ "name" : "Tom Jones", "returnedOn" : "04/22/2008", "returnedBy" : "Clay Fenlason", "feedback" : "Well done" },
					{ "name" : "Clay Fenlason", "returnedOn" : "04/25/2008", "returnedBy" : "Clay Fenlason", "feedback" : "Well done" }
				]
			},
			{
				"id" : "4", "title" : "Assignment 4", "type" : "non-electronic",
				"submitted" : [
					{"name" : "Carl Hall", "sections" : "A1", "feedback" : "good job" },
					{"name" : "Stuart Freeman", "sections" : "A1", "feedback" : "good job" }
				],
				"completed" : [
					{"name" : "Tom Jones", "returnedOn" : "04/22/2008", "returnedBy" : "Clay Fenlason", "feedback" : "Well done" },
					{"name" : "Clay Fenlason", "returnedOn" : "04/25/2008", "returnedBy" : "Clay Fenlason", "feedback" : "Well done" }
				]
			}
		]
	};

	new EJS({element: 'template'}).update('output', testdata);

	// add a hover effect to each row of data
	ListCommon.addHover('.dataRow', 'dataRowOn');

	// add the toggle events to the twisties
	var toggles = $('.twisty');
	for (var i=0; i<toggles.length; i++) {
		var show = (toggles[i].id.substring(0,9) == 'submitted')
		ListCommon.addToggle(toggles[i].id, toggles[i].id.replace('Twisty', 'List'), show);
	};

	$('.dataRow').click(function()
	{
		location.href='submissionview.html';
	});

	// accordion by assignment
	jQuery('#assignmentList').accordion({
		header: 'div.subheader',
		autoHeight: false,
		active: false,
		navigation: true
	});

	//Make tables sortable
	$(".tablesorter").tablesorter();

});
