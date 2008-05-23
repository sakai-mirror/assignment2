// Populate the test data
var testAsnn = {
	"assignments": [
		{"id": "1",
			"title": "Assignment 1",
			"type": "electronic"},
		{"id": "2",
			"title": "Assignment 2",
			"type": "non-electronic"},
		{"id": "3",
			"title": "Assignment 3",
			"type": "electronic"},
		{"id": "4",
			"title": "Assignment 4",
			"type": "non-electronic"}
	]
};
var testSubs1 = {
	"type": "electronic",
	"submitted": [
		{ "name": "Carl Hall",
			"submittedDate": "04/20/2008",
			"dueDate": "04/21/2008",
			"sections": "A1",
			"feedback": "" },
		{ "name": "Stuart Freeman",
			"submittedDate": "04/19/2008",
			"dueDate": "04/21/2008",
			"sections": "A1",
			"feedback": "" }
	],
	"returned": [
		{ "name": "Tom Jones",
			"returnedDate": "04/22/2008",
			"returnedBy": "Clay Fenlason",
			"sections": "A2",
			"feedback": "Well done"},
		{ "name": "Clay Fenlason",
			"returnedDate": "04/25/2008",
			"returnedBy": "Clay Fenlason",
			"sections": "A2",
			"feedback": "Well done"}
	]
};
var testSubs2 = {
	"type": "non-electronic",
	"submitted": [
		{"name": "Carl Hall",
			"sections": "A1",
			"feedback": "good job"},
		{"name": "Stuart Freeman",
			"sections": "A1",
			"feedback": "good job"}
	],
	"returned": [
		{"name": "Tom Jones",
			"returnedDate": "04/22/2008",
			"returnedBy": "Clay Fenlason",
			"feedback": "Well done"},
		{"name": "Clay Fenlason",
			"returnedDate": "04/25/2008",
			"returnedBy": "Clay Fenlason",
			"feedback": "Well done"}
	]
};

var InstSubList = {
	// constants
	RETURNED: 'Ret',
	SUBMITTED: 'Sub',

	// placeholders for template references.
	// these are populated by the init script run on document ready
	sidebarTemplate: null,
	nonElecRetTemp: null,
	elecRetTemp: null,
	nonElecSubTemp: null,
	elecSubTemp: null,
	bulkActionTemp: null,

	// placeholder for a context id, if passed through querystring
	context: null,

	// placeholder for the assignments list
	assignments: {},

	/**
	 * 
	 * @param asnnId
	 *        The id of an assignment to show the submission of
	 */
	showSubmissions: function(asnnId)
	{
		var asnn = InstSubList.assignments[asnnId];
		jQuery('#assn_' + asnnId).addClass('active');
		if (InstSubList.context)
		{
			var url = '/sakai-assignment2-tool/sdata/subList?asnnId=' + asnn['id'];
			jQuery.getJSON(url, function()
			{
				InstSubList.templateSubmission(asnn['type'],
						InstSubList.SUBMITTED, 'submitted_out', data);
				InstSubList.templateSubmission(asnn['type'],
						InstSubList.RETURNED, 'returned_out', data);
			});
		}
		else
		{
			InstSubList.templateSubmission(asnn['type'], InstSubList.SUBMITTED,
					'submitted_out', testSubs1);
			InstSubList.templateSubmission(asnn['type'], InstSubList.RETURNED,
					'returned_out', testSubs1);
		}
	},

	/**
	 * 
	 * @param type
	 *        Type of submission.  Expected: [electronic, non-electronic]
	 * @param status
	 *        Status of submission (returned, submitted)  Expected: [Ret, Sub]
	 * @param output
	 *        The area to receive the rendered output
	 * @param data
	 *        The data to use for transformation
	 */
	templateSubmission: function(type, status, output, data)
	{
		if (type == 'electronic')
		{
			jQuery('#' + output).html(InstSubList['elec' + status + 'Temp'].process(data));
		}
		else if (type == 'non-electronic')
		{
			jQuery('#' + output).html(InstSubList['nonElec' + status + 'Temp'].process(data));
		}
		//Make tables sortable
		jQuery(".tablesorter").tablesorter();
	},

	/**
	 * 
	 * @param assignments
	 *        List of assignments to cache using the id as a key for lookup
	 */
	cacheAssignments: function(assignments)
	{
		for (var i = 0; i < assignments.length; i++)
		{
			InstSubList.assignments[assignments[i].id] = assignments[i];
		}
	}
};

jQuery(document).ready(function()
{
	// Populate the tables
	var qs = new Querystring();
	InstSubList.context = qs.get('context');

	// populate the template references
	InstSubList.sidebarTemplate = TrimPath.parseDOMTemplate('sidebar_template');
	InstSubList.nonElecRetTemp = TrimPath.parseDOMTemplate('nonElectronicReturned_template');
	InstSubList.elecRetTemp = TrimPath.parseDOMTemplate('electronicReturned_template');
	InstSubList.nonElecSubTemp = TrimPath.parseDOMTemplate('nonElectronicSubmitted_template');
	InstSubList.elecSubTemp = TrimPath.parseDOMTemplate('electronicSubmitted_template');
	InstSubList.bulkActionTemp = TrimPath.parseDOMTemplate('bulkAction_template');

	// if a context is provided, get the data from the server
	if (InstSubList.context)
	{
		var url = '/sakai-assignment2-tool/sdata/subList?context=' + context;
		jQuery.getJSON(url, function(data)
		{
			// cache the data for submission lookup
			InstSubList.cacheAssignments(data['assignments']);

			// populate the sidebar with the list of assignments
			jQuery('#sidebar').html(InstSubList.sidebarTemplate.process(data));

			// show submissions of the first assignment
			if (asnn['assignments'].length > 0)
			{
				var asnn = data['assignments'][0];
				jQuery('#bulkAction_out').html(InstSubList.bulkActionTemp.process(asnn['id']));
				// go to server for submission list data.
				InstSubList.showSubmissions(asnn['id']);
			}
		});
	}
	// with no context, use test data
	else
	{
		// cache test data
		InstSubList.cacheAssignments(testAsnn['assignments']);
		// populate sidebar with assignments
		jQuery('#sidebar').html(InstSubList.sidebarTemplate.process(testAsnn));
		// show the submissions of the first assignment
		var asnn = testAsnn['assignments'][0];
		jQuery('#bulkAction_out').html(InstSubList.bulkActionTemp.process(asnn['id']));
		InstSubList.showSubmissions(asnn['id']);
	}

	// add a hover effect to each row of data
	ListCommon.addHover('.dataRow', 'dataRowOn');

	// add the toggle events to the twisties
	var toggles = jQuery('.twisty');
	for (var i=0; i<toggles.length; i++) {
		var show = (toggles[i].id.substring(0,9) == 'submitted')
		ListCommon.addToggle(toggles[i].id, toggles[i].id.replace('Twisty', 'List'), show);
	};

	jQuery('.dataRow').click(function()
	{
		location.href='submissionview.html';
	});

	//Make tables sortable
	jQuery(".tablesorter").tablesorter();

	// Set sidebar heights
	var document_height = jQuery(document).height() - 15;
	jQuery('#sidebar').height(document_height);
});
