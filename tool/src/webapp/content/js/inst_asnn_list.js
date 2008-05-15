jQuery(document).ready(function()
{
	var qs = new Querystring();

	var draftTemplate = new EJS({element: 'draft_template'});
	var postedTemplate = new EJS({element: 'posted_template'});
	var context = qs.get('context');

	// if a context is provided, get the data from the server
	if (context)
	{
		// set the iframe to the fit the screen
//		parent.setMainFrameHeight(parent.frames[0].name);
//		parent.setFocus(parent.focus_path);

		jQuery('#newLink').attr('href', 'newassignment1.html?context=' + context + '&KeepThis=true&TB_iframe=true&width=800&height=600&modal=true');

		var url = '/sakai-assignment2-tool/sdata/asnnList?context=' + context;
		jQuery.getJSON(url, function(data)
		{
			draftTemplate.update('draft_out', data);
			postedTemplate.update('posted_out', data);

			// add thickbox to rendered items
			tb_init('a.thickbox');
		});
	}
	// with no context, use test data
	else
	{
		// Populate the test data
		var testdata = {
			"context": "",
			"drafts": [
				{ "id" : "3", "title" : "Assignment 3", "sections" : "A1", "openDate" : "05/21/2008", "dueDate" : "06/21/2008" },
				{ "id" : "4", "title" : "Assignment 4", "sections" : "A2", "openDate" : "05/22/2008", "dueDate" : "06/22/2008" }
			],
			"posted": [
				{ "id" : "1", "title" : "Assignment 1", "sections" : "A3", "openDate" : "04/21/2008",  "dueDate" : "05/21/2008" },
				{ "id" : "2", "title" : "Assignment 2", "sections" : "A4", "openDate" : "04/22/2008", "dueDate" : "05/22/2008" }
			]
		};
		draftTemplate.update('draft_out', testdata);
		postedTemplate.update('posted_out', testdata);

		// add thickbox to rendered items
		tb_init('a.thickbox');
	}

	// add a hover effect to each row of data
	ListCommon.addHover('.dataRow', 'dataRowOn');

	// add the toggle events to the twisties
	ListCommon.addToggle('#postedTwisty', '#postedList', true);
	ListCommon.addToggle('#draftsTwisty', '#draftsList', true);

	// Make the tables sortable
	jQuery("#draftAssns").tablesorter({headers: {0: {sorter: false}}});
	jQuery("#postedAssns").tablesorter();
});
