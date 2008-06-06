// Populate the test data
var testdata = {
	"context": "",
	"drafts": [
		{ "id": "3",
			"title": "Assignment 3",
			"sections": "A1",
			"openDate": "05/21/2008",
			"dueDate": "06/21/2008" },
		{ "id": "4",
			"title": "Assignment 4",
			"sections": "A2",
			"openDate": "05/22/2008",
			"dueDate": "06/22/2008" }
	],
	"posted": [
		{ "id": "1",
			"title": "Assignment 1",
			"sections": "A3",
			"openDate": "04/21/2008",
			"dueDate": "05/21/2008" },
		{ "id": "2",
			"title": "Assignment 2",
			"sections": "A4",
			"openDate": "04/22/2008",
			"dueDate": "05/22/2008" }
	]
};

var InstAsnnList = {
	context: null,
	navTemplate: null,
	draftTemplate: null,
	postedTemplate: null,

	show: function(context)
	{
		// if a context is provided, get the data from the server
		if (context)
		{
			InstAsnnList.context = context;

			// update newLink to include context
			jQuery('#newLink').attr('href', 'newassignment1.html?context=' + context + '&KeepThis=true&TB_iframe=true&width=800&height=600&modal=true');

			var url = '/sakai-assignment2-tool/sdata/asnnList?context=' + context;
			jQuery.getJSON(url, function(data)
			{
				InstAsnnList.paintAssignments(data);
			});
		}
		// with no context, use test data
		else
		{
			InstAsnnList.paintAssignments(testdata);
		}
	},

	paintAssignments: function(data)
	{
		jQuery('#nav_out').html(InstAsnnList.navTemplate.process(data));
		jQuery('#draft_out').html(InstAsnnList.draftTemplate.process(data));
		jQuery('#posted_out').html(InstAsnnList.postedTemplate.process(data));

		// set the context for all context fields in forms
		jQuery('input[name="context"]').val(data['context']);

		// add the toggle events to the twisties
		ListCommon.addToggle('#postedTwisty', '#postedList', true);
		ListCommon.addToggle('#draftsTwisty', '#draftsList', true);

		// Make the tables sortable
		jQuery("#draftAssns").tablesorter({headers: {0: {sorter: false}}});
		jQuery("#postedAssns").tablesorter();

		// make sure thickbox is applied
		tb_init('a.thickbox, area.thickbox, input.thickbox');

		// set the iframe to the fit the screen
		if (window.frameElement)
		{
			var height = jQuery("#" + window.frameElement.id, parent.document).height();
			jQuery("#" + window.frameElement.id, parent.document).height(Math.max(700, height));
//			setMainFrameHeight(window.frameElement.id);
		}
	},

	copyAsnn: function(asnnId)
	{
		var url = '/sakai-assignment2-tool/sdata/asnnCopy';
		var data = {context: InstAsnnList.context, id: asnnId};
		jQuery.post(url, data, function(data, textStatus)
		{
			InstAsnnList.show(InstAsnnList.context);
		});
	}
}

jQuery(document).ready(function()
{
	var qs = new Querystring();
	var context = qs.get('context');

	InstAsnnList.navTemplate = TrimPath.parseDOMTemplate('nav_template');
	InstAsnnList.draftTemplate = TrimPath.parseDOMTemplate('draft_template');
	InstAsnnList.postedTemplate = TrimPath.parseDOMTemplate('posted_template');

	InstAsnnList.show(context);
});
