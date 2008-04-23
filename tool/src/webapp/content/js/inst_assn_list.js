$(document).ready(function()
{
	// add a hover effect to each row of data
	ListCommon.addHover('.dataRow', 'dataRowOn');

	// add the toggle events to the twisties
	ListCommon.addToggle('#postedTwisty', '#postedList', true);
	ListCommon.addToggle('#draftsTwisty', '#draftsList', true);

	// Make the tables sortable
	$("#draftAssns").tablesorter({headers: { 0: { sorter: false}}});
	$("#postedAssns").tablesorter();

});
