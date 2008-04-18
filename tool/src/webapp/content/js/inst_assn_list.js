$(document).ready(function()
{
	// add a hover effect to each row of data
	ListCommon.addHover('.dataRow', 'dataRowOn');

	// add the toggle events to the twisties
	ListCommon.addToggle('#publishedTwisty', '#publishedList', true);
	ListCommon.addToggle('#draftsTwisty', '#draftsList', true);
});