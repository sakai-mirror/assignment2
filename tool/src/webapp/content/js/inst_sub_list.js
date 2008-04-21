$(document).ready(function()
{
	// add a hover effect to each row of data
	ListCommon.addHover('.dataRow', 'dataRowOn');

	// add the toggle events to the twisties
	ListCommon.addToggle('#submitted1Twisty', '#submitted1List', true);
	ListCommon.addToggle('#completed1Twisty', '#completed1List');

	ListCommon.addToggle('#submitted2Twisty', '#submitted2List', true);
	ListCommon.addToggle('#completed2Twisty', '#completed2List');

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

});
