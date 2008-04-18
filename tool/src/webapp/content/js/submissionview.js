var InstSubmView = {
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
