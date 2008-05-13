var ListCommon = function()
{
	return {
		addToggle: function(twisty, area, normallyOpen)
		{
			var onClose = function()
			{
				$(twisty + ' > img:first').attr('src', '../images/widget1-collapse-arrow.png');
				$(area).slideUp('fast');
			};
			var onOpen = function()
			{
				$(twisty + ' > img:first').attr('src', '../images/widget1-arrow.png');
				$(area).slideDown('fast');
			};
			if (normallyOpen)
				$(twisty).toggle(onClose, onOpen);
			else
				$(twisty).toggle(onOpen, onClose);
		},

		addHover: function(selector, styleClass)
		{
			$(selector).hover(
				function()
				{
					$(this).addClass(styleClass);
				},
				function()
				{
					$(this).removeClass(styleClass);
				});
		}
	}
}();