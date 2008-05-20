jQuery(document).ready(function()
{
	var qs = new Querystring();
	var id = qs.get('id');
	var context = qs.get('context');

	// if no id, start the workflow over
	if (!id)
	{
		window.location.href = '/sakai-assignment2-tool/content/templates/newassignment1.html?context=' + context;
	}
	else
	{
		jQuery('#id').val(id);
		jQuery('#context').val(context);
		var url = '/sakai-assignment2-tool/sdata/newAsnn3?id=' + id;
		jQuery.getJSON(url, function(data)
		{
			switch (data['resubmissions'])
			{
				case -1:
					jQuery('#perpetual').attr('checked', 'checked');
					break;
				case 1:
					jQuery('#oneSubmit').attr('checked', 'checked');
					break;
				default:
					jQuery('#resubmitLimit').attr('checked', 'checked');
					break;
			}
			switch (data['notifications'])
			{
				case 0:
					jQuery('#dontSend').attr('checked', 'checked');
					break;
				case 1:
					jQuery('#sendEach').attr('checked', 'checked');
					break;
				case 2:
					jQuery('#sendDigest').attr('checked', 'checked');
					break;
			}
			switch (data['honorCode'])
			{
				case true:
					jQuery('#honor').attr('checked', 'checked');
					break;
				case false:
					jQuery('#noHonor').attr('checked', 'checked');
					break;
			}
		});
	}
});