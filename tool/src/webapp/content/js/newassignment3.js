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
			jQuery("input[name='resubmissions'][value='" + data['resubmissions'] + "']").attr('checked', 'checked');
			jQuery("input[name='notifications'][value='" + data['notifications'] + "']").attr('checked', 'checked');
			jQuery("input[name='honorcode'][value='" + data['honorCode'] + "']").attr('checked', 'checked');
		});
	}
});