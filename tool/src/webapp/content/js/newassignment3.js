jQuery(document).ready(function()
{
	jQuery("input[name='resubmissions']").change(function()
	{
		// the verbiage on the page makes the values seem backwards
		if (jQuery("input[name='resubmissions']:checked").val() == "2")
			jQuery('#numResubmitArea').removeClass('hidden');
		else
			jQuery('#numResubmitArea').addClass('hidden');
	});

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
			jQuery("input[name='resubmissions'][value='" + Math.min(2, data['resubmissions']) + "']").attr('checked', 'checked').change();
			if (data['resubmissions'] >= 2)
				jQuery('#numResubmit').val(data['resubmissions']);
			jQuery("input[name='notifications'][value='" + data['notifications'] + "']").attr('checked', 'checked').change();
			jQuery("input[name='honorcode'][value='" + data['honorCode'] + "']").attr('checked', 'checked').change();
		});
	}
});