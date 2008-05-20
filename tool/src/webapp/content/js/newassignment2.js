jQuery(document).ready(function()
{
	jQuery('#openDate').datepicker();
	jQuery('#dueDate').datepicker();
	jQuery('#acceptUntilDate').datepicker();
	jQuery('#dueDate').hide();
	jQuery('#dueTime').hide();
	jQuery('#acceptUntilDate').hide();
	jQuery('#acceptUntilTime').hide();
	jQuery("input[@name='dueDateRadio']").change(function()
	{
		if (jQuery("input[@name='dueDateRadio']:checked").val() == "1")
		{
			jQuery('#dueDate').show();
			jQuery('#dueTime').show();
		}
		else
		{
			jQuery('#dueDate').hide();
			jQuery('#dueTime').hide();
		}
	});
	jQuery("input[@name='acceptUntilRadio']").change(function()
	{
		if (jQuery("input[@name='acceptUntilRadio']:checked").val() == "1")
		{
			jQuery('#acceptUntilDate').show();
			jQuery('#acceptUntilTime').show();
		}
		else
		{
			jQuery('#acceptUntilDate').hide();
			jQuery('#acceptUntilTime').hide();
		}
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
		var url = 'sakai-assignment2-tool/sdata/newAsnn2?id=' + id;
		jQuery.getJSON(url, function(data)
		{
			jQuery('#openDate').val(data['openDate']);
			jQuery('#openTime').val(data['openTime']);
			jQuery('#dueDate').val(data['dueDate']);
			jQuery('#dueTime').val(data['dueTime']);
			jQuery('#acceptUntilDate').val(data['acceptUntilDate']);
			jQuery('#acceptUntilTime').val(data['acceptUntilTime']);
			jQuery('#whoWillSubmit').val(data['whoWillSubmit']);
			jQuery('#grading').val(data['grading']);
		});
	}
});
