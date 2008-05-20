jQuery(document).ready(function()
{
	jQuery('#openDate').datepicker();
	jQuery('#dueDate').datepicker();
	jQuery('#acceptUntilDate').datepicker();
	jQuery("input[@name='openDateRadio']").change(function()
	{
//		alert(jQuery("input[@name='openDateRadio']:checked").val());
		if (jQuery("input[@name='openDateRadio']:checked").val() == "true")
		{
			jQuery('#openD').show();
		}
		else
		{
			jQuery('#openD').hide();
		}
	});
	jQuery("input[@name='dueDateRadio']").change(function()
	{
//		alert(jQuery("input[@name='dueDateRadio']:checked").val());
		if (jQuery("input[@name='dueDateRadio']:checked").val() == "true")
		{
			jQuery('#dueD').show();
		}
		else
		{
			jQuery('#dueD').hide();
		}
	});
	jQuery("input[@name='acceptUntilDateRadio']").change(function()
	{
		if (jQuery("input[@name='acceptUntilDateRadio']:checked").val() == "true")
		{
			jQuery('#acceptD').show();
		}
		else
		{
			jQuery('#acceptD').hide();
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
		jQuery('#id').val(id);
		jQuery('#context').val(context);
		var url = '/sakai-assignment2-tool/sdata/newAsnn2?id=' + id;
		jQuery.getJSON(url, function(data)
		{
			if (data['openDate'])
			{
				jQuery('#openDate').val(data['openDate']);
				jQuery('#openTime').val(data['openTime']);
				jQuery('#openLater').attr('checked', 'checked');
				jQuery("input[@name='openDateRadio']").change();
			}
			if (data['dueDate'])
			{
				jQuery('#dueDate').val(data['dueDate']);
				jQuery('#dueTime').val(data['dueTime']);
				jQuery('#dueDateYes').attr('checked', 'checked');
				jQuery("input[@name='dueDateRadio']").change();
			}
			if (data['acceptUntilDate'])
			{
				jQuery('#acceptUntilDate').val(data['acceptUntilDate']);
				jQuery('#acceptUntilTime').val(data['acceptUntilTime']);
				jQuery('#acceptUntilDateNo').attr('checked', 'checked');
				jQuery("input[@name='acceptUntilDateRadio']").change();
			}
			jQuery('#whoWillSubmit').val(data['whoWillSubmit']);
			jQuery('#grading').val(data['grading']);
		});
	}
});
