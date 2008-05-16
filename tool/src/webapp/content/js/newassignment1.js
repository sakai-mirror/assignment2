jQuery(document).ready(function()
{
	var qs = new Querystring();
	var context = qs.get('context');

	jQuery('#context').val(context);

	var id = qs.get('id');
	if (id)
	{
		jQuery('#id').val(id);
		var url = '/sakai-assignment2-tool/sdata/newAsnn1?id=' + id;
		jQuery.getJSON(url, function(data)
		{
			jQuery('#title').val(data['title']);
			jQuery('#instructions').val(data['instructions']);
		});
	}
});