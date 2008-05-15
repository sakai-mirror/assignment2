jQuery(document).ready(function()
{
	var qs = new Querystring();
	var context = qs.get('context');

	jQuery('#context').val(context);
});