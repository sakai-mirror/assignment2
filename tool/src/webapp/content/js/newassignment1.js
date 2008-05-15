$(document).ready(function()
{
	var qs = new Querystring();
	var context = qs.get('context');

	$('#context').val(context);
});