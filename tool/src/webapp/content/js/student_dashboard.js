var Assn2 = function()
{
	return {
		doInit : function()
		{
			var url = 'http://localhost/sdata/assnStud';
			$.getJSON(url, function(data)
				{
					var output = TrimPath.processDOMTemplate('due_template', data);
					document.getElementById('due_output').innerHTML = output;

					output = TrimPath.processDOMTemplate('returned_template', data);
					document.getElementById('returned_output').innerHTML = output;

					output = TrimPath.processDOMTemplate('submitted_template', data);
					document.getElementById('submitted_output').innerHTML = output;
				});
		}
	}
}();

Assn2.doInit();
