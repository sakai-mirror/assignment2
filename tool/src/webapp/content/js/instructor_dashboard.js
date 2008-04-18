var Assn2 = function()
{
	return {
		doInit : function()
		{
			var url = 'http://localhost/sdata/assnInst';
			$.getJSON(url, function(data)
				{
					output = TrimPath.processDOMTemplate('returned_template', data);
					document.getElementById('returned_output').innerHTML = output;

					output = TrimPath.processDOMTemplate('submitted_template', data);
					document.getElementById('submitted_output').innerHTML = output;
				});
		}
	}
}();

Assn2.doInit();
