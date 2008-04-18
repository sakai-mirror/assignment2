var InstNewAssnView = {
	init : function()
	{
		$('#openDate').datepicker();
		$('#dueDate').datepicker();
		$('#acceptUntilDate').datepicker();
		$('#dueDate').hide();
		$('#dueTime').hide();
		$('#acceptUntilDate').hide();
		$('#acceptUntilTime').hide();
		$("input[@name='dueDateRadio']").change (
			function()
			{
				if ($("input[@name='dueDateRadio']:checked").val() == "1")
				{
					$('#dueDate').show();
					$('#dueTime').show();
				}
				else
				{
					$('#dueDate').hide();
					$('#dueTime').hide();
				}
			}
		);
		$("input[@name='acceptUntilRadio']").change (
			function()
			{
				if ($("input[@name='acceptUntilRadio']:checked").val() == "1")
				{
					$('#acceptUntilDate').show();
					$('#acceptUntilTime').show();
				}
				else
				{
					$('#acceptUntilDate').hide();
					$('#acceptUntilTime').hide();
				}
			}
		);
		$('#openDate').val(new Date().format("m/dd/yy"));
		$('#dueDate').val(new Date().format("m/dd/yy"));
		$('#acceptUntilDate').val(new Date().format("m/dd/yy"));
	}
};

$(document).ready(function()
{
	InstNewAssnView.init();
});
