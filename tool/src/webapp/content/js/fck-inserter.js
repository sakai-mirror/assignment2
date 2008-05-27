jQuery(document).ready(function()
{
	var qs = new Querystring();
	var basePath = "/library/editor/FCKeditor/";
	var conPath = basePath + "editor/filemanager/browser/default/browser.html?Connector=/sakai-fck-connector/filemanager/connector/"
		+ "/group/" + qs.get('context');
	jQuery("textarea.fckeditor").each(function()
	{
		var fck = new FCKeditor(this.id);
		fck.BasePath = basePath;
		fck.Width = "450";
		fck.Height = "310";
		fck.Config['CustomConfigurationsPath'] = "/library/editor/FCKeditor/config.js";
		fck.Config['ImageBrowserURL'] = conPath + "&Type=Image";
		fck.Config['LinkBrowserURL'] = conPath + "&Type=Link";
		fck.Config['FlashBrowserURL'] = conPath + "&Type=Flash";
		fck.Config['ImageUploadURL'] = fck.Config['ImageBrowserURL'] + "&Command=QuickUpload";
		fck.Config['LinkUploadURL'] = fck.Config['LinkBrowserURL'] + "&Command=QuickUpload";
		fck.Config['FlashUploadURL'] = fck.Config['FlashBrowserURL'] + "&Command=QuickUpload";
		fck.ReplaceTextarea();
	});
});
