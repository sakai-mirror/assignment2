var Assn2;
if (!Assn2) Assn2 = {};

Assn2.fckeditor = function() {
  // Private functions, if any
  
  // Public 
  return {
    initializeMarkupEditor: function(textarea_id, collection_id) {
      var basepath = "/library/editor/FCKeditor/";
      var browsePrefix = basepath + "editor/filemanager/browser/default/browser.html?Connector=/sakai-fck-connector/web/editor/filemanager/browser/default/connectors/jsp/connector&";
      var uploadPrefix = basepath + "/sakai-fck-connector/web/editor/filemanager/browser/default/connectors/jsp/connector?";
      
      var oFCKeditor = new FCKeditor(textarea_id);
	  oFCKeditor.BasePath = basepath;
	  if (collection_id != "") {
	    oFCKeditor.Config['ImageBrowserURL'] = browsePrefix + "Type=Image&CurrentFolder=" + collection_id;
	    oFCKeditor.Config['LinkBrowserURL'] = browsePrefix + "Type=Link&CurrentFolder=" + collection_id;
	    oFCKeditor.Config['FlashBrowserURL'] = browsePrefix + "Type=Flash&CurrentFolder=" + collection_id;
	    oFCKeditor.Config['ImageUploadURL'] = uploadPrefix + "Type=Image&Command=QuickUpload&Type=Image&CurrentFolder=" + collection_id;
	    oFCKeditor.Config['FlashUploadURL'] = uploadPrefix + "Type=Flash&Command=QuickUpload&Type=Flash&CurrentFolder=" + collection_id;
	    oFCKeditor.Config['LinkUploadURL'] = uploadPrefix + "Type=File&Command=QuickUpload&Type=Link&CurrentFolder=" + collection_id;
      }
      // oFCKeditor.Width  = "600" ;
      // oFCKeditor.Height = "400" ;
	  oFCKeditor.Config['CustomConfigurationsPath'] = "/library/editor/FCKeditor/config.js";
	  oFCKeditor.ReplaceTextarea() ;
	  //oFCKeditor.EditorDocument.body.style.cssText = 'color: red; font-style: italic;';
	  oFCKeditor.annotateAssn2 = true;
    }
  };
}();

function FCKeditor_OnComplete( editorInstance ){
    //editorInstance.LinkedField.form.onsubmit = doSave;
	//alert(editorInstance.name);
	if (editorInstance.Name.match("feedback_text") != null) {
		editorInstance.EditorDocument.body.style.cssText = 'color: red; font-style: italic;';
	}
}