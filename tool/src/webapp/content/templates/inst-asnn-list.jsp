<%@ page import="org.sakaiproject.component.cover.ServerConfigurationService" %>
<!DOCTYPE html      PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>Assignment List</title>

<%! // See SakaiRSF class DefaultPortalMatter
String getDefaultPortalMatter() {
  String skin = ServerConfigurationService.getString("skin.default");
  String skinRepo = ServerConfigurationService.getString("skin.repo");
  String headCssToolBase = "<link href=\"" + skinRepo
    + "/tool_base.css\" type=\"text/css\" rel=\"stylesheet\" media=\"all\" />\n";
  String headCssToolSkin = "<link href=\"" + skinRepo + "/" + skin
    + "/tool.css\" type=\"text/css\" rel=\"stylesheet\" media=\"all\" />\n";
  String headCss = headCssToolBase + headCssToolSkin;
  return headCss;
}
%>
<%= getDefaultPortalMatter() %>
<link media="all" href="../css/assignmentsv4.css" type="text/css" rel="stylesheet" />
<link media="all" href="../css/inst-asnn-list.css" type="text/css" rel="stylesheet" />

<!--
    <script type="text/javascript" src="../js/infusion/lib/jquery/core/js/jquery.js"></script>
    <script type="text/javascript" src="../js/infusion/lib/jquery/ui/js/ui.core.js"></script>
    <script type="text/javascript" src="../js/infusion/lib/jquery/ui/js/ui.draggable.js"></script>
    <script type="text/javascript" src="../js/infusion/framework/core/js/jquery.keyboard-a11y.js"></script>
    <script type="text/javascript" src="../js/infusion/lib/jquery/plugins/tooltip/js/jquery.tooltip.js"></script>
    <script type="text/javascript" src="../js/infusion/framework/core/js/Fluid.js"></script>
    <script type="text/javascript" src="../js/infusion/components/inlineEdit/js/InlineEdit.js"></script>
    <script type="text/javascript" src="../js/infusion/framework/core/js/FluidDOMUtilities.js"></script>
    <script type="text/javascript" src="../js/infusion/components/reorderer/js/GeometricManager.js"></script>
    <script type="text/javascript" src="../js/infusion/components/reorderer/js/Reorderer.js"></script>
    <script type="text/javascript" src="../js/infusion/lib/json/js/json2.js"></script>
    <script type="text/javascript" src="../js/infusion/framework/core/js/DataBinding.js"></script>
    <script type="text/javascript" src="../js/infusion/lib/fastXmlPull/js/fastXmlPull.js"></script>
    <script type="text/javascript" src="../js/infusion/framework/renderer/js/fluidParser.js"></script>
    <script type="text/javascript" src="../js/infusion/framework/renderer/js/fluidRenderer.js"></script>
    <script type="text/javascript" src="../js/infusion/components/pager/js/Pager.js"></script>
-->

    <script type="text/javascript" language="JavaScript">
        var sakai = sakai || {};
        sakai.curPlacement = '<%= request.getParameter("placement") %>';
        sakai.curContext = '<%= request.getParameter("context") %>';
        var iframeId = 'Main<%= request.getParameter("placement").replace("-","x") %>';
    </script>

    <!-- Application Code -->
    <script type="text/javascript" language="JavaScript" src="/library/js/headscripts.js"></script>
    <script src="../js/InfusionAll_r7132.js" type="text/javascript"></script>
    <script src="../js/assignment.js" type="text/javascript"></script>
    <script src="../js/inst-asnn-list.js" type="text/javascript"></script>
  </head>
  <body onload="setMainFrameHeight('Main<%= request.getParameter("placement").replace("-","x") %>');">
    <div class="portletBody">

    <ul class="breadCrumb">
      <li class="lastCrumb">Assignment List</li>
    </ul>

<div class="pager-sort-area">
<div id="asnn-list-area">
    <div id="pager-top" class="fl-pager-top flc-pager-top">
        <ul id="page-list" class="pager-links flc-pager-links">
          <li class="fl-pager-pageLink flc-pager-pageLink"><a href="#">1</a></li>
          <li class="flc-pager-pageLink-disabled">2</li>
          <li class="fl-pager-pageLink flc-pager-pageLink"><a href="#">3</a></li>
        </ul>
      <p id="page-size">Show <span>
        <select class="flc-pager-page-size">
          <option value="5">5</option>
          <option value="10">10</option>
          <option value="20">20</option>
          <option value="50">50</option>
        </select></span> per page
      </p>
      <p id="page-arrows">
      <span class="fl-pager-previous flc-pager-previous"><a href="#">&lt; previous</a></span>
      <span class="fl-pager-next flc-pager-next"><a href="#">next &gt;</a></span>
      </p>
    </div>
</div>

<div style="margin: 0; padding: 5px; border-top: solid 2px #cccccc;">
    <span class="sortbylabel">Sorted by:</span><ul class="sort-bar">
      <li class="sort-item"><a href="#" class="titlesort">Title</a> |</li>
      <!-- <li class="sort-item">Restricted To |</li> -->
      <li class="sort-item"><a href="#" class="opendatesort">Open Date</a> |</li>
      <li class="sort-item"><a href="#" class="duedatesort">Due Date</a> |</li>
      <li class="sort-item">
        <a href="#" class="instsort">Instructor Specified Order</a>
        <img src="/library/image/sakai/sortascending.gif" />
      </li>
    </ul>
</div>
</div>

    <table style="display: none; border-collapse: collapse" id="asnn-list">
      <thead>
        <tr>
          <th>
          </th>
          <th class="asnn-decorated-heading">
            <span>Assignments</span>
            <img class="addimage" src="/sakai-assignment2-tool/content/images/add.png" />
            <a class="addlink" href="#" onclick="window.location = '/portal/tool/'+sakai.curPlacement+'/assignment'">
            Add</a>
          </th>
          <th class="asnn-decorated-heading">
            <span>In/New</span>
          </th>
          <th class="asnn-decorated-heading">
            <input id="checkall" type="checkbox" checked="false"/>
          </th>
        </tr>
      </thead>
      <tbody id="asnn-list-body">
        <tr class="row">
          <td style="width: 40px"><img style="display:none" alt="Move Assignment" src="/sakai-assignment2-tool/content/images/4Arrows.png" class="movehandle" /></td>
          <td class="asnn-decorated-cell">
            <span style="display:none" class="asnnid">1</span>
            <span class="asnntitle">Audio Scriptwriting</span>
            <img class="attachments" src="/sakai-assignment2-tool/content/images/attach.png" />
            <span class="draft">draft</span>
            <img class="needsAttention" src="/../../library/image/sakai/warn.gif"
            	alt="There is a problem with this assignment. For additional information, click Edit."
            	title="There is a problem with this assignment. For additional information, click Edit." />
            <div>
            <a href="" class="editlink">Edit</a>
	    <span class="sep1"> | </span>
            <a href="" class="duplink">Duplicate</a>
	    <span class="sep2"> | </span>
            <a href="" class="gradelink">Grade</a>
	    </div>
	    <div>
	    <span class="opendatelabel">Open: </span>
            <span class="opendate">May 6, 2008 3:00 PM</span>
	    </div>
	    <div>
	    <span class="duedatelabel">Due: </span>
            <span class="duedate">May 13, 2008 3:00 PM</span>
	    </div>
	    <div>
	    <span class="groupslabel">Restricted To: </span>
            <span class="groups">Red Cohort, Yellow Cohort</span>
	    </div>
          </td>
          <td class="asnn-decorated-cell">
            <span class="inAndNew">8/4</span>
            <a href="" class="inAndNewLink">10/4</a>
          </td>
          <td class="asnn-decorated-cell"><input class="asnncheck" type="checkbox" /></td>

        </tr>
      </tbody>
    </table>

    <div style="text-align: right; width: 90%; margin: 20px">
      <input style="display: none" type="button" value="Remove" id="removebutton" />
    </div>

  <div class="pager-sort-area">
<div >
    <div id="pager-bottom" class="fl-pager-bottom flc-pager-bottom">
        <ul id="page-list-bottom" class="pager-links flc-pager-links">
          <li class="fl-pager-pageLink flc-pager-pageLink"><a href="#">1</a></li>
          <li class="flc-pager-pageLink-disabled">2</li>
          <li class="fl-pager-pageLink flc-pager-pageLink"><a href="#">3</a></li>
        </ul>
      <p id="page-size-bottom">
	<span class="flc-pager-summary">1-10 of 500 items</span>
      </p>
      <p id="page-arrows-bottom">
      <span class="fl-pager-previous flc-pager-previous"><a href="#">&lt; previous</a></span>
      <span class="fl-pager-next flc-pager-next"><a href="#">next &gt;</a></span>
      </p>
    </div>
</div>

<div style="margin: 0; padding: 5px; border-top: solid 2px #cccccc;">
    <span class="sortbylabel">Sorted by:</span><ul class="sort-bar">
      <li class="sort-item"><a href="#" class="titlesort">Title</a> |</li>
      <!-- <li class="sort-item">Restricted To |</li> -->
      <li class="sort-item"><a href="#" class="opendatesort">Open Date</a> |</li>
      <li class="sort-item"><a href="#" class="duedatesort">Due Date</a> |</li>
      <li class="sort-item">
        <a href="#" class="instsort">Instructor Specified Order</a>
        <img src="/library/image/sakai/sortascending.gif" />
      </li>
    </ul>
</div>
</div> <!-- end pager-sort-area -->

    <!-- This is the template for the Confirm Asnn Remove Dialog. -->
	<div id="remove-asnn-dialog" style="display:none">
	    <div class="generalErrors">
	       <ul class="ui-dialog-msg-list">
	          <li class="alertMessageInline" >Are you sure you want to remove the following assignments and any associated submissions?</li>
	       </ul>
	    </div>
		<table class="listHier lines" > <!--  rsf:id="assignment-list-table" > -->
			<thead><tr>
				<th>Assignment Title</th>
				<th>Due</th>
				<th>Submissions</th>
			</tr></thead>
			<tbody id="asnn-to-delete"> <!--
				<td id="asnn-to-delete-title">Homework 1</td>
				<td id="asnn-to-delete-due">Mar 26, 2008 5:00 pm</td>
				<td id="asnn-to-delete-numsubmissions">1</td> -->
			</tbody>
		</table>
		<span style="display:none" id="asnn-to-delete-id"></span>
		<fieldset class="submit">
			<input id="remove-asnn-button" accesskey="r" type="button" class="remove-button active" value="Remove" />
			<input id="cancel-remove-asnn-button" class="cancel-button" accesskey="x" type="button" value="Cancel" />
		</fieldset>
	</div>

    <script type="text/javascript">
      asnn2.initAsnnList();
    </script>

    </div>
  </body>
</html>
