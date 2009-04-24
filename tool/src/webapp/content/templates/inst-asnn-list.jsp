<!DOCTYPE html      PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>Assignment List</title>
    <link media="all" href="../css/tool_base.css" type="text/css" rel="stylesheet" />
    <link media="all" href="../css/assignmentsv4.css" type="text/css" rel="stylesheet" />
    <link media="all" href="../css/inst-asnn-list.css" type="text/css" rel="stylesheet" />

<!--
    <script type="text/javascript" src="../../../lib/jquery/core/js/jquery.js"></script>
    <script type="text/javascript" src="../../../lib/jquery/ui/js/ui.core.js"></script>
    <script type="text/javascript" src="../../../lib/jquery/ui/js/ui.draggable.js"></script>
    <script type="text/javascript" src="../../../framework/core/js/jquery.keyboard-a11y.js"></script>
    <script type="text/javascript" src="../../../lib/jquery/plugins/tooltip/js/jquery.tooltip.js"></script>
    <script type="text/javascript" src="../../../framework/core/js/Fluid.js"></script>
    <script type="text/javascript" src="../../../components/inlineEdit/js/InlineEdit.js"></script>
    <script type="text/javascript" src="../../../framework/core/js/FluidDOMUtilities.js"></script>
    <script type="text/javascript" src="../../../components/reorderer/js/GeometricManager.js"></script>
    <script type="text/javascript" src="../../../components/reorderer/js/Reorderer.js"></script>
-->

    <!-- More things for the renderer -->

<!--
    <script type="text/javascript" src="../../../lib/json/js/json2.js"></script>
    <script type="text/javascript" src="../../../framework/core/js/DataBinding.js"></script>
    <script type="text/javascript" src="../../../lib/fastXmlPull/js/fastXmlPull.js"></script>
    <script type="text/javascript" src="../../../framework/renderer/js/fluidParser.js"></script>
    <script type="text/javascript" src="../../../framework/renderer/js/fluidRenderer.js"></script>
-->

    <!-- Pager -->
    <!-- <script type="text/javascript" src="../../../components/pager/js/Pager.js"></script> -->

    <script type="text/javascript" language="JavaScript">
        var sakai = sakai || {};
        sakai.curPlacement = '<%= request.getParameter("placement") %>';
        sakai.curContext = '<%= request.getParameter("context") %>';
    </script>

    <!-- Application Code -->
    <script src="../js/InfusionAll_r7128.js" type="text/javascript"></script>
    <script src="../js/inst-asnn-list.js" type="text/javascript"></script>
	<script type="text/javascript" language="JavaScript" src="/library/js/headscripts.js"></script>
  </head>
  <body onload="setMainFrameHeight('Main<%= request.getParameter("placement").replace("-","x") %>');">
    <div class="portletBody">

    <ul class="breadCrumb">
      <li class="lastCrumb">Assignment List</li>
    </ul>

    <ul id="pager-top" class="fl-pager-top flc-pager-top">
      <li value="1" class="fl-pager-pageLink flc-pager-pageLink"><a href="#">1</a></li>
      <li value="2" class="fl-pager-pageLink flc-pager-pageLink"><a href="#">2</a></li>
      <li class="fl-pager-previous flc-pager-previous"><a href="#">&lt; previous</a></li>
      <li class="fl-pager-next flc-pager-next"><a href="#">next &gt;</a></li>
      <li>
        Show <span>
          <select class="flc-pager-page-size">
            <option value="5">5</option>
            <option value="10">10</option>
            <option value="20">20</option>
            <option value="50">50</option>
          </select></span> per page
      </li>
    </ul>

    <span>Sorted by:</span>
    <ul class="sort-bar">
      <li class="sort-item"><a href="#" id="titlesort">Title</a> |</li>
      <li class="sort-item">Restricted To |</li>
      <li class="sort-item">Open Date |</li>
      <li class="sort-item">Due Date |</li>
      <li class="sort-item">Instructor Specified Order</li>
    </ul>

    <table id="asnn-list">
      <thead>
        <tr>
          <th>
          </th>
          <th>
            <span>Assignments</span>
            <a href="#" onclick="window.location = '/portal/tool/'+sakai.curPlacement+'/assignment'">Add</a>
          </th>
          <th>
            <span>In/New</span>
          </th>
          <th>
            <input id="checkall" type="checkbox" />
          </th>
        </tr>
      </thead>
      <tbody id="asnn-list-body">
        <tr class="row">
          <td><span class="movehandle">Move</span></td>
          <td><p>
            <span style="display:none" class="asnnid">1</span>
            <span class="asnntitle">Audio Scriptwriting</span><br/>
            <a href="" class="editlink">Edit</a>
            <a href="" class="duplink">Duplicate</a>
            <a href="" class="gradelink">Grade</a>
            <br/>
            <span class="opendate">Open: May 6, 2008 3:00 PM</span><br/>
            <span class="duedate">Due: May 13, 2008 3:00 PM</span><br/>
            <span class="groups">Restricted To: Red Cohort, Yellow Cohort</span>
            </p>
          </td>
          <td class="inAndNew" >8/4</td>
          <td><input class="asnncheck" type="checkbox" /></td>

        </tr>
      </tbody>
    </table>

    <input type="button" value="Remove" id="removebutton" />

    <script type="text/javascript">
      asnn2.initAsnnList();
    </script>

    </div>
  </body>
</html>
