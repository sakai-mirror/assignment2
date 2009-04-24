var asnn2 = asnn2 || {};

asnn2.livedata = true;

/*
 * Returns an list of Assignment Objects that can be viewed.
 */
asnn2.getAsnnCompData = function () {
  var designSampleData = [
    {
      id: "1",
      title: "Audio Scriptwriting",
      editlink: {
        target: "/whatever/edit/1",
        linktext: "Edit"
      },
      duplink: {
        target: "/whatever/duplicate/1",
        linktext: "Duplicate"
      },
      gradelink: {
        target: "/whatever/grade/1",
        linktext: "Grade"
      },
      opentext: "Open: May 6, 2008 3:00 PM",
      duetext: "Due: May 13, 2008 3:00 PM",
      grouptext: "Restricted To: Red Cohort, Yellow Cohort",
      inAndNew: "8/4"
    },
    {
      id: "2",
      title: "Read Chapter 16 of Friedman",
      editlink: {
        target: "/whatever/edit/2",
        linktext: "Edit"
      },
      duplink: {
        target: "/whatever/duplicate/2",
        linktext: "Duplicate"
      },
      opentext: "Open: Apr 29, 2008 3:00 PM",
      duetext: "Due: May 6, 2008 3:00 PM",
      inAndNew: "N/A"
    },
    {
      id: "3",
      title: "Grant Writing",
      editlink: {
        target: "/whatever/edit/3",
        linktext: "Edit"
      },
      duplink: {
        target: "/whatever/duplicate/3",
        linktext: "Duplicate"
      },
      gradelink: {
        target: "/whatever/grade/3",
        linktext: "Grade"
      },
      opentext: "Open: Apr 22, 2008 3:00 PM",
      duetext: "Due: Apr 29, 2008 3:00 PM",
      inAndNew: "8/0"
    },
    {
      id: "4",
      title: "Interactive Storytelling",
      editlink: {
        target: "/whatever/edit/4",
        linktext: "Edit"
      },
      duplink: {
        target: "/whatever/duplicate/4",
        linktext: "Duplicate"
      },
      gradelink: {
        target: "/whatever/grade/4",
        linktext: "Grade"
      },
      opentext: "Open: Apr 15, 2008 3:00 PM",
      duetext: "Due: Apr 22, 2008 3:00 PM",
      inAndNew: "8/2"
    },
    {
      id: "5",
      title: "Professional Writing for Visual Media",
      editlink: {
        target: "/whatever/edit/5",
        linktext: "Edit"
      },
      duplink: {
        target: "/whatever/duplicate/5",
        linktext: "Duplicate"
      },
      gradelink: {
        target: "/whatever/grade/5",
        linktext: "Grade"
      },
      opentext: "Open: Apr 8, 2008 3:00 PM",
      duetext: "Due: Apr 15, 2008 3:00 PM",
      inAndNew: "8/0"
    }
  ];

  var dataFromEntity = function (obj, index) {
    return obj.data; 
  };

  var renderFromData = function (obj, index) {
    var ditto = ['id','title'];
    var togo = {};
    for (var i in ditto) {
      togo[ditto[i]] = obj[ditto[i]];
    } 
    togo.editlink = { 
      target: '/portal/tool/'+sakai.curPlacement+'/assignment/'+obj.id,
      linktext: "Edit" 
    };
    togo.duplink = {
      target: '/portal/tool/'+sakai.curPlacement+'/assignment?duplicatedAssignmentId='+obj.id,
      linktext: "Duplicate"
    }; 
    return togo;
  };

  var togo = []
  if (asnn2.livedata === true) {
    jQuery.ajax({
      url: "/direct/assignment2/sitelist.json", 
      async: false, 
      success: function (payload) {
        var data = JSON.parse(payload);
        togo = fluid.transform(data.assignment2_collection, dataFromEntity, renderFromData);
      }
    });
  }
  else {
    togo = designSampleData;
  }

  return togo;
}

asnn2.setupReordering = function () {
  fluid.reorderList("#asnn-list", {
    selectors : {
      movables: ".row",
      grabHandle: ".movehandle"
    }
  });
}

asnn2.initAsnnList = function () {
  var treedata = {
    "row:": asnn2.getAsnnCompData()
  };

  var selectorMap = [
    { selector: ".row", id: "row:" },
    { selector: ".asnnid", id: "id" },
    { selector: ".asnntitle", id: "title" },
    { selector: ".gradelink", id: "gradelink"},
    { selector: ".editlink", id: "editlink" },
    { selector: ".duplink", id: "duplink" },
    { selector: ".opendate", id: "opentext" },
    { selector: ".duedate", id: "duetext" },
    { selector: ".groups", id: "grouptext" },
    { selector: ".inAndNew", id: "inAndNew" },
  ];

  var asnnlistTemplate = fluid.selfRender(jQuery("#asnn-list"), treedata, {cutpoints: selectorMap});
  /*
  fluid.reorderList("#asnn-list", {
    selectors: {
      movables: "[id^=asnnid]"
    }
  });
  */

  /*
   *  Set up sorting events
   */
  var reverseSort = false;
  $("#titlesort").bind("click", function(e) {
    var newdata;

    if (reverseSort) {
      newdata = {
      "row:": asnn2.getAsnnCompData()
      };
      reverseSort = false;
    }
    else {
      newdata = {
      "row:": asnn2.getAsnnCompData().reverse()
      };
      reverseSort = true;

    }

    fluid.reRender(asnnlistTemplate, jQuery("#asnn-list"), newdata, {cutpoints: selectorMap})
    asnn2.setupReordering();
  });

  /*
   *  Set up inline edits
   */
  var titleEdits = fluid.inlineEdits("#asnn-list", {
    selectors : {
      text: ".asnntitle",
      editables: "p"
    },
    listeners: {
      onFinishEdit: function (newValue, oldValue, editNode, viewNode) {
        //alert("New value is: " + newValue);
      }
    }
  });

  /*
   * Set up reordering
   */
  asnn2.setupReordering();

  /*
   * Set up check all/none control and Remove Button
   */
  $("#checkall").bind("change", function(e) {
    $(".asnncheck").each(function (i) {
      this.checked = e.currentTarget.checked;
    });
  });

  $("#removebutton").bind("click", function(e) {
    var toremove = [];

    $(".asnncheck").each(function (i) {
      if (this.checked) {
        var asnnid = $(".asnnid", this.parentNode.parentNode);
        toremove.push(asnnid.text());
      }
    });
    alert("Removing: " + toremove);
  });


  /*
   * Set up the pagers
   */
  var pager = fluid.pager(".portletBody", {
    listeners: {
      onModelChange: function (newModel, oldModel) {
        var something = "cool";
        //alert(newModel.toString());
      }
    },
    dataModel: [1,2,3,4,5],

    pageBar: {
      type: "fluid.pager.pagerBar",
      dataModel: {length: 5},
      options: {
        pageList: {
          type: "fluid.pager.renderedPageList",
          options: {
            linkBody: "a"
          }
        }
      }
    }
  });

/*
  var newModel = fluid.copy(pager.model);
  newModel.totalRange = treedata.length;
  newModel.pageCount = Math.max(1, Math.floor((newModel.totalRange - 1)/ newModel.pageSize) + 1);
  //newModel.sortKey = that.state.sortKey;
  //newModel.sortDir = that.state.sortDir;
  that.pager.events.onModelChange.fire(newModel, pager.model, pager);
  fluid.model.copyModel(pager.model, newModel);
*/
}
