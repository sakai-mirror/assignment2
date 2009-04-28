var asnn2 = asnn2 || {};

asnn2.livedata = true;

/*
 * Returns an list of Assignment Objects that can be viewed.
 */
asnn2.getAsnnCompData = function () {
  
  var dataFromEntity = function (obj, index) {
    return obj.data; 
  };

  var renderFromData = function (obj, index) {
    var ditto = ['id','title', 'sortIndex', 'openDate', 'dueDate',
                 'requiresSubmission'];
    var togo = {};
    for (var i in ditto) {
      togo[ditto[i]] = obj[ditto[i]];
    } 
    if (obj.requiresSubmission === true) {
      togo.inAndNewLink = {
        target: '/portal/tool/'+sakai.curPlacement+'/viewSubmissions/'+obj.id,
        linktext: obj.inAndNew
      } 
    }
    else {
      togo.inAndNew = obj.inAndNew;
    }
    if (obj.openDate) {
      togo.opentext = "Open: " + new Date(obj.openDate).toLocaleString();
    }
    if (obj.dueDate) {
      togo.duetext = "Due: " + new Date(obj.dueDate).toLocaleString();
    }
    togo.editlink = { 
      target: '/portal/tool/'+sakai.curPlacement+'/assignment/'+obj.id,
      linktext: "Edit" 
    };
    togo.duplink = {
      target: '/portal/tool/'+sakai.curPlacement+'/assignment?duplicatedAssignmentId='+obj.id,
      linktext: "Duplicate"
    }; 
    if (obj.graded === true) {
        togo.gradelink = {
            target: '/portal/tool/'+sakai.curPlacement+'/viewSubmissions/'+obj.id,
            linktext: "Grade"
        }
    }
    return togo;
  };

  var togo = []
  if (asnn2.livedata === true) {
    jQuery.ajax({
      type: "GET",	
      url: "/direct/assignment2/sitelist.json",
      data: { 
        'siteid': sakai.curContext,
        'no-cache': true
      },
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

asnn2.selectorMap = [
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
  { selector: ".inAndNewLink", id: "inAndNewLink" },
];

asnn2.setupReordering = function () {
  fluid.reorderList("#asnn-list", {
    selectors : {
      movables: ".row",
      grabHandle: ".movehandle"
    }
  });
};

asnn2.renderAsnnList = function(treedata) {
  if (asnn2.asnnListTemplate) {
    fluid.reRender(asnn2.asnnListTemplate, jQuery("#asnn-list"), treedata, {cutpoints: asnn2.selectorMap})
  }
  else {
    asnn2.asnnListTemplate = fluid.selfRender(jQuery("#asnn-list"), treedata, {cutpoints: asnn2.selectorMap});
  }
};

asnn2.setupRemoveCheckboxes = function () {
  $("#checkall").bind("change", function(e) {
    $(".asnncheck").each(function (i) {
      this.checked = e.currentTarget.checked;
    });
  });
};

/**
 * Refresh all the actions and listeners on the asnn list table that need to be
 * setup each time it is rendered.
 */
asnn2.refreshAsnnListEvents = function () {
  asnn2.setupRemoveCheckboxes(); 
  asnn2.setupReordering();

};

asnn2.initAsnnList = function () {
  var treedata = {
    "row:": asnn2.getAsnnCompData()
  };

  asnn2.renderAsnnList(treedata);

  /*
   *  Set up sorting events
   */
  var sortMap = [
      { selector: "#titlesort", property: "title" },
      { selector: "#opendatesort", property: "openDate" },
      { selector: "#duedatesort", property: "dueDate" },
      { selector: "#instsort", property: "sortIndex" }
  ];
  
  var sortDir = 1;
  for (var i in sortMap) {
    var item = sortMap[i];
    $(item.selector).bind("click", function(sortby) {
      return function (e) {
        var newdata = asnn2.getAsnnCompData();
        newdata.sort(function (arec,brec) {
          var a = arec[sortby];
          var b = brec[sortby];
          return a === b? 0 : ( a > b? sortDir : -sortDir); 
        });

        sortDir = sortDir * -1;

        asnn2.renderAsnnList({ "row:": newdata });
        asnn2.refreshAsnnListEvents();
      };
    }(item.property));
  }

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
        var asnnid = $(".asnnid", viewNode.parentNode).text();
        jQuery.ajax({
          type: "POST",
          url: "/direct/assignment2/"+asnnid+"/edit",
          data: {
            id: asnnid,
            title: newValue
          }
        });
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
  asnn2.setupRemoveCheckboxes();

  $("#removebutton").bind("click", function(e) {
    var toremove = [];

    $(".asnncheck").each(function (i) {
      if (this.checked) {
        var asnnid = $(".asnnid", this.parentNode.parentNode).text();
        toremove.push(asnnid);
        // TODO: Bulk these delete commands together
        jQuery.ajax({
          type: "DELETE",
          url: "/direct/assignment2/"+asnnid+"/delete"
        });
        
      }
    });
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
