var asnn2 = asnn2 || {};

asnn2.livedata = true;

/**
 * Returns a list of Assignment Objects that can be viewed.
 */
asnn2.getAsnnCompData = function () {

  var dataFromEntity = function (obj, index) {
    return obj.data;
  };

  var renderFromData = function (obj, index) {
    var ditto = ['id','title', 'sortIndex', 'openDate', 'dueDate',
                 'requiresSubmission'];
    var togo = {};
    for (var i = 0; i < ditto.length; i++) {
      togo[ditto[i]] = obj[ditto[i]];
    }
    if (obj.requiresSubmission === true) {
      togo.inAndNewLink = {
        target: '/portal/tool/'+sakai.curPlacement+'/viewSubmissions/'+obj.id,
        linktext: obj.inAndNew
      };
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
   if (obj.canEdit && obj.canEdit === true) {
      togo.editlink = {
        target: '/portal/tool/'+sakai.curPlacement+'/assignment/'+obj.id,
        linktext: "Edit"
      };
      togo.duplink = {
        target: '/portal/tool/'+sakai.curPlacement+'/assignment?duplicatedAssignmentId='+obj.id,
        linktext: "Duplicate"
      };
    }
    if (obj.graded === true) {
        togo.gradelink = {
            target: '/portal/tool/'+sakai.curPlacement+'/viewSubmissions/'+obj.id,
            linktext: "Grade"
        };
    }
    if (obj.attachments.length > 0) {
        togo.hasAttachments = true;
    }
    if (obj.groups && obj.groups.length > 0) {
        var groupnames = fluid.transform(obj.groups, function(grp,idx) {
          return " "+grp.title;
        });
        togo.grouptext = "Restricted To:" + groupnames.toString();
    }
    if (obj.gbItemMissing || obj.groupMissing) {
    	togo.needsAttention = true;
    }
    return togo;
  };

  var togo = [];
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
};

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
  { selector: ".attachments", id: "hasAttachments" },
  { selector: ".needsAttention", id: "needsAttention"}
];

asnn2.sortMap = [
  { selector: "#titlesort", property: "title" },
  { selector: "#opendatesort", property: "openDate" },
  { selector: "#duedatesort", property: "dueDate" },
  { selector: "#instsort", property: "sortIndex" }
];

/*
 * This tracks the current page state, such as what we're sorting by and in what
 * order, the cached fluid render template, and the current comp data model array.
 */
asnn2.pageState = {
  listTemplate: Object(),
  sortby: "sortIndex",
  sortDir: -1,
  dataArray: [],
  pageModel: {}
};

/*
 * Initializes the top sorting links.
 *
 * For sorting -1 is ascending, and 1 is descending.
 */
asnn2.setupSortLinks = function() {
  for (var i in asnn2.sortMap) {
    var item = asnn2.sortMap[i];
    $(item.selector).bind("click", function(sortby) {
      return function (e) {
        /*
         * If we are sorting by a different term, we want to switch the sort direction back
         * to ascending, otherwise we'll swap it from the current value.
         */
        if (asnn2.pageState.sortby === sortby) {
          asnn2.pageState.sortDir = asnn2.pageState.sortDir * -1;
        }
        else {
          asnn2.pageState.sortby = sortby;
          asnn2.pageState.sortDir = -1;
        }

        var newdata = asnn2.pageState.dataArray;
        newdata.sort(function (arec,brec) {
          var a = arec[sortby];
          var b = brec[sortby];
          return a === b? 0 : ( a > b? -asnn2.pageState.sortDir : asnn2.pageState.sortDir);
        });

        jQuery("img", this.parentNode.parentNode).remove();

        if (asnn2.pageState.sortDir < 0) {
          jQuery(this).after('<img src="/library/image/sakai/sortascending.gif" />');
        }
        else {
          jQuery(this).after('<img src="/library/image/sakai/sortdescending.gif" />');
        }

        asnn2.renderAsnnListPage();
      };
    }(item.property));
  }
};

/*
 * The setup functions below each perform some action on the rendered Assignment list,
 * typically for setting up the events necessary for inline editing, reordering, etc.
 * These need to be called each time the assignment list is repainted.
 */

asnn2.setupRemoveCheckboxes = function () {
  $("#checkall").bind("change", function(e) {
    $(".asnncheck").each(function (i) {
      this.checked = e.currentTarget.checked;
    });
  });
};

/**
 * Reorders the pageState data for the moved pageModel and returns the new array
 * of Assignment ID's in Sort order.
 *
 * @param {Array} Array of numbers with the sortIndex's for the current page.
 * @returns {Array} Array of the entire datasets sortIndex's
 */
asnn2.reorderData = function (moved) {
  var slice = asnn2.findPageSlice(asnn2.pageState.pageModel);
  var allIdIdx = fluid.transform(asnn2.pageState.dataArray, function(obj,index) { return obj.id; });
  var indexesById = {};

  // Copy the reordered page on top of the entire dataset (all pages)
  for (var i = 0, j = slice[0]; i < moved.length; i++, j++) {
    allIdIdx[j] = new Number(moved[i]);
  }

  // Make a new hash using AsnnId's as keys and storing the sortIndex
  for (var k = 0; k < allIdIdx.length; k++) {
    indexesById[allIdIdx[k]] = new Number(k);
  }

  // Update the Sort Indexs on the Assignments stored in Page State
  for (var m = 0; m < asnn2.pageState.dataArray.length; m++) {
    var curdata = asnn2.pageState.dataArray[m];
    curdata.sortIndex = indexesById[curdata.id];
  }

  // Resort the data packing the page, so it will be correct if we page back and
  // forth before reloading the data from the server.
  asnn2.pageState.dataArray.sort(function (a, b) {
    return a.sortIndex - b.sortIndex;
  });

  // Return the new order of items. Admittedly this whole method sucks, I'm still
  // exploring the built-in functionality/methods of JS data structures to find
  // a better way to do this part.
  return allIdIdx;
}

/**
 * This sets up the drag'n'drop hopefully accessible reordering each time the list
 * is paged or refreshed.
 *
 * Because the page can be sorted many different ways, we only want the reordering to
 * be available when it is sorted by Instructor Specified Order in Ascending Order.
 */
asnn2.setupReordering = function () {
  var asnnsels = {};
  var afterMoveFunc = function(){};
  var allowReorder = true;
  if (asnn2.pageState.sortDir !== -1 || asnn2.pageState.sortby !== 'sortIndex') {
    allowReorder = false;
    asnnsels = {
      movables: ".row",
      grabHandle: ".dummy"
    };
  }
  else {
    asnnsels = {
      movables: ".row",
      grabHandle: ".movehandle"
    };
    afterMoveFunc = function(item,requestedPosition,movables) {
      var neworder = [];
      movables.each(function(i, obj) {
        neworder.push(jQuery('.asnnid',obj).text());
      });
      var integralIdx = asnn2.reorderData(neworder);
      // Stub for reorder Ajax call
      //alert(neworder);
      jQuery.ajax({
        type: "GET", // Grrr
        url: "/direct/assignment2/reorder.json",
        data: {
          "siteid":sakai.curContext,
          "order": integralIdx.toString()
        }
      });
    };
  }

  fluid.reorderList("#asnn-list", {
    selectors : asnnsels,
    listeners: {
      afterMove: afterMoveFunc,
      onHover: function(item,state) {
        jQuery('td', item).each(function(i, obj) {
          if (i === 0) {
            if (state && allowReorder === true) {
              jQuery('img',this).show();
            }
            else {
              jQuery('img',this).hide();
            }
          }
          else {
            if (state) {
              jQuery(this).addClass('asnn-hover');
            }
            else {
              jQuery(this).removeClass('asnn-hover');
            }
          }
        });
      }
    }
  });
};

asnn2.getAsnnObj = function(val, prop) {
  var p = prop || "id";
  for (var i = 0; i < asnn2.pageState.dataArray.length; i++) {
    var next = asnn2.pageState.dataArray[i];
    if (next[p] == val) { // Yes, double equal. Looking at usage in onFinishEdit still for the inline edit
      return asnn2.pageState.dataArray[i];
    }
  }
  return undefined;
}

/*
 *  Set up inline edits
 */
asnn2.setupInlineEdits = function () {
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
        asnn2.getAsnnObj(new Number(asnnid))['title'] = newValue;
      }
    }
  });
};

/**
 * Refresh all the actions and listeners on the asnn list table that need to be
 * setup each time it is rendered.
 */
asnn2.setupAsnnList = function () {
  asnn2.setupRemoveCheckboxes();
  asnn2.setupReordering();
  asnn2.setupInlineEdits();
};

/** End Asnn List Setup Functions **/

/**
 * Performs the actual rendering of the list area using the Fluid Renderer.
 *
 * @param {Array|null} The list of assignments to render, in renderer model form.
 * If not passed in, will use the stored state data.
 */
asnn2.renderAsnnList = function(asnndata) {
  var data = asnndata || asnn2.pageState.dataArray;

  var dopple = $.extend(true, [], data);

  var treedata = {
    "row:": dopple
  };

  if (asnn2.asnnListTemplate) {
    fluid.reRender(asnn2.asnnListTemplate, jQuery("#asnn-list"), treedata, {cutpoints: asnn2.selectorMap});
  }
  else {
    asnn2.asnnListTemplate = fluid.selfRender(jQuery("#asnn-list"), treedata, {cutpoints: asnn2.selectorMap});
  }
};

/**
 * Used to render the Asnn List using a model from the Fluid Pager. This is designed to be
 * call from the pager listener and use the pages state to rerender the Asnn List.
 * @param {pageModel} A Fluid Page Model
 */
asnn2.renderAsnnListPage = function(newPageModel) {
  var pageModel = newPageModel || asnn2.pageState.pageModel;
  var bounds = asnn2.findPageSlice(pageModel);
  // TODO: Does Javascript array.slice just copy the references or really make new objects?
  var torender = [];
  for (var i = bounds[0]; i < bounds[1]+1; i++) {
    torender.push(asnn2.pageState.dataArray[i]);
  }
  jQuery("#asnn-list").hide();
  asnn2.renderAsnnList(torender);
  asnn2.setupAsnnList();
  jQuery("#asnn-list").show();
};

/**
 * Determine the slice to render based off a pageModel.
 * @param {pageModel} Page model from the Fluid Pager. This is the object model you get whenever it
 * changes.
 * @return {Array} An array consisting of the start and end to use. ex. [10,14]
 */
asnn2.findPageSlice = function(pageModel) {
  var start = pageModel.pageIndex * pageModel.pageSize;
  var end = start + Number(pageModel.pageSize) - 1; // This was getting coerced to String addition
  if (end > (pageModel.totalRange-1)) {
    end = pageModel.totalRange-1;
  }
  return [start,end];
};

/**
 * The master init function to be called at the bottom of the HTML page.
 */
asnn2.initAsnnList = function () {
  asnn2.pageState.dataArray = asnn2.getAsnnCompData();

  // I would like to remove this, but am getting a duplicate attribute error currently
  // when I first render it in the pager listener.
  asnn2.renderAsnnList();

  asnn2.setupSortLinks();

  /*
   * Bind the remove button at the bottom of the screen.
   * TODO: Put the confirmation dialog back in.
   */
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
        //TODO Properly refire the pager with an updated model
        window.location.reload();
      }
    });
  });


  /*
   * Set up the pagers
   */
  // I'm getting a too much recursion error when using my component tree, using a simple array for now.
  var fakedata = [];
  for (var i = 0; i < asnn2.pageState.dataArray.length; i++) {
    fakedata.push(i);
  }

  var pager = fluid.pager("#asnn-list-area", {
    listeners: {
      onModelChange: function (newModel, oldModel) {
        // We need to store the pageModel so that the Sorting links can use it when they need
        // to refresh the list
        asnn2.pageState.pageModel = newModel;
        asnn2.renderAsnnListPage();
      }
    },
    dataModel: fakedata,
    pagerBar: {type: "fluid.pager.pagerBar", options: {
      pageList: {type: "fluid.pager.renderedPageList",
        options: {
          linkBody: "a"
        }
      }
    }}
  });

};
