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
  { selector: ".attachments", id: "hasAttachments" }
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
 * A function suitable for use in fluid.transform that will help create an array
 * of just the sortIndex from an array of Assignment objects.
 *
 * @param {Array} Array of Assignment objects
 */
asnn2.sortIndexFromAsnn = function(obj, index) {
  return obj.sortIndex;
}

/**
 * This will reorder the data stored in the page state. What's happening is that
 * we are passing in an Array of integers indicating the new sortIndex order of
 * the current page being displayed. The current page can be calculated using
 * the current pageModel from the state.
 *
 * Once we have that we will create the array of sortIndexes for the current data.
 * We do all of them because at the moment the Asnn2 services require all the 
 * indexes for reordering.
 *
 * After that we will copy the moved indexes to the entire index array for the page.
 * We will then use that master index array in a sort function to reorder the
 * array of Assignments objects. 
 *
 * 1. Get current page slice indices
 * 2. Create sortIndex array of all data
 * 3. Copy the moved indexes over the data at the slice points
 * 4. Re-sort the real data
 * 5. Return the full sortIndex array
 * 
 * @param {Array} Array of numbers with the sortIndex's for the current page.
 * @returns {Array} Array of the entire datasets sortIndex's
 */
asnn2.reorderData = function (moved) {
  var slice = asnn2.findPageSlice(asnn2.pageState.pageModel);
  var allSortIdx = fluid.transform(asnn2.pageState.dataArray, asnn2.sortIndexFromAsnn);
  //TODO Leaving off here 
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
      // Stub for reorder Ajax call
      //alert(neworder);
      jQuery.ajax({
        type: "GET", // Grrr
        url: "/direct/assignment2/reorder.json",
        data: {
          "siteid":sakai.curContext,
          "order":neworder.toString()
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
  asnn2.renderAsnnList(torender);
  asnn2.setupAsnnList(); 
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
