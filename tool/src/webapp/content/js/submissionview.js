var asnn2subview = asnn2subview || {};

asnn2subview.selectorMap = [
  { selector: ".row", id: "row:" },
  { selector: ".sub-table-header", id: "header:" },
  { selector: ".student-name", id: "student-name"},
  { selector: ".submitted-time", id: "submitted-time"},
  { selector: ".submission-status", id: "submission-status"},
  { selector: ".grade", id: "grade"},
  { selector: ".feedback-released", id: "feedback-released"},
  { selector: ".student-grade-link", id: "student-grade-link"},
  { selector: ".student-name-sort", id: "student-name-sort" },
  { selector: ".submitted-time-sort", id: "submitted-time-sort"},
  { selector: ".submission-status-sort", id: "submission-status-sort"},
  { selector: ".feedback-released-sort", id: "feedback-released-sort" },
  { selector: ".grade-sort", id: "grade-sort" },
  { selector: ".grade-col-header", id: "grade-col-header" },
  { selector: ".grade-td", id: "grade-td" }
];

asnn2subview.getSortHeaderComptree = function() {
  var tree = {
      children: [ 
        { ID: "student-name-sort",
          value: true
        },
        { ID: "submitted-time-sort",
          value: true
        },
        { ID: "submission-status-sort",
          value: true
        },
        { ID: "feedback-released-sort",
          value: true
        }
      ]
  };
  if (asnn2subview.graded === true) {
    tree.children.push({
      ID: "grade-col-header", value: true
    });
    tree.children.push({
      ID: "grade-sort", value: true
    });
  }

};

asnn2subview.subTableRenderer = function (overallThat, inOptions) {
  var that = fluid.initView("asnn2subview.subTableRenderer", overallThat.container, inOptions);

  return {
    returnedOptions: {
      listeners: {
        onModelChange: function (newModel, oldModel) {
          jQuery.ajax({
            type: "GET",
            url: "/direct/assignment2submission.json?asnnid="+asnn2subview.asnnid+"&_start="+(newModel.pageIndex*newModel.pageSize)+"&_limit="+newModel.pageSize,
            cache: false,
            success: function (payload) {
              var data = JSON.parse(payload);
              togo = fluid.transform(data.assignment2submission_collection, asnn2util.dataFromEntity, asnn2subview.filteredRowTransform);
               
              var treedata = { 
                "header:": asnn2subview.getSortHeaderComptree(),
                "row:": togo  
              };
              // Keep this around to test on Fluid Trunk and create a Jira to have more debug information if it's still the same.
              //var treedata = { children: [{ ID: "row:", children: togo }] };
              asnn2subview.renderSubmissions(treedata);
            },
            failure: function() {
              // TODO We need to handle this
            } 
          });
        }
      }
    }
  };

};

asnn2subview.renderSubmissions = function(treedata) {
  if (asnn2subview.subListTemplate) {
    fluid.reRender(asnn2subview.subListTemplate, jQuery("#asnn-submissions-table"), treedata, {cutpoints: asnn2subview.selectorMap});
  }
  else {
    asnn2subview.subListTemplate = fluid.selfRender(jQuery("#asnn-submissions-table"), treedata, {cutpoints: asnn2subview.selectorMap});
  }
  RSF.getDOMModifyFirer().fireEvent();
}

asnn2subview.initPager = function(numSubmissions) {
  var graded = true;

  var columnDefs = [
    {
      key: "student-name-sort",
      valuebinding: "*.studentName",
      sortable: true
    },
    {
      key: "submitted-time-sort",
      valuebinding: "*.submittedDateFormat",
      sortable: true
    },
    {
      key: "submission-status-sort",
      valuebinding: "*.submissionStatus",
      sortable: true
    },
    {
      key: "feedback-released-sort",
      valuebinding: "*.feedbackReleased",
      sortable: true
    }
  ];

  if (graded === true) {
    columnDefs.push({
      key: "grade-sort",
      valuebinding: "*.grade",
      sortable: true
    });
  }

  var pagerBarOptions = {
          type: "fluid.pager.pagerBar",
          options: {
            pageList: {
               type: "fluid.pager.renderedPageList",
               options: {
                 linkBody: "a",
                 pageStrategy: fluid.pager.gappedPageStrategy(3, 1)
                 }
               }
            }
      };

  var fakedata = [];
  for (var i = 0; i < numSubmissions; i++) {
    fakedata.push(i);
  }

  asnn2subview.pager = fluid.pager("#submissions-table-area", {
    dataModel: fakedata,
    columnDefs: columnDefs,
    pagerBar: pagerBarOptions,
    bodyRenderer: {
      type: "asnn2subview.subTableRenderer",
      options: {
//        filteredRowTransform : filteredRowTransform,
        selectors: {
          root: ".fl-pager-data"
        },
        renderOptions: {debugMode: false, cutpoints: asnn2subview.selectorMap}
      }
    }

  });

};

asnn2subview.filteredRowTransform = function(obj, idx) {
    var row = obj;
    var togo = [
      { ID: "student-grade-link",
        target: '/portal/tool/'+sakai.curPlacement+'/grade/'+asnn2.curAsnnId+'/'+row.studentId,
        linktext: row.studentName
      },
      {
        ID: "submission-status",
        value: row.submissionStatus
      }
    ];

    if (row.submittedDateFormat) {
      togo.push({ ID: "submitted-time",
        value: row.submittedDateFormat
      });
    }

    if (row.feedbackReleased === true) {
      togo.push({ ID: "feedback-released", value: true});
    }

    if (asnn2subview.graded === true) {
      togo.push({ ID: "grade", value: row.grade});
    }


    return togo;
  };

asnn2subview.init = function(asnnid, contextId, placementId, numSubmissions, graded) {
  sakai.curPlacement = placementId;
  sakai.curContext = contextId;

  asnn2.curAsnnId = asnnid;

  asnn2subview.asnnid = asnnid;
  asnn2subview.graded = graded;
  asnn2subview.initPager(numSubmissions);
/*
  jQuery.ajax({
    type: "GET",
    url: "/direct/assignment2submission.json?asnnid="+asnnid,
    cache: false,
    success: function (payload) {
      var data = JSON.parse(payload);
      togo = fluid.transform(data.assignment2submission_collection, asnn2util.dataFromEntity, comptreeFromData);
      asnn2subview.initPager(togo, numSubmissions);
    }
    // TODO Handle the failure case
  });
*/
};
