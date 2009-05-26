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
  { selector: ".grade-td", id: "grade-td" }
];

asnn2subview.initPager = function(data) {
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
                 linkBody: "a"
                 }
               }
            }
      };

  var filteredRowTransform = function(obj, idx) {
    //if (obj.feedbackReleased === true) {
    //  obj.showFeedbackIcon = true;
    //}
    //obj.studentGradeLink = {
    var row = obj.row;
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

    if (graded === true) {
      togo.push({ ID: "grade", value: row.grade});
    }


    return togo;
  };

  var pager = fluid.pager("#submissions-table-area", {
    dataModel: data,
    columnDefs: columnDefs,
    pagerBar: pagerBarOptions,
    bodyRenderer: {
      type: "fluid.pager.selfRender",
      options: {
        filteredRowTransform : filteredRowTransform,
        selectors: {
          root: ".fl-pager-data"
        },
        renderOptions: {debugMode: false, cutpoints: asnn2subview.selectorMap}
      }
    }

  });

};

asnn2subview.init = function(asnnid, contextId, placementId) {
  sakai.curPlacement = placementId;
  sakai.curContext = contextId;

  asnn2.curAsnnId = asnnid;

  var comptreeFromData = function(obj, idx) {
    if (obj.feedbackReleased === true) {
      obj.showFeedbackIcon = true;
    }
    obj.studentGradeLink = {
      ID: "student-grade-link",
      target: '/portal/tool/'+sakai.curPlacement+'/grade/'+asnnid+'/'+obj.studentId,
      linktext: obj.studentName
    }
    return obj;
  }

  jQuery.ajax({
    type: "GET",
    url: "/direct/assignment2submission.json?asnnid="+asnnid,
    cache: false,
    success: function (payload) {
      var data = JSON.parse(payload);
      togo = fluid.transform(data.assignment2submission_collection, asnn2util.dataFromEntity, comptreeFromData);
      asnn2subview.initPager(togo);
    }
    // TODO Handle the failure case
  });
};
