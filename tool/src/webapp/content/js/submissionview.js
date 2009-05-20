var asnn2subview = asnn2subview || {};

asnn2subview.selectorMap = [
  { selector: ".row", id: "row" },
  { selector: ".student-name", id: "student-name"},
  { selector: ".submitted-time", id: "submitted-time"},
  { selector: ".submission-status", id: "submission-status"},
  { selector: ".grade", id: "grade"},
  { selector: ".feedback-released", id: "feedback-released"}
];

asnn2subview.initPager = function(data) {

  var columnDefs = [
    {
      key: "student-name",
      valuebinding: "studentName",
      sortable: false
    }
  ];

  var pager = fluid.pager("#submissions-table-area", {
    dataModel: data,
    columnDefs: columnDefs,

    bodyRenderer: {
      type: "fluid.pager.selfRender",
      options: {
        selectors: {
          root: ".fl-pager-data"
        },
        renderOptions: {debugMode: true, cutpoints: asnn2subview.selectorMap}
      }
    }

  });

};

asnn2subview.init = function() {

  jQuery.ajax({
    type: "GET",
    url: "/direct/assignment2submission.json?asnnid=44",
    cache: false,
    success: function (payload) {
      var data = JSON.parse(payload);
      togo = fluid.transform(data.assignment2submission_collection, asnn2util.dataFromEntity);
      asnn2subview.initPager(togo);
    }
    // TODO Handle the failure case
  });
};