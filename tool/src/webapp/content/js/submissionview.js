var asnn2subview = asnn2subview || {};

asnn2subview.init = function() {
  alert("Hello Submission View!");

  jQuery.ajax({
    type: "GET",
    url: "/direct/assignment2submission.json?asnnid=44",
    cache: false,
    success: function (payload) {
      var data = JSON.parse(payload);
      alert(JSON.stringify(data));
    }
  });
};