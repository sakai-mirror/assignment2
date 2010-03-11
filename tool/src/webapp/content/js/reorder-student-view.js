/*
 * JavaScript for rendering the student view.
 * 
 * Uses functions from assignment.js and inst-asnn-list.js
 * 
 */

var asnn2 = asnn2 || {};
asnn2.reorder = asnn2.reorder || {};

asnn2.reorder.buildReorderRenderTreeFromData = function(obj, index) {
  var togo = {};
  
  asnn2.copyDefaultAsnnObjProperties(togo, obj);
  asnn2.addCommonAsnnListReadOnlyRenderObjects(togo, obj);
  
  if (obj.requiresSubmission === true) {
    togo.inAndNew = obj.inAndNew; 
  }
  
  return togo;
};

asnn2.reorder.getReorderCompData = function() {
  return fluid.transform(asnn2.getRawJSONSiteList().assignment2_collection, 
      asnn2util.dataFromEntity, asnn2.reorder.buildReorderRenderTreeFromData);
};

asnn2.reorder.renderList = function() {
  var data = asnn2.reorder.getReorderCompData();
  
  var dopple = $.extend(true, [], data);

  var treedata = {
    "row:": dopple
  };
  
  fluid.selfRender(jQuery("#reorder-list"), treedata ,{cutpoints: asnn2.selectorMap});
};

asnn2.reorder.rebalanceSelects = function() {
  var selects = jQuery("#asnn-list-body select")
  var count = 0;
  selects.each(function(index, ele) {
    jQuery(this).prev().val((index)+"");
    jQuery(this).val((index)+"");
    
    var togo = []
    jQuery("#asnn-list-body .asnnid").each(function() {
        togo.push($(this).text());
    });
    
    jQuery(".asnn-order").val(togo.toString());
  });
};

asnn2.initReorderStudentView = function() {
  asnn2.reorder.renderList();
  
  var selects = jQuery("#asnn-list-body select");
  var len = selects.length;
  
  selects.each(function(idx) {
    var obj = $(this);
    for (var i = 0; i < len; i++) {
      obj.append('<option value="'+ (i) +'">   '+ (i+1) +'   </option>');
    }
    obj.change(function() {
      var curPos = parseInt($(this).prev().val());
      var newPos = parseInt($(this).val());
      var pos = -1;
      if (newPos > curPos)
        pos = 1;
      
      asnn2.reorder.curReorderer.requestMovement(
          {
              element: jQuery(jQuery(".row").get($(this).val())),
              lockedelem: null,
              position: pos
          }, 
          jQuery(obj.parent().parent().get(0))
      );
      //alert("selected: " + obj.val());
      asnn2.reorder.rebalanceSelects();
    });
  });
  asnn2.reorder.rebalanceSelects();
  
  asnn2.reorder.curReorderer = fluid.reorderList("#reorder-list", {
    selectors: {
      movables: ".row",
      grabHandle: ".reorderhandle"
    },
    listeners: {
      afterMove: function(item,requestedPosition,movables) {
        asnn2.reorder.rebalanceSelects();
      }
    }
  });  
  
  
  jQuery("#reorder-list").show();
  
  
};