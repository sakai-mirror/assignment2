function a2SetMainFrameHeight(){
	if (iframeId != ""){
		if(arguments[0] != null){
			height = arguments[0];
		} else {
			height = jQuery(document).height();// + 10;
		}
		jQuery("#" + iframeId, parent.document).height(height);
	}
}
newValue = "";
function useValue(value){
   newValue = value;
}
function changeValue(){   
    el = jQuery("select[name='page-replace\:\:gradebook_item-selection']").get(0);
    if(el){
        for(i=0;i<el.length;i++){
            if(el.options[i].text == newValue){
                el.selectedIndex = i;  
            }
        }
    }

    selectGraded();
    populateTitleWithGbItemName();
}

function populateTitleWithGbItemName() {
    var curr_title = jQuery("input[name='page-replace\:\:title']").get(0).value;
    if (!curr_title || curr_title == "") {
        // get the currently selected gb item
        var gbSelect = jQuery("select[name='page-replace\:\:gradebook_item-selection']").get(0);
        var gbSelIndex = gbSelect.selectedIndex;
        if (gbSelIndex != 0) { 
            var selectedItem = gbSelect.options[gbSelIndex].text;
            // replace the empty title field with the new_title
            jQuery("input[name='page-replace\:\:title']").val(selectedItem);
        }
    }
}

function selectGraded() {
    el = jQuery("select[name='page-replace\:\:gradebook_item-selection']").get(0);
    if (el.selectedIndex != 0){
        jQuery("input[type='radio'][id='page-replace\:\:select_graded']").get(0).checked=true;
    } else {
        jQuery("input[type='radio'][id='page-replace\:\:select_ungraded']").get(0).checked=true;
    }
}


groups_toggle = function(){
	el = jQuery("input[type='radio'][value='false'][name='page-replace\:\:access_select-selection']").get(0);
	if (el && el.checked) {
		jQuery('li#groups_table_li').hide();
	} else {
		jQuery('li#groups_table_li').show();
	}
}

function toggle_group_checkboxes(check_all_box){
	if (check_all_box.checked){
		jQuery('table#groupTable :checkbox').attr('checked', 'checked');
	} else {
		jQuery('table#groupTable :checkbox').removeAttr('checked');
	}
}

function update_resubmit_until(){
	el = jQuery("input:checkbox[@name='page-replace\:\:resubmit_until']").get(0);
	if (el){
	if (el.checked) {
		jQuery(".resubmit_until_toggle").show();
	} else {
		jQuery(".resubmit_until_toggle").hide();
	}
	}
}

function override_submission_settings(){
    override_checkbox = jQuery("input:checkbox[@name='page-replace\:\:override_settings']").get(0);
    
    if (override_checkbox){
        if (override_checkbox.checked) {
            // change text back to normal
            jQuery("#override_settings_container").removeClass("inactive");
            
            // enable all of the form elements
            jQuery("select[@name='page-replace\:\:resubmission_additional-selection']").removeAttr("disabled");
            jQuery("input:checkbox[@name='page-replace\:\:require_accept_until']").removeAttr("disabled");
            // TODO - get the rsf date widget to work when these are disabled -it
            // is throwing syntax error upon submission
            //jQuery("input[@name='page-replace\:\:accept_until\:1\:date-field']").removeAttr("disabled");
           //jQuery("input[@name='page-replace\:\:accept_until\:1\:time-field']").removeAttr("disabled");
        } else {
            // gray out the text
            jQuery("#override_settings_container").addClass("inactive");
            
            // disable all form elements
            jQuery("select[@name='page-replace\:\:resubmission_additional-selection']").attr("disabled","disabled");
            jQuery("input:checkbox[@name='page-replace\:\:require_accept_until']").attr("disabled","disabled");
           // jQuery("input[@name='page-replace\:\:accept_until\:1\:date-field']").attr("disabled","disabled");
            //jQuery("input[@name='page-replace\:\:accept_until\:1\:time-field']").attr("disabled","disabled");
        }
    }
}

function set_accept_until_on_submission_level(){
    el = jQuery("input:checkbox[@name='page-replace\:\:require_accept_until']").get(0);
    if (el){
        if (el.checked) {
            jQuery("#accept_until_container").show();
        } else {
            jQuery("#accept_until_container").hide();
        }
    }
}

function update_new_gb_item_helper_url() {
    var gbUrlWithoutName = jQuery("a[id='page-replace\:\:gradebook_url_without_name']").attr("href");
    var new_title = jQuery("input[name='page-replace\:\:title']").get(0).value
    
    // encode unsafe characters that may be in the assignment title
   
    var escaped_title = "";
    if (new_title) {
        escaped_title = escape(new_title);
    }
    
    var modifiedUrl = gbUrlWithoutName + "&name=" + escaped_title;
    
    jQuery("a[id='page-replace\:\:gradebook_item_new_helper']").attr("href", modifiedUrl);
}

jQuery(document).ready(function(){
	update_resubmit_until();
	asnn2.initializeSorting();
});

function slide_submission(img){
	jQuery(img).parent('h4').next('div').toggle();
	flip_image(img)
}
function slideFieldset(img) {
	jQuery(img).parent('legend').next('ol').toggle();
	flip_image(img);
	a2SetMainFrameHeight();
}
function slideDiv(img) {
    jQuery(img).parent('legend').next('div').toggle();
    flip_image(img);
    a2SetMainFrameHeight();
}
function flip_image(img){
	if(img.src.match('collapse')){
		img.src=img.src.replace(/collapse/, 'expand');
	}else{
		img.src=img.src.replace(/expand/, 'collapse');
	}
}


function updateAttachments(imgsrc, filename, link, ref, filesize){
	//Check if current last row is "demo"
   if (jQuery("#attachmentsFieldset ol:first li:last").hasClass("skip")){
   	newRow = jQuery("#attachmentsFieldset ol:first li:last").get(0);
   	jQuery(newRow).removeClass("skip");
   } else {
    newRow = jQuery("#attachmentsFieldset ol:first li:last").clone(true).appendTo("#attachmentsFieldset ol:first").get(0);
   }
   jQuery("img", newRow).attr("src",imgsrc);
   jQuery("a:first", newRow).attr("href", link);
   jQuery("a:first", newRow).html(filename);
   jQuery("input", newRow).attr("value", ref);
   jQuery("span:first", newRow).html(filesize);
   
   updateDisplayNoAttachments();
}

function removeAttachment(attach) {
    // we need to leave at least one "skipped" demo for use for new attachments
    var li = jQuery("#attachmentsFieldset li");
    if (li.size() <=1) {
        // this is the only one in the list, so we need to just
        // change it to "skipped"
        jQuery(attach).parent('span').parent('li').addClass("skip");
    } else {
        jQuery(attach).parent('span').parent('li').remove();
    }
}

function updateDisplayNoAttachments(){
    var li = jQuery("#attachmentsFieldset li").get(0);
    if (li) {
        var skipped = jQuery("#attachmentsFieldset ol:first li:last").hasClass("skip");
        if (skipped) {
            jQuery("span.no_attachments_yet").show();
        } else {
            jQuery("span.no_attachments_yet").hide();
        }
    } else {
        jQuery("span.no_attachments_yet").show();
    }
}

var asnn2 = asnn2 || {};

(function (jQuery, asnn2) {
    
  //Sorting functions
    var sortBy; 
    var sortDir; 
    var pStart=0; 
    var pLength=5;
    
    function sortPageRows(b,d) {
       pLength = jQuery("select[name='page-replace\:\:pagerDiv:1:pager_select_box-selection']").val();
       sortBy=b; sortDir=d;
       var trs = jQuery.makeArray(jQuery("table#sortable tr:gt(0)"));
       trs.sort(function(a,b){
          return (jQuery("." + sortBy, a).get(0).innerHTML.toLowerCase() < jQuery("." + sortBy, b).get(0).innerHTML.toLowerCase()
          ? -1 : (jQuery("." + sortBy, a).get(0).innerHTML.toLowerCase()> jQuery("." + sortBy, b).get(0).innerHTML.toLowerCase()
             ? 1 : 0));
       });
       if (sortDir == 'desc') {trs.reverse();}
       jQuery(trs).appendTo(jQuery("table#sortable tbody"));
       jQuery("a img", "table#sortable tr:first").remove();
       imgSrc = "<img src=\"/sakai-assignment2-tool/content/images/bullet_arrow_" + (d == 'asc' ? 'up' : 'down') + ".png\" />";
       jQuery("a." + b, "table#sortable tr:first").append(imgSrc);
       //Now paging
       jQuery("table#sortable tr:gt(0)").hide();
       jQuery("table#sortable tr:gt(" + pStart + "):lt(" + pLength +")").show();
       trsLength = jQuery("table#sortable tr:gt(0)").size();
       if (pStart == 0){
         jQuery("div.pagerDiv input[name='page-replace\:\:pagerDiv\:1\:pager_first_page'], div.pagerDiv input[name='page-replace\:\:pagerDiv\:1\:pager_prev_page']").attr('disabled', 'disabled');
       } else {
          jQuery("div.pagerDiv input[name='page-replace\:\:pagerDiv\:1\:pager_first_page'], div.pagerDiv input[name='page-replace\:\:pagerDiv\:1\:pager_prev_page']").removeAttr('disabled');
       }
       if (Number(pStart) + Number(pLength) >= trsLength) {
          jQuery("div.pagerDiv input[name='page-replace\:\:pagerDiv\:1\:pager_next_page'], div.pagerDiv input[name='page-replace\:\:pagerDiv\:1\:pager_last_page']").attr('disabled', 'disabled');
       } else {
          jQuery("div.pagerDiv input[name='page-replace\:\:pagerDiv\:1\:pager_next_page'], div.pagerDiv input[name='page-replace\:\:pagerDiv\:1\:pager_last_page']").removeAttr('disabled');  
       }
       
       //now parse the date
       format = jQuery("div.pagerDiv div.pager_viewing_format").get(0).innerHTML;
       format = format.replace(/\{0\}/, Number(pStart) + 1);
       last = Number(pStart) + Number(pLength) > trsLength ? trsLength : Number(pStart) + Number(pLength);
       format = format.replace(/\{1\}/, last);
       format = format.replace(/\{2\}/, jQuery("table#sortable tr:gt(0)").size());
       jQuery("div.pagerDiv div.pagerInstruction").html(format);
       
       // align the grading box
       asnn2.alignGrading();

    };
    
    asnn2.initializeSorting = function() {
        if (jQuery("table#sortable").get(0)) {
            sortPageRows(defaultSortBy,'asc');
            pStart=0;
        } 
    };
    
    asnn2.changeSort = function(newBy) {
        sortPageRows(newBy, (newBy!=sortBy ? 'asc' : (sortDir == 'asc' ? 'desc' : 'asc')));
    };
    
    asnn2.changePage = function(newPStart){
        total = jQuery("table#sortable tr:gt(0)").size();
        if ("first" == newPStart) {
            pStart = 0;
        } else if ("prev" == newPStart) {
            pStart = pStart - pLength;
            if (pStart < 0) pStart =0;
        } else if ("next" == newPStart) {
            pStart = Number(pStart) + Number(pLength);
            if (pStart > total) {
                pStart = total - (total % pLength);
            }
        } else if ("last" == newPStart) {
            if (total > pLength) {
                pStart = total - (total % pLength);
            } else {
                pStart = 0;
            }
        }
       sortPageRows(sortBy, sortDir);   
    };
    
    
})(jQuery, asnn2);


/* New Asnn2 functions that are namespaced. Will need to go back
 * and namespace others eventually.
 */

(function (jQuery, asnn2) {
    var EXPAND_IMAGE = "/sakai-assignment2-tool/content/images/expand.png";
    var COLLAPSE_IMAGE = "/sakai-assignment2-tool/content/images/collapse.png";
    var NEW_FEEDBACK_IMAGE = "/library/image/silk/email.png";
    var READ_FEEDBACK_IMAGE = "/library/image/silk/email_open.png";
    
    asnn2.toggle_hideshow_by_id = function (arrowImgId, toggledId) {
        toggle_hideshow(jQuery('#'+arrowImgId.replace(/:/g, "\\:")),
                        jQuery('#'+toggledId.replace(/:/g, "\\:")));
    }
    
    function toggle_hideshow(arrowImg, toggled) {
        if (arrowImg.attr('src') == EXPAND_IMAGE) {
            arrowImg.attr('src', COLLAPSE_IMAGE);
            toggled.show();
        }
        else {
            arrowImg.attr('src', EXPAND_IMAGE);
            toggled.hide();
        }
    };
    
    function mark_feedback(submissionId, versionId) {
        var queries = new Array();
        queries.push(RSF.renderBinding("MarkFeedbackAsReadAction.asnnSubId",submissionId));
        queries.push(RSF.renderBinding("MarkFeedbackAsReadAction.asnnSubVersionId",versionId));
        queries.push(RSF.renderActionBinding("MarkFeedbackAsReadAction.execute"))
        var body = queries.join("&");
        jQuery.post(document.URL, body);
    };
    
    /**
     * Custom javascript for the Assignment Add/Edit page. When the "Require
     * Submissions" box is checked or unchecked, the Submission Details below it
     * need to show/hide. It does this by hiding/removing all siblings at the
     * same level (ie. the rest of the <li/> in the current list). it also
     * needs to show/hide the notifications block, as well
     * 
     * @param element The jQuery element whose downstream siblings will be 
     * shown or hidden
     * @param show Whether or not to show them.
     */
    asnn2.showHideSiblings = function(element, show) {
    	if (show == true) {
    		jQuery(element).nextAll().show();
    		jQuery("#notifications").show();
    	}
    	else {
    		jQuery(element).nextAll().hide();
    		jQuery("#notifications").hide();
    	}
    };
    
    /**
     * Given a checkbox element, hide or show the area element whenever checkbox
     * is clicked.  Checking the box shows the area, unchecking the box hides
     * the area. 
     */
    asnn2.showHideByCheckbox = function(checkboxElem, areaElem) {
    	var area = jQuery(areaElem);
    	if (checkboxElem.checked) {
    		area.show();
    	}
    	else {
    		area.hide();
    	}
    };

    /**
     * Setup the element for a Assignment Submission Version. This includes
     * hooking up the (un)collapse actions, as well as the Ajax used to mark
     * feedback as read when the div is expanded.
     *
     * If the markup changes, this will need to change as well as it depends
     * on the structure.
     */
    asnn2.assnSubVersionDiv = function (elementId, feedbackRead, submissionId, versionId) {
        var escElemId = elementId.replace(/:/g, "\\:");
        var versionHeader = jQuery('#'+escElemId+ ' h3');
        var arrow = versionHeader.find("img:first");
        var toggled = jQuery('#'+escElemId+ ' div')
        var envelope = versionHeader.find("img:last");
        versionHeader.click(function() {
            toggle_hideshow(arrow, toggled);
            if (envelope.attr('src') == NEW_FEEDBACK_IMAGE) {
                envelope.attr('src', READ_FEEDBACK_IMAGE);
                mark_feedback(submissionId, versionId);
            }
        });
    };
    
    asnn2.studentSubmissionConfirm = function(buttonform) {
    	asnn2.turnOnPortalOverlay();
        confirmDialog = jQuery('#submit-confirm-dialog');
        dialogOptions = {
            resizable: false,
            width: 500,
            modal: true,
            overlay: {
                opacity: 0.5,
                background: "#eee"
            }
        };
        var submitButton = buttonform;
        jQuery('#submission-confirm-button').click( function (event) {
            confirmDialog.dialog('destroy');
            asnn2.turnOffPortalOverlay();
            submitButton.onclick = function (event) { return true };
            submitButton.click();
        });

        jQuery('#submission-cancel-button').click( function (event) {
            confirmDialog.dialog('destroy');
            asnn2.turnOffPortalOverlay();
        });

        confirmDialog.dialog(dialogOptions).show();
        return false;
    };
    
    /**
     * Turns on the 2.x portals background overlay
     */
    asnn2.turnOnPortalOverlay = function() {
    	jQuery("body", parent.document).append("<div id='portalMask' style='width:100%;height:100%'></div>");
    	jQuery("#" + iframeId, parent.document).css("z-index", "9001").css("position", "relative").css("background", "#fff");
    };
    
    /**
     * Turns off the 2.x portal background overlay
     */
    asnn2.turnOffPortalOverlay = function() {
    	jQuery("#portalMask", parent.document).trigger("unload").unbind().remove();
        jQuery("#" + iframeId, parent.document).css("z-index", "0");
    };
    
    /**
     * Aligns the "Apply to Unassigned" box with the grading column 
     */
    asnn2.alignGrading = function() {
        var p = jQuery("td.grade:first");
        var position = p.position();
        if (position) {
            var applyToUnassigned = jQuery('#unassigned-apply');
            applyToUnassigned.attr("style", "margin-left:" + position.left + "px");
        }
    };
    
    
})(jQuery, asnn2);
