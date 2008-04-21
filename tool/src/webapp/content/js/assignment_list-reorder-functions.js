jQuery(document).ready(function(){
	jQuery("td#sortable").Sortable({
		accept: "sortableitem",
		axis: "vertically",
      floats: true,
      helperclass: "helperclass",
      onStop: redrawTableY
	});
   //match heights of <li> and <tr>
   jQuery("table#reorder-table tr.datarow").each(function(i){
      li = jQuery("#sortable div").get(i);
      this.id='trow_' + li.id.substring(3);
      if(jQuery(li).height() > jQuery(this).height()){
         jQuery(this).height(jQuery(li).height());
      }else{
         jQuery(li).height(jQuery(this).height());
      }
   });
   //add cords
   jQuery("table#reorder-table > tbody > tr.datarow").each(function(i){
      jQuery(this).children("td").each(function(j){
         this.id='cell_' + i + '_' + j;
      });
   });
   jQuery(jQuery("table#reorder-table tr").get(1)).hover(function(){
      jQuery(this).css("background-color", "#fff");
   }, function(){});

});

redrawTableY = function(){
   serial = jQuery.SortSerialize('sortable');
   jQuery.get("ajax-callback?" + serial.hash.replace(/[\[\]]/g, ""));

   jQuery("table#reorder-table tr.datarow").each(function(i){
   	jQuery(this).animate({opacity: 0}, 10);
   })

	setTimeout(function(){
		jQuery("#sortable > div").each(function(i){
			jQuery("#reorder-table > tbody").append(jQuery("#trow_" + this.id.substring(3)));
		});
	
		jQuery("table#reorder-table tr.datarow").each(function(i){
			jQuery(this).animate({opacity: 0}, 100 * (i+1)).animate({opacity: 1}, 500);//timeout trick
		})
   }, 10);
}
