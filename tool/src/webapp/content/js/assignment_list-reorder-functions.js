$(document).ready(function(){
	$("ul.sortable").Sortable({
		accept: "sortableitem",
		axis: "vertically",
      floats: false,
      hoverclass: "hoverClass",
      onStop: redrawTableY
	});
   //match heights of <li> and <tr>
   $("table#reorder-table tr.datarow").each(function(i){
      li = $("ul.sortable li").get(i);
      this.id='trow_' + li.id.substring(3);
      if($(li).height() > $(this).height()){
         $(this).height($(li).height());
      }else{
         $(li).height($(this).height());
      }
   });
   //add cords
   $("table#reorder-table > tbody > tr.datarow").each(function(i){
      $(this).children("td").each(function(j){
         this.id='cell_' + i + '_' + j;
      });
   });
   $($("table#reorder-table tr").get(1)).hover(function(){
      $(this).css("background-color", "#fff");
   }, function(){});

});

redrawTableY = function(){
   serial = $.SortSerialize('sortable');
   alert(serial.hash);
   $("ul.sortable > li").each(function(i){
      $("#reorder-table > tbody").append($("#trow_" + this.id.substring(3)));
   });
}
