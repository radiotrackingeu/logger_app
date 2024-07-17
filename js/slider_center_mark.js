Shiny.addCustomMessageHandler(
  "slider_set_center_mark", 
  function(params) {
    console.log(params);
    if ($("#"+params.id_center_mark).length === 0) {
      $("#"+params.id_slider).siblings(".irs").children(".irs-grid").append("<span id='"+params.id_center_mark+"' class='irs-grid-pol center_mark' style='left: 0%;'></span>");
    }
    if ($("#"+params.id_center_mark+"_label").length === 0) {
      $("#"+params.id_slider).siblings(".irs").children(".irs-grid").append("<span id='"+params.id_center_mark+"_label' class='irs-grid-text center_mark' style='left: 0%;'></span>");
    }
    margin_l = $("#"+params.id_slider).siblings(".irs").children(".irs-grid").children(".js-grid-text-0").css("margin-left");
    center_mark = $("#"+params.id_center_mark);
    center_mark.css({ left: params.value});
    center_mark_label = $("#"+params.id_center_mark+"_label");
    center_mark_label.css({ 'left': params.value, 'margin-left': margin_l});
    center_mark_label.html(params.label);
  }
)