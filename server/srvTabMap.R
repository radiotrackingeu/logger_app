############ srvTabMap.R ############

# render map
output$map <- renderLeaflet({
  validate(
    need(global$receivers, "Please provide receiver data.")
  )
  map() %>% 
    addStations(global$receivers, color="black", radius=10, group="Stations") %>% 
    addAntennaeCones() %>% 
    addLegend(position="topleft",   
      pal=color_palette(),
      values=sorted_data()$max_signal,
      title="SNR"
    )
})



# render data info text 
output$map_signal_select_prop<-renderText(
  if(input$map_activate_single_data){
    paste0("Date and Time: ", sorted_data()$timestamp[input$map_choose_single_data_set])
  }
)

miniplot_base<-reactive({
  if(input$map_activate_single_data){
    ggplot(sorted_data())+geom_point(aes(timestamp,max_signal),color="blue")
  }
})

output$map_miniplot<-renderPlot({
  if(input$map_activate_single_data){
    miniplot_base() + geom_point(aes(timestamp[input$map_choose_single_data_set],max_signal[input$map_choose_single_data_set]),color="red")
  }
})


observeEvent(input$map_show_antennae_outline, {
  if(input$map_show_antennae_outline)
    leafletProxy("map") %>% showGroup("antennae_cones")
  else 
    leafletProxy("map") %>% hideGroup("antennae_cones")
})

color_palette <- reactive({
  pal <- colorNumeric(
    palette = "Reds",
    domain = sorted_data()$max_signal,
    reverse = FALSE)
  pal
})

observeEvent(input$map_choose_single_data_set,{
  # validate(
  #   need(global$receivers, "Please provide file with antennae specifications."),
  #   need(sorted_data(), "Please have a look at the filter settings.")
  # )
  if (is.null(leafletProxy("map")))
    return(NULL)
  leafletProxy("map") %>% clearGroup("bats") %>% clearPopups() %>% clearMarkers()
  if(input$map_activate_single_data){
    leafletProxy("map") %>% addDetectionCones()
  }
})

sorted_data <- reactive({
  filtered_data()[order(filtered_data()$timestamp),]
})

# creates basic map
map <- reactive({
  leaflet() %>% addProviderTiles(providers[[input$map_choose]]) %>% addMeasure(position = "bottomleft", 
    primaryLengthUnit = "meters",  
    primaryAreaUnit = "sqmeters",
    activeColor = "blue",
    completedColor = "red") %>% addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>% addScaleBar(position="bottomright")
})