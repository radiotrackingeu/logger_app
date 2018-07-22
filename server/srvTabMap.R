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

observe({
  if(is.null(smoothed_curves())) return(NULL)
  updateSliderInput(session,"map_choose_single_data_set",min = 1,max = length(unique(smoothed_curves()$timestamp)))
})



# render data info text 
output$map_signal_select_prop<-renderText(
  if(input$map_activate_single_data){
    paste0("Date and Time: ", selected_time())
  }
)

miniplot_base<-reactive({
  if(input$map_activate_single_data){
    ggplot(doa_smoothed())+geom_point(aes(timestamp,angle,color=Station))
  }
})

output$map_miniplot<-renderPlot({
  if(input$map_activate_single_data){
    miniplot_base() + geom_vline(xintercept=as.numeric(selected_time()))
  }
})


observe({
  if(!input$select_offline_map){
    print("Use offline Maps")
    leafletProxy("map") %>% addTiles(
      urlTemplate=paste0("http://localhost:", session$clientData$url_port,"/Tiles/{z}/{x}/{y}.png")
    )
  }
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

selected_time <- reactive({
  tmp<-unique(smoothed_curves()$timestamp)
  tmp[order(tmp)][input$map_choose_single_data_set]
})


observeEvent(input$map_choose_single_data_set,{
  # validate(
  #   need(global$receivers, "Please provide file with antennae specifications."),
  #   need(sorted_data(), "Please have a look at the filter settings.")
  # )
  if (is.null(leafletProxy("map")))
    return(NULL)
  leafletProxy("map") %>% clearGroup("bats") %>% clearPopups() %>% clearMarkers() %>% clearGroup("Bearing")
  if(input$map_activate_single_data){
    data<-subset(doa_smoothed(),timestamp==selected_time())
    data_cones<-subset(smoothed_curves(),timestamp==selected_time())
    leafletProxy("map") %>% addDetectionCones(data_cones)
    if(nrow(data)>0){
      data<-merge(data,global$receivers[!duplicated(global$receivers$Station),c("Station","Longitude","Latitude")],by.x="Station",by.y="Station")
      data<-cbind(data,utm=wgstoutm(data[,"Longitude"],data[,"Latitude"]))
      colnames(data)[which(colnames(data)=='Longitude')]<-"pos_x"
      colnames(data)[which(colnames(data)=='Latitude')]<-"pos_y"
      leafletProxy("map") %>% addBearings(data)
    }
  }
})

sorted_data <- reactive({
  if(is.null(smoothed_curves())) return(NULL)
  smoothed_curves()[order(smoothed_curves()$timestamp),]
})

# creates basic map

map <- reactive({
  leaflet() %>%
    addProviderTiles(providers[[input$map_choose]]) %>%
    addMeasure(position = "bottomleft", 
    primaryLengthUnit = "meters",  
    primaryAreaUnit = "sqmeters",
    activeColor = "blue",
    completedColor = "red") %>% addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>% addScaleBar(position="bottomright")
})