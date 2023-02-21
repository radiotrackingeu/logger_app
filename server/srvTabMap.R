############ srvTabMap.R ############

antennae_cones<-reactive({
  req(global$receivers)
  calculate_antennae_cones(global$receivers)
})

#
observe({
  req(global$bearing)
  updateSliderInput(session,"map_choose_single_data_set",min = 1,max = nrow(global$bearing))
})


# render map and add stations
output$map <- renderLeaflet({
  req(global$receivers)
  map()%>%addStations(global$receivers, color="black", radius=10, group="Stations")
})

# add extra spatial points
# observeEvent(input$add_data,{
#   req(gpx_data())
#   #leafletProxy("map") %>% addMarkers(data=gpx_data(),label=~paste(time,session_start))
#   #mytrack<-subset(gpx_data(),timestamp>=(selected_time()-30)&timestamp<=(selected_time()+30))
#   #if(nrow(mytrack)>0){
#   #  leafletProxy("map") %>% addCircles(lng = mytrack$lon, lat=mytrack$lat, radius=5, label=mytrack$timestamp, group = "GPX")
#   #}
# })


# render data info text 
output$map_signal_select_prop<-renderText(
  if(input$map_activate_single_data){
    paste0("Date and Time: ", selected_time())
  }
)


miniplot_base<-reactive({
  req(global$bearing)
  if(input$map_activate_single_data){
    ggplot(global$bearing)+geom_point(aes(timestamp,angle,color=Station))
  }
})

output$map_miniplot<-renderPlot({
  req(selected_time())
  if(input$map_activate_single_data){
    miniplot_base() + geom_vline(xintercept=as.numeric(selected_time()))
  }
})


observeEvent(input$update_map,{
  leafletProxy("map") %>% addAntennaeCones(antennae_cones())
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
  req(filtered_data())
  pal <- colorNumeric(
    palette = "Reds",
    domain = filtered_data()$max_signal,
    reverse = FALSE)
  pal
})

selected_time <- reactive({
  req((input$map_choose_single_data_set))
  req(tm_signal_data())
  tmp<-unique(tm_signal_data()$timestamp)
  rv<-NULL
  if(!input$app_live_mode){
    rv<-tmp[order(tmp)][input$map_choose_single_data_set]
  }else{
    rv<-tmp[order(tmp,decreasing = TRUE)][input$map_choose_single_data_set]
  }
  return(rv)
})

#add triangulations
observeEvent(input$update_map,{
  req(global$triangulation)
  leafletProxy("map") %>% clearGroup("triangulations")
  #pal <- colorNumeric(
  #  palette = "Spectral",
  #  domain = global$triangulation$timestamp)
  pal<-colorFactor("Dark2",domain=global$triangulation$freq_tag)
  leafletProxy("map") %>% addCircles(lng = global$triangulation$pos.X, lat=global$triangulation$pos.Y, 
                                     label = as.POSIXct(global$triangulation$timestamp, tz="UTC", origin="1970-01-01"),
                                     radius=5, 
                                     group = "triangulations",
                                     color=pal(global$triangulation$freq_tag)
                                     )
  if(!is.null(gpx_data())){
    leafletProxy("map") %>% addCircles(lng = gpx_data()[[input$lng_to_compare]], lat=gpx_data()[[input$lat_to_compare]], 
                                      #label = as.POSIXct(global$triangulation$timestamp, tz="UTC", origin="1970-01-01"),
                                      radius=5, 
                                      group = "triangulations",
                                      color="red")
  }
})

observe({
  req(leafletProxy("map"))
  req(selected_time())
  req(global$bearing)
  leafletProxy("map") %>% clearGroup("bats") %>% clearGroup("Bearings")%>% clearGroup("triangulations") %>% clearGroup("GPX")
  if(input$map_activate_single_data){
    data_cones<-na.omit(subset(tm_signal_data(),timestamp == selected_time()))
    leafletProxy("map") %>% addDetectionCones(data_cones)
    if(nrow(tm_bearing_data())>0){
      data<-na.omit(subset(tm_bearing_data(),timestamp == selected_time()))
      if(nrow(data)>0){
        data<-merge(data,global$receivers[!duplicated(global$receivers$Station),c("Station","Longitude","Latitude")],by.x="Station",by.y="Station")
        data<-cbind(data,utm=wgstoutm(data[,"Longitude"],data[,"Latitude"]))
        colnames(data)[which(colnames(data)=='Longitude')]<-"latitude"
        colnames(data)[which(colnames(data)=='Latitude')]<-"longitude"
        leafletProxy("map") %>% addBearings(data)
      }
    }
    tmp_pos<-na.omit(subset(global$triangulation,timestamp==selected_time()))
    if(nrow(tmp_pos)>0){
      leafletProxy("map") %>% addCircles(lng = tmp_pos$pos.X, 
                                         lat = tmp_pos$pos.Y, 
                                         label = as.POSIXct(tmp_pos$timestamp, tz="UTC", origin="1970-01-01"),
                                         radius=5, 
                                         group = "triangulations",
                                         color="blue"
      ) 
    }
  }
})

# creates basic map
map <- reactive({
  l<-leaflet() %>%
    addProviderTiles(providers[[input$map_choose]]) %>%
    addMeasure(position = "bottomleft", 
      primaryLengthUnit = "meters",  
      primaryAreaUnit = "sqmeters",
      activeColor = "blue",
      completedColor = "red") %>% 
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
    addScaleBar(position="bottomright")
  if (is.null(isolate(global$map_markers)))
    return(l)
  l<-l %>%
    addCircleMarkers(lat=isolate(global$map_markers$Latitude), lng=isolate(global$map_markers$Longitude), group="user_markers", layerId=paste0("marker_",seq_len(nrow(isolate(global$map_markers)))), label = isolate(global$map_markers$Comment))
})

# add features to the basic map
observe({
  req(leafletProxy("map"))
  req((global$receivers))
  req((filtered_data()))
  req(antennae_cones())
  leafletProxy("map") %>% 
    clearControls() %>%
    addAntennaeCones(antennae_cones())
})

tm_signal_data<- eventReactive(input$map_activate_single_data,{
  req(filtered_data())
  tmp<-time_match_signals(filtered_data(),input$intra_station_time_error, F)
  #no frequency tag included!!!
  return(timematch_inter(tmp,input$time_error_inter_station))
})

tm_bearing_data<- eventReactive(input$map_activate_single_data,{
  req(global$bearing)
  #no frequency tag included!!!
  return(timematch_inter(global$bearing,input$time_error_inter_station))
})
