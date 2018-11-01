############ srvTabMap.R ############

# render map
output$map <- renderLeaflet({
  req(global$receivers)
  map()%>%addStations(global$receivers, color="black", radius=10, group="Stations")
})

# add features
observe({
  req(leafletProxy("map"))
  req((global$receivers))
  req((sorted_data()))
  leafletProxy("map") %>% 
    clearControls() %>%
    addAntennaeCones() %>% 
    addLegend(position="topleft",   
              pal=color_palette(),
              values=sorted_data()$strength,
              title="SNR"
    )
})


# add markers
observe({
  req(global$map_markers)
  leafletProxy("map") %>% addMarkers(lat=global$map_markers$Latitude, lng=global$map_markers$Longitude, group="user_markers", layerId=paste0("marker_",seq_len(nrow(global$map_markers))), label = global$map_markers$comment)
})

# add extra spatial points
observeEvent(input$add_data,{
  req(gpx_data())
  leafletProxy("map") %>% addMarkers(data=gpx_data(),label=~paste(time,session_start))
  #mytrack<-subset(gpx_data(),timestamp>=(selected_time()-30)&timestamp<=(selected_time()+30))
  #if(nrow(mytrack)>0){
  #  leafletProxy("map") %>% addCircles(lng = mytrack$lon, lat=mytrack$lat, radius=5, label=mytrack$timestamp, group = "GPX")
  #}
})

observeEvent(input$map_click,{
  updateTextInput(session,"map_lat",value = round(input$map_click$lat,digits=5))
  updateTextInput(session,"map_lng",value = round(input$map_click$lng,digits=5))
  updateTextInput(session,"map_comment", value = format(Sys.time(),"%F %T"))
})

observeEvent(input$map_add_marker,{
  # leafletProxy("map") %>% addMarkers(lat=as.numeric(input$map_lat), lng=as.numeric(input$map_lng), group="user_markers", label=input$map_comment, layerId = paste0("marker_",nrow(global$map_markers)+1))
  global$map_markers<-unique.data.frame(rbind(global$map_markers,data.frame("Latitude"=as.numeric(input$map_lat), "Longitude"=as.numeric(input$map_lng), "comment"=input$map_comment, timestamp=format(Sys.time(),"%F %T"))))
})

observeEvent(input$map_marker_click,{
  leafletProxy("map") %>% removeMarker(input$map_marker_click$id)
  id<-strsplit(x = input$map_marker_click$id, split="_")
  id<-as.numeric(unlist(id)[2])
  global$map_markers <- global$map_markers[-c(id),]
})

observeEvent(input$map_rm_markers,{
  leafletProxy("map") %>% clearGroup("user_markers")
  global$map_markers <- NULL
})

observe({
  req(global$bearing)
  if(!input$app_live_mode){
    updateSliderInput(session,"map_choose_single_data_set",min = 1,max = length(unique(global$bearing$timestamp)))
  }else{
    updateSliderInput(session,"map_choose_single_data_set",min = 1,max = input$live_update_interval)
  }
})

observeEvent(input$minus, {
  updateSliderInput(session,"map_choose_single_data_set", value = input$map_choose_single_data_set - 1)
})

observeEvent(input$plus, {
  updateSliderInput(session,"map_choose_single_data_set", value = input$map_choose_single_data_set + 1)
})  



# render data info text 
output$map_signal_select_prop<-renderText(
  if(input$map_activate_single_data){
    paste0("Date and Time: ", selected_time())
  }
)

miniplot_base<-reactive({
  if(input$map_activate_single_data){
    ggplot(global$bearing)+geom_point(aes(timestamp,angle,color=Station))
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
  req((input$map_choose_single_data_set))
  req(global$bearing)
  tmp<-unique(global$bearing$timestamp)
  rv<-NULL
  if(!input$app_live_mode){
    rv<-tmp[order(tmp)][input$map_choose_single_data_set]
  }else{
    rv<-tmp[order(tmp,decreasing = TRUE)][input$map_choose_single_data_set]
  }
  return(rv)
})

#add triangulations
observe({
  req(global$triangulation)
  leafletProxy("map") %>% clearGroup("triangulations")
  pal <- colorNumeric(
    palette = "Spectral",
    domain = global$triangulation$timestamp)
  leafletProxy("map") %>% addCircles(lng = global$triangulation$pos.X, lat=global$triangulation$pos.Y, 
                                     label = as.POSIXct(global$triangulation$timestamp, tz="UTC", origin="1970-01-01"),
                                     radius=5, 
                                     group = "triangulations",
                                     color=pal(global$triangulation$timestamp)
                                     )
})

observe({
  req(leafletProxy("map"))
  req(selected_time())
  req(global$bearing)
  leafletProxy("map") %>% clearGroup("bats") %>% clearGroup("Bearings") %>% clearGroup("GPX")
  if(input$map_activate_single_data){
    data_cones<-na.omit(subset(filtered_data(),timestamp > selected_time() - input$time_error_inter_station & timestamp < selected_time() + input$time_error_inter_station))
    leafletProxy("map") %>% addDetectionCones(data_cones)
    if(nrow(global$bearing)>0){
      data<-na.omit(subset(global$bearing,timestamp > selected_time() - input$time_error_inter_station & timestamp < selected_time() + input$time_error_inter_station))
      if(nrow(data)>0){
        data<-merge(data,global$receivers[!duplicated(global$receivers$Station),c("Station","Longitude","Latitude")],by.x="Station",by.y="Station")
        data<-cbind(data,utm=wgstoutm(data[,"Longitude"],data[,"Latitude"]))
        colnames(data)[which(colnames(data)=='Longitude')]<-"pos_x"
        colnames(data)[which(colnames(data)=='Latitude')]<-"pos_y"
        leafletProxy("map") %>% addBearings(data)
      }
    }
  }
})

sorted_data <- reactive({
  if(is.null(global$bearing)) return(NULL)
  global$bearing[order(global$bearing$timestamp),]
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
    addMarkers(lat=isolate(global$map_markers$Latitude), lng=isolate(global$map_markers$Longitude), group="user_markers", layerId=paste0("marker_",seq_len(nrow(isolate(global$map_markers)))), label = isolate(global$map_markers$comment))
})