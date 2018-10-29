############ srvTabMap.R ############

# render map
output$map <- renderLeaflet({
  validate(
    need(global$receivers, "Please provide receiver data.")
  )
  map()%>%addStations(global$receivers, color="black", radius=10, group="Stations")
})

observe({
  if(is.null(leafletProxy("map"))) return(NULL)
  if(is.null((global$receivers$Longitude))) return(NULL)
  if(is.null((sorted_data()))) return(NULL)
  leafletProxy("map") %>% 
    clearControls() %>%
    addAntennaeCones() %>% 
    addLegend(position="topleft",   
              pal=color_palette(),
              values=sorted_data()$max_signal,
              title="SNR"
    )
})

observe({
  if(is.null(global$map_markers)) return(NULL)
  if(is.null((nrow(global$map_markers)>0))) return(NULL)
  leafletProxy("map") %>% addMarkers(lat=global$map_markers$Latitude, lng=global$map_markers$Longitude, group="user_markers", layerId=paste0("marker_",seq_len(nrow(global$map_markers))), label = global$map_markers$comment)
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
  req(doa_data())
  if(!input$app_live_mode){
    updateSliderInput(session,"map_choose_single_data_set",min = 1,max = length(unique(doa_data()$timestamp)))
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
    ggplot(doa_data())+geom_point(aes(timestamp,angle,color=Station))
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
  req(smoothed_curves())
  tmp<-unique(smoothed_curves()$timestamp)
  rv<-NULL
  if(!input$app_live_mode){
    rv<-tmp[order(tmp)][input$map_choose_single_data_set]
  }else{
    rv<-tmp[order(tmp,decreasing = TRUE)][input$map_choose_single_data_set]
  }
  return(rv)
})

observe({
  req(triangulations())
  leafletProxy("map") %>% addCircles(lng = triangulations()$pos.X, lat=triangulations()$pos.Y, radius=5)
})

observe({
  req(leafletProxy("map"))
  req(selected_time())
  req(doa_data())
  leafletProxy("map") %>% clearGroup("bats") %>% clearGroup("Bearings") %>% clearGroup("GPX")
  if(input$map_activate_single_data){
    data_cones<-subset(smoothed_curves(),timestamp %in% selected_time())
    if(!is.null(gpx_data())){
      mytrack<-subset(gpx_data(),timestamp>=(selected_time()-30)&timestamp<=(selected_time()+30))
      if(nrow(mytrack)>0){
        leafletProxy("map") %>% addCircles(lng = mytrack$lon, lat=mytrack$lat, radius=5, label=mytrack$timestamp, group = "GPX")
      }
    }
    leafletProxy("map") %>% addDetectionCones(data_cones)
    if(nrow(doa_data())>0){
      data<-subset(doa_data(),timestamp %in% selected_time())
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
  if(is.null(smoothed_curves())) return(NULL)
  smoothed_curves()[order(smoothed_curves()$timestamp),]
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