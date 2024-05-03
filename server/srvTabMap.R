############ srvTabMap.R ############


# render map and add stations
output$map <- renderLeaflet({
  l<-leaflet() %>%
    # addProviderTiles(providers[[input$map_choose]]) %>%
    addTiles("", group = "Borders only") %>% 
    addProviderTiles("OpenStreetMap.Mapnik", group ="OSM") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addMeasure(position = "bottomleft", 
      primaryLengthUnit = "meters",  
      primaryAreaUnit = "sqmeters",
      activeColor = "blue",
      completedColor = "red") %>% 
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
    addScaleBar(position="bottomright") %>% 
    addLayersControl(
      baseGroups = c("OSM", "Satellite"),
      overlayGroups = c("Antenna Cones"),
      options = layersControlOptions(collapsed = TRUE), 
      position = c("topright")
    )
})

outputOptions(output, "map", suspendWhenHidden = FALSE)

observeEvent(global$receivers, ignoreNULL = T, ignoreInit = T, {
  req(global$receivers)
  
  leafletProxy("map") %>%
    clearGroup("Stations") %>%
    clearGroup("Antenna Cones") %>%
    addStations(data = global$receivers, group="Stations") %>%
    addAntennaeCones(
      calculate_antennae_cones(global$receivers), 
      group="Antenna Cones"
    ) %>% fitBounds(
      lng1 = min(global$receivers$Longitude, na.rm = T),
      lat1 = min(global$receivers$Latitude, na.rm = T),
      lng2 = max(global$receivers$Longitude, na.rm = T),
      lat2 = max(global$receivers$Latitude, na.rm = T)
    )
})


color_palette <- reactive({
  req(filtered_data())
  pal <- colorNumeric(
    palette = "Reds",
    domain = filtered_data()$max_signal,
    reverse = FALSE)
  pal
})


observeEvent(global$triangulation, ignoreNULL = T, ignoreInit = T, {
  leafletProxy("map") %>% clearGroup("triangulations")
  if (length(unique(global$triangulation$freq_tag)) > 1) {
    pal <- colorFactor("Dark2", domain = global$triangulation$freq_tag)
    values <- global$triangulation$freq_tag
    labFormat <- labelFormat
    title <- "Tag"
  } else {
    values <- as.numeric(global$triangulation$timestamp)
    pal <- colorNumeric(palette = "Spectral", domain = values)
    labFormat <- function(type, x) {format(as.POSIXct(x, origin="1970-01-01"), tz="UTC", format="%F %T" )}
    title <- "Timestamp"
  }
  req(any(!is.na(global$triangulation$pos.X)))
  leafletProxy("map") %>% 
    addCircles(
      lng = global$triangulation$pos.X, 
      lat = global$triangulation$pos.Y, 
      label = as.POSIXct(global$triangulation$timestamp, tz="UTC", origin="1970-01-01"),
      radius = 6, 
      group = "triangulations",
      color = pal(values),
      opacity = 0.9,
      fillOpacity = 0.5,
      stroke = 6
    ) %>%
      addLegend(
        position="bottomright", 
        pal = pal, 
        values = values, 
        labFormat = labFormat,
        title = title
      )
})

observeEvent(global$extra_points, {
  req(global$extra_points)  
  # Variable to store group names for layer control
  group_names <- vector("list", length(global$extra_points))
  
  # Iterate over each item in the list; each item is assumed to be a list of data frames
  for(i in seq_along(global$extra_points)) {
    track_list <- global$extra_points[[i]]
    
    # Now iterate over each data frame within this sub-list
    for(df_name in names(track_list)) {
      df <- track_list[[df_name]]  # Access the actual data frame by name
      group_name <- paste("Track", i, "-", df_name)
      
      if ("lon" %in% names(df) && "lat" %in% names(df)) {
        leafletProxy("map") %>% 
          clearGroup(group_name) %>% 
          addCircles(
            lng = df$lon,
            lat = df$lat,
            radius = 7,
            weight = 7,
            popup = paste("Track:", df_name, "<br>Longitude:", df$lon, "<br>Latitude:", df$lat),
            group = group_name
          )
        # Collect group names for layer control
        group_names[[i]] <- append(group_names[[i]], group_name)
      }
    }
  }
  
  group_names <- unlist(group_names)
  
  leafletProxy("map") %>% 
    removeLayersControl() %>%
    addLayersControl(
      baseGroups = c("OSM", "Satellite"),
      overlayGroups = c("Antenna Cones", group_names),
      options = layersControlOptions(collapsed = TRUE), 
      position = c("bottomleft")
    )
})

observeEvent(global$map_markers, ignoreNULL = T, ignoreInit = T, {
  leafletProxy("map") %>%
    addCircleMarkers(
      lat=global$map_markers$Latitude,
      lng=global$map_markers$Longitude, 
      group="user_markers", 
      layerId=paste0("marker_",seq_len(nrow(global$map_markers))), 
      label = global$map_markers$Comment
    )
})

################################# deactivated code ################################################
# #
# observe({
#   req(global$bearing)
#   updateSliderInput(session,"map_choose_single_data_set",min = 1,max = nrow(global$bearing))
# })

##add extra spatial points
# observeEvent(input$add_data,{
#   req(gpx_data())
#   leafletProxy("map") %>% addMarkers(data=gpx_data(),label=~paste(time,session_start))
#   mytrack<-subset(gpx_data(),timestamp>=(selected_time()-30)&timestamp<=(selected_time()+30))
#   if(nrow(mytrack)>0){
#    leafletProxy("map") %>% addCircles(lng = mytrack$lon, lat=mytrack$lat, radius=5, label=mytrack$timestamp, group = "GPX")
#   }
# })

# # render data info text 
# output$map_signal_select_prop<-renderText(
#   if(input$map_activate_single_data){
#     paste0("Date and Time: ", selected_time())
#   }
# )
# 
# 
# miniplot_base<-reactive({
#   req(global$bearing)
#   if(input$map_activate_single_data){
#     ggplot(global$bearing)+geom_point(aes(timestamp,angle,color=Station))
#   }
# })
# 
# output$map_miniplot<-renderPlot({
#   req(selected_time())
#   if(input$map_activate_single_data){
#     miniplot_base() + geom_vline(xintercept=as.numeric(selected_time()))
#   }
# })


# observeEvent(input$update_map,{
#   # leafletProxy("map") %>% addAntennaeCones(antennae_cones())
#   # if(!input$select_offline_map){
#   #   print("Use offline Maps")
#   #   leafletProxy("map") %>% addTiles(
#   #     urlTemplate=paste0("http://localhost:", session$clientData$url_port,"/Tiles/{z}/{x}/{y}.png")
#   #   )
#   # }
#   # if(input$map_show_antennae_outline)
#   #   leafletProxy("map") %>% showGroup("antennae_cones")
#   # else 
#   #   leafletProxy("map") %>% hideGroup("antennae_cones")
# })

# selected_time <- reactive({
#   req((input$map_choose_single_data_set))
#   req(tm_signal_data())
#   tmp<-unique(tm_signal_data()$timestamp)
#   rv<-NULL
#   if(!input$app_live_mode){
#     rv<-tmp[order(tmp)][input$map_choose_single_data_set]
#   }else{
#     rv<-tmp[order(tmp,decreasing = TRUE)][input$map_choose_single_data_set]
#   }
#   return(rv)
# })

#add triangulations
# observeEvent(input$update_map,{
#   req(global$triangulation)
#   leafletProxy("map") %>% clearGroup("triangulations")
#   if (length(unique(global$triangulation$freq_tag))>1){
#     pal <- colorFactor("Dark2",domain=global$triangulation$freq_tag)
#     values <- global$triangulation$freq_tag
#     labFormat <- labelFormat
#     title <- "Tag"
#   } else {
#     values <- as.numeric(global$triangulation$timestamp)
#     pal <- colorNumeric(palette = "Spectral", domain = values)
#     labFormat <- function(type, x) {format(as.POSIXct(x, origin="1970-01-01"), tz="UTC", format="%F %T" )}
#     title <- "Timestamp"
#   }
#   req(any(!is.na(global$triangulation$pos.X)))
#   leafletProxy("map") %>% addCircles(lng = global$triangulation$pos.X, lat=global$triangulation$pos.Y, 
#                                      label = as.POSIXct(global$triangulation$timestamp, tz="UTC", origin="1970-01-01"),
#                                      radius=5, 
#                                      group = "triangulations",
#                                      color= pal(values)
#                                      ) %>%
#     addLegend(
#       position="bottomright", 
#       pal = pal, 
#       values = values, 
#       labFormat = labFormat,
#       title = title
#       )
#   # if(!is.null(gpx_data())){
#   #   
#   #   #Add markers for extra points
#   #     leafletProxy("map") %>%
#   #       addMarkers(data = global$extra_points, ~lon, ~lat, popup = ~info, group = ~group)
#   # 
#   #     # Update the layers control
#   #     leafletProxy("map") %>%
#   #       addLayersControl(
#   #         overlayGroups = c("GPS Data", unique(global$extra_points$group)),
#   #         options = layersControlOptions(collapsed = False)
#   #       )
#   #   
#   #   # leafletProxy("map") %>% addCircles(lng = gpx_data()[[input$lng_to_compare]], lat=gpx_data()[[input$lat_to_compare]], 
#   #   #                                   #label = as.POSIXct(global$triangulation$timestamp, tz="UTC", origin="1970-01-01"),
#   #   #                                   radius=5, 
#   #   #                                   group = "triangulations",
#   #   #                                   color="red")
#   # }
# })

# observe({
#   req(leafletProxy("map"))
#   req(selected_time())
#   req(global$bearing)
#   leafletProxy("map") %>% clearGroup("bats") %>% clearGroup("Bearings")%>% clearGroup("triangulations") %>% clearGroup("GPX")
#   if(input$map_activate_single_data){
#     data_cones<-na.omit(subset(tm_signal_data(),timestamp == selected_time()))
#     leafletProxy("map") %>% addDetectionCones(data_cones)
#     if(nrow(tm_bearing_data())>0){
#       data<-na.omit(subset(tm_bearing_data(),timestamp == selected_time()))
#       if(nrow(data)>0){
#         data<-merge(data,global$receivers[!duplicated(global$receivers$Station),c("Station","Longitude","Latitude")],by.x="Station",by.y="Station")
#         data<-cbind(data,utm=wgstoutm(data[,"Longitude"],data[,"Latitude"]))
#         colnames(data)[which(colnames(data)=='Longitude')]<-"latitude"
#         colnames(data)[which(colnames(data)=='Latitude')]<-"longitude"
#         leafletProxy("map") %>% addBearings(data)
#       }
#     }
#     tmp_pos<-na.omit(subset(global$triangulation,timestamp==selected_time()))
#     if(nrow(tmp_pos)>0){
#       leafletProxy("map") %>% addCircles(lng = tmp_pos$pos.X, 
#                                          lat = tmp_pos$pos.Y, 
#                                          label = as.POSIXct(tmp_pos$timestamp, tz="UTC", origin="1970-01-01"),
#                                          radius=5, 
#                                          group = "triangulations",
#                                          color="blue"
#       ) 
#     }
#   }
# })


# # creates basic map
# map <- reactive({
#   l<-leaflet() %>%
#     addProviderTiles(providers[[input$map_choose]]) %>%
#     addMeasure(position = "bottomleft", 
#       primaryLengthUnit = "meters",  
#       primaryAreaUnit = "sqmeters",
#       activeColor = "blue",
#       completedColor = "red") %>% 
#     addEasyButton(easyButton(
#       icon="fa-crosshairs", title="Locate Me",
#       onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
#     addScaleBar(position="bottomright")
#   if (is.null(isolate(global$map_markers)))
#     return(l)
#   l<-l %>%
#     addCircleMarkers(lat=isolate(global$map_markers$Latitude), lng=isolate(global$map_markers$Longitude), group="user_markers", layerId=paste0("marker_",seq_len(nrow(isolate(global$map_markers)))), label = isolate(global$map_markers$Comment))
# 
#   
#   
#   })


# # add features to the basic map
# observe({
#   req(leafletProxy("map"))
#   req((global$receivers))
#   req((filtered_data()))
#   req(antennae_cones())
#   leafletProxy("map") %>%
#     clearControls() %>%
#     addAntennaeCones(antennae_cones())
# })

# tm_signal_data<- eventReactive(input$map_activate_single_data,{
#   req(filtered_data())
#   tmp<-time_match_signals(filtered_data(),input$intra_station_time_error, F)
#   #no frequency tag included!!!
#   return(timematch_inter(tmp,input$time_error_inter_station))
# })

# tm_bearing_data<- eventReactive(input$map_activate_single_data,{
#   req(global$bearing)
#   #no frequency tag included!!!
#   return(timematch_inter(global$bearing,input$time_error_inter_station))
# })
