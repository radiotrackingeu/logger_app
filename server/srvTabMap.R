############ srvTabMap.R ############

selected_tri <- ""
selected_station <- ""

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
      overlayGroups = c("Antenna Cones", "Bearings"),
      options = layersControlOptions(collapsed = TRUE), 
      position = c("topright")
    ) %>% hideGroup("Bearings")
})

outputOptions(output, "map", suspendWhenHidden = FALSE)

observeEvent(global$bearing, ignoreNULL = T, ignoreInit = T, {
  updateSliderInput(
    inputId = "slider_bearings_time", 
    min = min(global$bearing$timestamp, na.rm = T), 
    max = max(global$bearing$timestamp, na.rm = T), 
    value = c(min(global$bearing$timestamp, na.rm = T), max(global$bearing$timestamp, na.rm = T)), 
    timeFormat = "%Y-%m-%d %H:%M"
  )
})

observeEvent(global$receivers, ignoreNULL = F, ignoreInit = T, {
  leafletProxy("map") %>%
    clearGroup("Stations") 
  if (!is.null(global$receivers) && nrow(global$receivers) > 0)
    leafletProxy("map") %>%
      addStations(data = global$receivers, group="Stations") %>%
      fitBounds(
        lng1 = min(global$receivers$Longitude, na.rm = T),
        lat1 = min(global$receivers$Latitude, na.rm = T),
        lng2 = max(global$receivers$Longitude, na.rm = T),
        lat2 = max(global$receivers$Latitude, na.rm = T)
      )
})

observeEvent(cones(), ignoreNULL = F, {
  leafletProxy("map") %>%
    clearGroup("Antenna Cones") 
  if (!is.null(cones()) && length(cones()) > 0)
    leafletProxy("map") %>%
      addAntennaeCones(
        cones(), 
        group="Antenna Cones"
      )
})

cones <- eventReactive(global$receivers, ignoreNULL = F, {
  if (is.null(global$receivers) || nrow(global$receivers) == 0)
    return(NULL)
  return(calculate_antennae_cones(global$receivers))
})

color_palette <- reactive({
  req(filtered_data())
  pal <- colorNumeric(
    palette = "Reds",
    domain = filtered_data()$max_signal,
    reverse = FALSE)
  pal
})

tri_palette <- reactive({
  if (!"Bearings" %in% input$map_groups){
    req(!is.null(global$triangulation) || !is.null(global$bearing))
    pal <- list()
    if ((!is.null(global$triangulation) || !is.null(global$bearing)) && uniqueN(c(global$triangulation$freq_tag, global$bearing$freq_tag)) > 1) {
      pal$values <- sort(unique(c(global$triangulation$freq_tag, global$bearing$freq_tag)))
      pal$pal <- colorFactor("Dark2", domain = pal$values)
      pal$labFormat <- function(type, x) {sprintf(x)}
      pal$title <- "Tag"
    } else {
      pal$values <- sort(unique(c(as.numeric(global$bearing$timestamp), as.numeric(global$triangulation$timestamp))))
      pal$pal <- colorNumeric(
        palette = rainbow(
          n = ceiling(as.numeric(max(global$bearing$timestamp, global$signals$timestamp))) - trunc(as.numeric(min(global$bearing$timestamp, global$signals$timestamp)))
        ), 
        domain = pal$values
      )
      pal$labFormat <- function(type, x) {
        format(as.POSIXct(x, origin = "1970-01-01", tz = "GMT"), "%F %H:%M", tz = "GMT")
      }
      pal$title <- "Timestamp" 
    }
    pal
  } else {
    req(!is.null(filtered_bearings()) && nrow(filtered_bearings())>0)
    pal <- list()
    if (uniqueN(global$bearing$freq_tag) > 1) {
      pal$values <- sort(unique(global$bearing$freq_tag))
      pal$pal <- colorFactor("Dark2", domain = pal$values)
      pal$labFormat <- function(type, x) {sprintf(x)}
      pal$title <- "Tag (time-filtered)"
    } else {
      pal$values <- sort(unique(as.numeric(filtered_bearings()$timestamp)))
      pal$pal <- colorNumeric(
        palette = rainbow(
          n = ceiling(max(as.numeric(filtered_bearings()$timestamp), na.rm = T)) - trunc(min(as.numeric(filtered_bearings()$timestamp), na.rm = T))
        ), 
        domain = pal$values
      )
      pal$labFormat <- function(type, x) {
        format(as.POSIXct(x, origin = "1970-01-01", tz = "GMT"), "%F %H:%M", tz = "GMT")
      }
      pal$title <- "Timestamp (filtered)" 
    }
    pal
  }
})

observeEvent(global$triangulation, ignoreNULL = T, ignoreInit = T, {
  leafletProxy("map") %>% clearGroup("triangulations") %>% removeControl("legend_tri")
  
  req(any(!is.na(global$triangulation$pos.X)))
  if (length(unique(global$triangulation$freq_tag))>1){
    fColor <- global$triangulation$freq_tag
  } else{
    fColor <- global$triangulation$timestamp
  }
  leafletProxy("map") %>% 
    addCircles(
      lng = global$triangulation$pos.X, 
      lat = global$triangulation$pos.Y, 
      label = lapply(
        paste0(
          "Time: ", format(as.POSIXct(global$triangulation$timestamp, origin = "1970-01-01", tz = "GMT"), "%d.%m. %H:%M:%S", tz = "GMT"), "<br>",
          "Tag: ", global$triangulation$freq_tag, "<br>",
          "Longitude: ", global$triangulation$pos.X, "<br>",
          "Latitude: ", global$triangulation$pos.Y, "<br>",
          "bIds: ", global$triangulation$bearing_bIds, "<br>",
          "Stations: ", global$triangulation$tri_stations, "<br>",
          "tId: ", global$triangulation$tId
        ), 
        HTML
      ),
      radius = 6, 
      group = "triangulations",
      color = tri_palette()$pal(fColor),
      opacity = 0.9,
      fillOpacity = 0.5,
      stroke = 6,
      layerId = global$triangulation$tId
    )
})

observeEvent(tri_palette(), ignoreNULL = T, {
  leafletProxy("map") %>%
    addLegend(
      position="bottomright", 
      pal = tri_palette()$pal, 
      values = tri_palette()$values, 
      labFormat = tri_palette()$labFormat,
      title = tri_palette()$title,
      layerId = "legend_tri"
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
      overlayGroups = c("Antenna Cones", "Bearings", group_names),
      options = layersControlOptions(collapsed = TRUE), 
      position = c("bottomleft")
    ) %>% hideGroup("Bearings")
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

observeEvent(input$map_shape_click, ignoreNULL = T, ignoreInit = T, {
  # clicked on triangulation?
  if (input$map_shape_click$group %in% c("triangulations", "active_tri")) {
    leafletProxy("map") %>%
      clearGroup("bearings") %>%
      clearGroup("cones")
    # clicked on currently not selected triangulation?
    if (!selected_tri == input$map_shape_click$id) {
      tri <- global$triangulation[global$triangulation$tId == input$map_shape_click$id,]
      fColor <- ifelse(length(unique(global$triangulation$freq_tag))>1, tri$freq_tag, as.numeric(tri$timestamp))
      if (length(unique(global$triangulation$freq_tag))>1){
        fColor <- tri$freq_tag
      } else{
        fColor <- tri$timestamp
      }
      # highlight selected triangulation
      leafletProxy("map") %>%
        clearGroup("active_tri") %>%
        addCircles(        
          data = tri, 
          lng = ~pos.X, 
          lat = ~pos.Y, 
          label = HTML(
            "Time: ", format(as.POSIXct(tri$timestamp, origin = "1970-01-01", tz = "GMT"), "%d.%m. %H:%M:%S", tz = "GMT"), "<br>",
            "Tag: ", tri$freq_tag, "<br>",
            "Longitude: ", tri$pos.X, "<br>",
            "Latitude: ", tri$pos.Y, "<br>",
            "bIds: ", tri$bearing_bIds, "<br>",
            "Stations: ", tri$tri_stations, "<br>",
            "tId: ", tri$tId
          ), 
          group = "active_tri",
          radius = 11,
          weight = 7,
          fill = T,
          fillOpacity = 0,
          fillColor = tri_palette()$pal(fColor),
          color = "white",
          opacity = 1,
          layerId = ~tId
        )
      
      bearings <- global$bearing[bId %in% tstrsplit(tri$bearing_bIds,"/")]
      # only two bearings? Draw lines from tri to stations.
      if (bearings[,.N]==2){
        by (bearings, seq_len(bearings[,.N]), function(b) {
          leafletProxy("map") %>%
            addPolylines(
              lng=c(tri$pos.X, b$longitude),
              lat=c(tri$pos.Y, b$latitude),
              color = "#f542da",
              dashArray = "4 6",
              group="bearings",
              weight = 3,
              opacity = 1,
              label = HTML(
                "Time: ", format(as.POSIXct(b$timestamp, origin = "1970-01-01", tz = "GMT"), "%d.%m. %H:%M:%S", tz = "GMT"), "<br>",
                "Timeslot: ", format(as.POSIXct(b$time_matched, origin = "1970-01-01", tz = "GMT"), "%d.%m. %H:%M:%S", tz = "GMT"),"<br>",
                "Tag: ", b$freq_tag, "<br>",
                "Station:", b$station, "<br>",
                "Angle: ", b$angle, "<br>",
                "Strength: ", b$strength
              )
            )
        })
      # more than 2 bearings? Draw lines from all involved stations. Length defined by signal strength.
      } else {
        bearings[, c("dest_lon", "dest_lat") := as.data.table(getHeadingCoords(bearings$longitude, bearings$latitude, angle, estimateDist(bearings$strength, minLength = 1200)))]
        a_ply(.data = bearings, .margins = 1, .expand = F, .fun = function(b) {
          leafletProxy("map") %>%
            addPolylines(
              lng = c(
                b$longitude, 
                b$dest_lon
              ),
              lat = c(
                b$latitude, 
                b$dest_lat
              ),
              color = "#f542da",
              dashArray = "4 6", 
              group="bearings",
              weight = 3,
              opacity = 1,
              label = HTML(
                "Time: ", format(as.POSIXct(b$timestamp, origin = "1970-01-01", tz = "GMT"), "%d.%m. %H:%M:%S", tz = "GMT"), "<br>",
                "Timeslot: ", format(as.POSIXct(b$time_matched, origin = "1970-01-01", tz = "GMT"), "%d.%m. %H:%M:%S", tz = "GMT"),"<br>",
                "Tag: ", b$freq_tag, "<br>",
                "Station:", b$station, "<br>", 
                "Angle: ", b$angle, "<br>", 
                "Strength: ", b$strength, "<br>",
                "bIds: ", b$bId
              )
            )
        })
      }
      leafletProxy("map") %>%
        addDetectionCones(cones(), bearings)
      selected_tri <<- input$map_shape_click$id
    } else {
      leafletProxy("map") %>%
        clearGroup("active_tri")
      selected_tri <<- ""
    }
  }
})

observeEvent(input$map_marker_click, ignoreNULL = T, ignoreInit = T, {
  if (input$map_marker_click$group == "Stations") {
    leafletProxy("map") %>%
      clearGroup("st_bearings")

    req(global$bearing)
    if (!selected_station == input$map_marker_click$id) {
      clicked_station <- unique(global$receivers[Station == strsplit(x=input$map_marker_click$id, split = "%", fixed = T)[[1]][1]], by = c("Station"))
      bearings <- global$bearing[station == clicked_station$Station][!is.na(angle)]
      if (bearings[, .N] > 0) {
        if (uniqueN(global$bearing$freq_tag) > 1){
          bColor <- bearings$freq_tag
        } else{
          bColor <- bearings$timestamp
        }
        bearings[, c("dest_lon", "dest_lat") := as.data.table(getHeadingCoords(clicked_station$Longitude, clicked_station$Latitude, bearings$angle, estimateDist(.SD[, strength], minLength = 100)))]
        bearings[, color := tri_palette()$pal(bColor)]
        a_ply(.data = bearings, .margins = 1, .expand = F, .fun = function(b) {
          leafletProxy("map") %>%
            addPolylines(
              lng = c(
                clicked_station$Longitude,
                b$dest_lon
              ),
              lat = c(
                clicked_station$Latitude,
                b$dest_lat
              ),
              color =  b$color,
              group="st_bearings",
              weight = 1.5,
              opacity = 0.6,
              label = HTML(
                "Time: ", format(as.POSIXct(b$timestamp, origin = "1970-01-01", tz = "GMT"), "%d.%m. %H:%M:%S", tz = "GMT"), "<br>",
                "Timeslot: ", format(as.POSIXct(b$time_matched, origin = "1970-01-01", tz = "GMT"), "%d.%m. %H:%M:%S", tz = "GMT"),"<br>",
                "Tag: ", b$freq_tag, "<br>",
                "Angle: ", b$angle, "<br>",
                "Strength: ", b$strength, "<br>",
                "bIds: ", b$bId
              )
            )
        })
      } else {
        showNotification(HTML("No bearings on ", clicked_station$Station, "<br> Adjust filters date, time or bearing method to see more."), type="message", duration = 3)
      }
      selected_station <<- paste0(clicked_station$Station, "%", format(clicked_station$Longitude, scientific=F), "%", format(clicked_station$Latitude, scientific=F))
    } else {
      selected_station <<- ""
    }
  }
})

filtered_bearings <- reactive({
  req(global$bearing)
  req("Bearings" %in% input$map_groups)
  global$bearing[timestamp %between% input$slider_bearings_time]
}) %>% debounce(millis = 750)

observeEvent(filtered_bearings(), ignoreNULL = T, ignoreInit = F, {
  leafletProxy("map") %>%
    clearGroup("Bearings") %>%
    clearGroup("st_bearings")
  
  bearings <- filtered_bearings()[!is.na(angle)]
  if (bearings[, .N] > 0) {
    if (uniqueN(global$bearing$freq_tag) > 1){
      bColor <- bearings$freq_tag
    } else{
      bColor <- bearings$timestamp
    }
    bearings[, c("dest_lon", "dest_lat") := as.data.table(getHeadingCoords(longitude, latitude, bearings$angle, estimateDist(.SD[, strength], minLength = 100)))]
    bearings[, color := tri_palette()$pal(bColor)]
    a_ply(.data = bearings, .margins = 1, .expand = F, .fun = function(b) {
      leafletProxy("map") %>%
        addPolylines(
          lng = c(
            b$longitude,
            b$dest_lon
          ),
          lat = c(
            b$latitude,
            b$dest_lat
          ),
          color =  b$color,
          group="Bearings",
          weight = 1.5,
          opacity = 0.6,
          label = HTML(
            "Time: ", format(as.POSIXct(b$timestamp, origin = "1970-01-01", tz = "GMT"), "%d.%m. %H:%M:%S", tz = "GMT"), "<br>",
            "Timeslot: ", format(as.POSIXct(b$time_matched, origin = "1970-01-01", tz = "GMT"), "%d.%m. %H:%M:%S", tz = "GMT"),"<br>",
            "Tag: ", b$freq_tag, "<br>",
            "Angle: ", b$angle, "<br>",
            "Strength: ", b$strength, "<br>",
            "bIds: ", b$bId
          )
        )
    })
  }
})

observeEvent(input$map_groups, ignoreInit = T, {
  if ("Bearings" %in% input$map_groups) {
    showElement(id="panel_bearings_time")
  } else{
    hideElement(id="panel_bearings_time")
  }
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
