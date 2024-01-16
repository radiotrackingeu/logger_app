############ srvTabBearingsComp.R ############

source("server/read_DJI.R")


drone_files<-list.files(path="data/drone_logs", pattern="*.csv", full.names=T)
names(drone_files) <- substr(basename(drone_files), 1, nchar(basename(drone_files))-4)

drone_gps <- ldply(.data = drone_files, .id="file", .fun=function(f) {
  readDJI(fpath=f, tz = "Etc/GMT+5" )
})
setDT(drone_gps)
observeEvent(global$signals, ignoreNULL = T, {
    station_names <- sort(as.character(unique(global$signals$Name)))
    updateSelectInput(session, "input_bearings_comp_stat_select", choices = station_names)
})

stat_name <- reactive({
  input$input_bearings_comp_stat_select
}) %>% debounce(750)

stat_bearings <- reactive({
  req(global$bearing)
  req(nrow(global$bearing)>0)
  print("get tag bearings")
  stat_bearings<-as.data.table(global$bearing)[Station == stat_name()]
  setnames(stat_bearings, names(stat_bearings), paste0("tag.", names(stat_bearings)))
  stat_bearings[, ts_key:=tag.timestamp]
  setkey(stat_bearings, ts_key)
  return(stat_bearings)
})

stat_pos <- reactive({
  req(global$receivers)
  req(stat_name())
  print("get station pos")
  stat_pos <- unique(as.data.table(global$receivers)[Station==stat_name(),.(Longitude, Latitude)])
  if (!nrow(stat_pos)==1)
    stop("No or more than one position for ", stat_name())
  return(stat_pos)
})

drone_bearings <- reactive({
  req(stat_pos())
  req(drone_gps)
  print("calc drone bearings")
  drone_bearings <- cbind(
    drone_gps[,.(timestamp, latitude, longitude, altitude, file)], 
    "angle" = bearing(
      p1=stat_pos(), 
      p2=drone_gps[,.(longitude, latitude)]
    )%%360
  )
  setnames(drone_bearings, names(drone_bearings), paste0("drone.", names(drone_bearings)))
  drone_bearings[, ts_key:=drone.timestamp]
  setkey(drone_bearings, ts_key)
})

filtered_drone_bearings <- reactive({
  req(stat_bearings())
  req(drone_bearings())
  print("filter drone bearings")
  setkey(drone_bearings(), ts_key)
  drone_bearings()[drone.timestamp %between% c(min(stat_bearings()$tag.timestamp, na.rm=T), max(stat_bearings()$tag.timestamp, na.rm=T))]
})

matched_bearings <- reactive({
  req(stat_bearings())
  req(filtered_drone_bearings())
  print("match bearings")
  matched_bearings <- filtered_drone_bearings()[stat_bearings(), roll="nearest", nomatch=NULL]
  matched_bearings[, time_diff:=difftime(tag.timestamp, drone.timestamp, units="secs")][, angle_diff:=angle_between(tag.angle, drone.angle)]
})

output$bearings_comp_map <- renderLeaflet({
  print("render map")
  leaflet() %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellit", options = providerTileOptions(maxZoom = 20)) %>%
    addProviderTiles( "OpenTopoMap", group = "OpenTopoMap", options = providerTileOptions(maxZoom = 20)) %>%
    addProviderTiles("OpenStreetMap.Mapnik", group ="OSM", options = providerTileOptions(maxZoom = 20)) %>%
    addLayersControl(
      baseGroups = c("OSM", "Satellite", "OpenTopoMap")
    ) %>%
    addScaleBar(position = "bottomright")
})
outputOptions(output, "bearings_comp_map", suspendWhenHidden=FALSE)

observeEvent(global$receivers, ignoreNULL = T, ignoreInit = T, {
  req(leafletProxy("bearings_comp_map"))
  print("add stations to map")
  stations <- unique(as.data.table(global$receivers), by=c("Station", "Latitude", "Longitude")) 
  leafletProxy("bearings_comp_map") %>% 
    clearGroup("stations") %>%
    clearGroup("antennae_cones") %>%
    addMarkers(
      data=stations,
      lng = ~Longitude, 
      lat = ~Latitude, 
      label = lapply(
        paste0("<b>",stations$Station, "</b>
          <br>", sprintf("%.5f N",as.numeric(stations$Latitude)), "  ", sprintf("%09.5f E", as.numeric(stations$Longitude))
        ),
        HTML
      ),
      icon = makeIcon(iconUrl = "icon_mast.png", iconWidth = 25, iconHeight = 25, iconAnchorX = 25*0.49, iconAnchorY = 25*0.95),
      group = "stations",
      layerId = ~Station
    ) %>% 
    addAntennaeCones(antennae_cones()) %>%
    fitBounds(
      min(stations$Longitude),
      min(stations$Latitude),
      max(stations$Longitude),
      max(stations$Latitude),
      options=list(padding=c(50,50))
    )
})

observeEvent(filtered_drone_bearings(), {
  pal<-colorNumeric(palette = rainbow(nrow(filtered_drone_bearings())), domain = as.numeric(filtered_drone_bearings()$drone.timestamp))
  labFormat <- function(type, x) {
    format(as.POSIXct(x, origin = "1970-01-01", tz = "GMT"), "%d.%m.%y %H:%M", tz = "GMT")
  }
  leafletProxy("bearings_comp_map") %>%
    clearGroup("drone.gps") %>%
    removeShape("legend") %>%
    addCircleMarkers(
      data = filtered_drone_bearings(), 
      lng = ~drone.longitude, 
      lat= ~drone.latitude, 
      label=~drone.file,
      radius = 6, 
      fill=T, 
      fillColor = pal(filtered_drone_bearings()$drone.timestamp), 
      color="black", 
      fillOpacity=1, 
      opacity=1, 
      weight=0.5,
      group = "drone.gps"
    ) %>% 
    addLegend(title="Time (UTC)", pal=pal, values = as.numeric(filtered_drone_bearings()$drone.timestamp), labFormat = labFormat, layerId = "legend")
})

output$bearings_comp_plot <- renderPlot({
  dta<-matched_bearings()[abs(time_diff)<10]
  validate(need(nrow(dta)>0, "No data to show."))
  pal <- colorRampPalette(c("Red","Blue"))
  switch (input$input_bearings_comp_plot_select,
    "Bearings & Diff" = {
      cowplot::plot_grid(ncol=1, align = "v", rel_heights = c(4,1), axis="tblr",
        ggplot(dta)+
          geom_point(aes(x=tag.timestamp, y=tag.angle), color="blue", size=2)+
          geom_point(aes(x=drone.timestamp, y=drone.angle), color="forestgreen", size=2)+
          geom_point(data=~subset(., tag.angle >= 315), aes(x=tag.timestamp, y=tag.angle-360), color="blue", alpha=0.3, fill=NA)+
          geom_point(data=~subset(., tag.angle <= 45 ), aes(x=tag.timestamp, y=tag.angle+360), color="blue", alpha=0.3, fill=NA)+
          geom_point(data=~subset(., drone.angle >= 315), aes(x=drone.timestamp, y=drone.angle-360), color="forestgreen", alpha=0.3, fill=NA)+
          geom_point(data=~subset(., drone.angle <= 45 ), aes(x=drone.timestamp, y=drone.angle+360), color="forestgreen", alpha=0.3, fill=NA)+
          scale_x_datetime(date_labels =  "%F %T")+
          ylab("Bearing [°]")+
          xlab("Timestamp")+
          ggtitle(paste0("Bearings ", stat_name()), subtitle = paste0("Tag (blue) and Drone (green) bearings from ", stat_name(), " as well as the difference (red).\nSmaller, transparent points are replicas of exiting points shifted by +/-360 degrees to better show groups reaching over 0/360 threshold.")),
        ggplot(dta)+
          geom_point(aes(x=tag.timestamp, y=angle_diff), color="red", size=2)+
          ylab("Bearing diff. (abs) [°]")+
          xlab("Timestamp")
      )
    },
    # "Plot2" = {
    #   ggplot(matched_bearings())+
    #     geom_point(aes(x=drone.timestamp, y=angle_diff, color=as.numeric(time_diff)))+
    #     scale_color_gradient2("Tag time - Drone time [s]", low="red", mid="forestgreen", high="red")+
    #     geom_point(data=~subset(., is.na(tag.angle)), aes(x=drone.timestamp, y=0), color="#ff00ff", size=2)+
    #     scale_x_datetime(date_labels =  "%F %T")+
    #     # theme_dark()+
    #     ylab("Tag Bearing - Drone Bearing [°]")+
    #     xlab("Timestamp")+
    #     ggtitle(paste0("Bearing Difference ", stat_name()), subtitle = "Missing Tag Bearings marked in pink at angle 0.")
    # },
    "Tag vs Drone Bearing" = {
      ggplot(dta)+
        geom_point(aes(x=drone.angle, y=tag.angle, color=abs(as.numeric(time_diff))), size=2)+
        geom_point(data=~subset(., tag.angle >= 315), aes(x=drone.angle, y=tag.angle-360, color=abs(as.numeric(time_diff))), alpha=0.3, fill=NA)+
        geom_point(data=~subset(., tag.angle <= 45 ), aes(x=drone.angle, y=tag.angle+360, color=abs(as.numeric(time_diff))), alpha=0.3, fill=NA)+
        scale_color_gradient("Difference between Tag and Drone time [s]", low = "blue", high = "red")+
        geom_abline(slope = 1, intercept = c(-360,0,360), color="#ff00ff")+
        # scale_x_datetime(date_labels =  "%F %T")+
        # theme_dark()+
        ylab("Tag Bearing [°]")+
        xlab("Drone Bearing [°]")+
        lims(x=c(-45,415),y=c(-45,415))+
        ggtitle(paste0("Bearing Ratio ", stat_name()), subtitle = "Purple lines are 1:1.\nSmaller, transparent points are replicas of exiting points shifted by +/-360 degrees to better show groups reaching over 0/360 threshold.")
    },
  )
})

