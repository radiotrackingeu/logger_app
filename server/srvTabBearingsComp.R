############ srvTabBearingsComp.R ############

source("server/read_DJI.R")


drone_files<-list.files(path="data/drone_logs", pattern="*.csv", full.names=T)
names(drone_files) <- substr(basename(drone_files), 1, nchar(basename(drone_files))-4)

drone_gps <- ldply(.data = drone_files, .id="file", .fun=function(f) {
  readDJI(fpath=f)
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
  matched_bearings <- stat_bearings()[filtered_drone_bearings(), roll = "nearest"]
  matched_bearings[, time_diff:=difftime(tag.timestamp, drone.timestamp)][, angle_diff:=tag.angle-drone.angle)%%360, ]
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
  switch (input$input_bearings_comp_plot_select,
    "Plot1" = {
      ggplot(matched_bearings())+
        geom_point(aes(x=tag.timestamp, y=tag.angle), color="blue")+
        geom_point(aes(x=drone.timestamp, y=drone.angle), color="green")+
        geom_point(aes(x=drone.timestamp, y=angle_diff), color="red")+
        scale_x_datetime(date_labels =  "%F %T")+
        ylab("Bearing [°]")+
        xlab("Timestamp")+
        ggtitle(paste0("Bearings ", stat_name()), subtitle = paste0("Tag (blue) and Drone (green) bearings from ", stat_name(), " as well as the difference (red)."))
    },
    "Plot2" = {
      ggplot(matched_bearings())+
        geom_point(aes(x=drone.timestamp, y=angle_diff, color=as.numeric(time_diff)))+
        scale_color_distiller("Tag time - Drone time [s]", palette = "RdYlGn")+
        geom_point(data=~subset(., is.na(tag.angle)), aes(x=drone.timestamp, y=0), color="#ff00ff", size=1)+
        scale_x_datetime(date_labels =  "%F %T")+
        theme_dark()+
        ylab("Tag Bearing - Drone Bearing [°]")+
        xlab("Timestamp")+
        ggtitle(paste0("Bearing Difference ", stat_name()), subtitle = "Missing Tag Bearings marked in pink at angle 0.")
    },
  )
})

