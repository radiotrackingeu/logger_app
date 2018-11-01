#expected input is data frame containing DoA data for all frequencies at all stations
# i.e. cols timestamp, station, angle, freq_tag

#render speed plot

#map
# creates basic map
tri_map <- reactive({
  leaflet() %>% addProviderTiles(providers[[input$map_choose]]) %>% addMeasure(position = "bottomleft", 
    primaryLengthUnit = "meters",  
    primaryAreaUnit = "sqmeters",
    activeColor = "blue",
    completedColor = "red") %>% addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>% addScaleBar(position="bottomright")
})

# data input
data_in<-reactive({
  validate(need(global$receivers, "No receiver data provided!"))
  validate(need(input$navbar=="Triangulation", ""))
  validate(need(doa_smoothed(), "Could not calculate DoAs"))
  # sample_size=500
  # data.frame(timestamp=sample(1530003973:1530004174,sample_size,replace = T),station=sample(isolate(global$receivers$receiver),sample_size,replace=T),angle=as.numeric(sample(0:359,sample_size,replace=T)),freq_tag=sample(isolate(global$frequencies$Name),sample_size,replace=T),stringsAsFactors = F)
  return(doa_smoothed())
})

# add wgs and utm position data and names to freq-filtered input data
tri_position_data<-reactive({
  data<-merge(tri_timeslots(),global$receivers[!duplicated(global$receivers$Station),c("Station","Longitude","Latitude")],by.x="station",by.y="Station")
  data<-cbind(data,utm=wgstoutm(data[,"Longitude"],data[,"Latitude"]))
  colnames(data)[which(colnames(data)=='Longitude')]<-"pos_x"
  colnames(data)[which(colnames(data)=='Latitude')]<-"pos_y"
  data
})

# break data into timeslots, taking mean of angle where there is more than one observation per station
tri_timeslots <- reactive ({
  slot_size=1 #seconds
  ret<-data.frame(stringsAsFactors = F)#timestamp=as.POSIXct(double(),tz="GMT"),freq_tag=character(),station=character(),angle=numeric(), stringsAsFactors = F)
  data<-data_in()
  for (t in seq(min(data$timestamp),max(data$timestamp),slot_size)) {
    data_t<-subset(data, data$timestamp >= t & data$timestamp < t+slot_size)
    for (f in unique(data_t$freq_tag)) {
      data_tf <- subset(data_t, data_t$freq_tag==f)
      for (s in unique(data_tf$Station)) {
        data_tfs <- subset(data_tf, data_tf$Station==s)
        if (nrow(data_tfs > 0)) {
          line<-list(timestamp=as.POSIXct(t,tz="GMT",origin="1970-01-01 00:00:00"),freq_tag=f,station=s,angle=mean(data_tfs$angle),strength=mean(data_tfs$strength))
          ret<-rbind(ret,line,stringsAsFactors=F, make.row.names=F)
        }
      }
    }
  }
  return(ret)
})

# break timeslots into two data.frames, $lines contains slots with just one observation per timeslot, $points those with 2 or more
tri_timeslots_lines_points <- reactive({
  data<-tri_filtered_data()
  multiple<-unique(data[duplicated(data$timestamp),])
  points<-data[data$timestamp %in% multiple$timestamp,]
  lines<-data[!data$timestamp %in% multiple$timestamp,]
  return(list(lines=lines,points=points))
})



output$tri_map <- renderLeaflet({
    m <- tri_map()
    m <- m %>% addStations(global$receivers, color="black", radius=10, group="Stations")
    m <- m %>% addLayersControl(overlayGroups = c("Stations","Bearings","Triangulations","Tri Bearing","Tri Error"), options = layersControlOptions(sortLayers=FALSE))
    return(m)
})

observe({
  leafletProxy("tri_map") %>% 
    clearGroup("Bearings") %>% 
    clearGroup("Triangulations") %>% 
    clearGroup("Tri Error") %>% 
    clearGroup("Tri Bearing") %>% 
    clearPopups() %>% 
    clearMarkers()
  leafletProxy("tri_map") %>% addBearings(tri_timeslots_lines_points()$lines, weight=1, color="red", group="Bearings")
  leafletProxy("tri_map") %>% addTriangulations(tri_timeslots_lines_points()$points, error=input$tri_error, radius=7, group="Triangulations")
})

observe({
  if(input$navbar!="Triangulation")
    return(NULL)
  validate(need(tri_timeslots(), "Please provide data."))
  updateSelectizeInput(session, "tri_frequency", choices=unique(tri_timeslots()$freq_tag))
})

output$tri_ui_timeline<-renderUI({
    sliderInput("tri_timeline",NULL,min(data_in()$timestamp),max(data_in()$timestamp),c(min(data_in()$timestamp),data_in()$timestamp+300),width="100%", animate=T, timeFormat="%F %T", ticks=T,step=input$tri_timestep)
})