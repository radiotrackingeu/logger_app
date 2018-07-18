#expected input is data frame containing DoA data for all frequencies at all stations
# i.e. cols timestamp, station, angle, freq_tag

# placeholder vars
data_in<-reactive({
  validate(need(global$receivers, "No receiver data provided!"))
  validate(need(input$filter_freq || input$filter_one_freq, "Please enable either Single oder Multiple Frequency filter!"))
  validate(need(doa_smoothed(), "Could not calculate DoAs"))
  # sample_size=500
  # data.frame(timestamp=sample(1530003973:1530004174,sample_size,replace = T),station=sample(isolate(global$receivers$receiver),sample_size,replace=T),angle=as.numeric(sample(0:359,sample_size,replace=T)),freq_tag=sample(isolate(global$frequencies$Name),sample_size,replace=T),stringsAsFactors = F)
  return(doa_smoothed())
})

# filter input data by frequency
tri_filtered_data<-reactive({
  tri_position_data()[tri_position_data()$freq_tag==input$tri_frequency,]
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
  slot_size=5 #seconds
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



output$map_triangulation <- renderLeaflet({
    m <- leaflet() %>% addTiles()
    m <- m %>% addStations(tri_filtered_data(), color="black", radius=10, group="Stations")
    m <- m %>% addBearings(tri_timeslots_lines_points()$lines, weight=1, color="red", group="Bearings")
    m <- m %>% addTriangulations(tri_timeslots_lines_points()$points, error=input$tri_error, color="blue", radius=7, group="Triangulations")
    m <- m %>% addLayersControl(overlayGroups = c("Stations","Bearings","Triangulations","Tri Bearing","Tri Error"), options = layersControlOptions(sortLayers=FALSE))
    return(m)
})

output$tri_positions_and_angles<-renderDataTable({tri_filtered_data()[,c("timestamp","station","angle","freq_tag")]},
  options=list(
    #filter="none", autoHideNavigation = T, selection= "multiple", rownames = F, options=list(paging=F,
    pagingType="simple", pageLength=10, lengthChange=F, searching=F,scrollY="400px"#),
  )
)

observe({
  if (input$navbar!="Triangulation")
    return(NULL)
  validate(need(tri_timeslots(), "Please provide data."))
  updateSelectizeInput(session, "tri_frequency", choices=unique(tri_timeslots()$freq_tag))
})

output$tri_ui_timeline<-renderUI({
    sliderInput("tri_timeline",NULL,min(data_in()$timestamp),max(data_in()$timestamp),min(data_in()$timestamp),width="100%", animate=animationOptions(interval=100), timeFormat="%F %T", ticks=T,step=1)
})