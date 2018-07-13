#expected input is data frame containing DoA data for all frequencies at all stations
# i.e. cols timestamp, station, angle, freq

# placeholder vars
data_in<-reactive({
  validate(need(global$receivers, "No receiver data provided!"))
  validate(need(global$frequencies, "Please provide frequency data."))
  sample_size=100
  data.frame(timestamp=sample(1530003973:1530003974,sample_size,replace = T),station=sample(global$receivers$receiver,sample_size,replace=T),angle=as.numeric(sample(0:359,sample_size,replace=T)),freq=sample(global$frequencies$Name,sample_size,replace=T),stringsAsFactors = F)
})

# filter input data by frequency
tri_filtered_data<-reactive({
  tri_position_data()[tri_position_data()$freq==input$tri_frequency,]
})

# add wgs and utm position data and names to freq-filtered input data
tri_position_data<-reactive({
  data<-merge(tri_timeslots(),global$receivers[,c("receiver","pos_x","pos_y")],by.x="station",by.y="receiver")
  data<-cbind(data,utm=wgstoutm(data[,"pos_x"],data[,"pos_y"]))
})

# break data into timeslots, taking mean of angle where there is more than one observation per station
tri_timeslots <- reactive ({
  slot_size=5 #seconds
  ret<-data.frame(stringsAsFactors = F)#timestamp=as.POSIXct(double(),tz="GMT"),freq=character(),station=character(),angle=numeric(), stringsAsFactors = F)
  data<-data_in()
  for (t in seq(min(data$timestamp),max(data$timestamp),slot_size)) {
    data_t<-subset(data, data$timestamp >= t & data$timestamp < t+slot_size)
    for (f in unique(data_t$freq)) {
      data_tf <- subset(data_t, data_t$freq==f)
      for (s in unique(data_tf$station)) {
        data_tfs <- subset(data_tf, data_tf$station==s)
        if (nrow(data_tfs > 0)) {
          line<-list(timestamp=as.POSIXct(t,tz="GMT",origin="1970-01-01"),freq=f,station=s,angle=mean(data_tfs$angle))
          ret<-rbind(ret,line,stringsAsFactors=F, make.row.names=F)
        }
        # else 
        #   print(paste("No data for ",t,f,s))
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
    # m <- m %>% addStations(tri_filtered_data())
    # m <- m %>% addBearings(tri_timeslots_lines_points()$lines, weight=2, color="blue")
    # m <- m %>% addTriangulations(tri_timeslots_lines_points()$points)
    # View(tri_timeslots_lines_points()$points)
    return(m)
})

output$tri_positions_and_angles<-renderDataTable({tri_filtered_data()[,c("timestamp","station","angle","freq")]},
  options=list(
    #filter="none", autoHideNavigation = T, selection= "multiple", rownames = F, options=list(paging=F, 
    pagingType="simple", pageLength=10, lengthChange=F, searching=F,scrollY="400px"#),
  )
)

observe({
  validate(need(global$frequencies, "Please provide frequency data."))
    updateSelectizeInput(session, "tri_frequency", choices=global$frequencies$Name)
})

output$tri_ui_timeline<-renderUI({
    sliderInput("tri_timeline",NULL,min(data_in()$timestamp),max(data_in()$timestamp),min(data_in()$timestamp),width="100%", animate=animationOptions(interval=100), timeFormat="%F %T", ticks=T,step=1)
})
