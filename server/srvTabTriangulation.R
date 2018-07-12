#expected input is data frame containing DoA data for all frequencies at all stations
# i.e. cols timestamp, station, angle, freq

# placeholder vars
data_in<-reactive({
  validate(need(global$receivers, "No receiver data provided!"))
  validate(need(global$frequencies, "Please provide frequency data."))
  data.frame(timestamp=as.POSIXct(sample(1520601506.61609:1520602506.61609,200,replace = T),tz="GMT",origin="1970-01-01"),station=sample(global$receivers$receiver,200,replace=T),angle=as.numeric(sample(0:359,200,replace=T)),freq=sample(global$frequencies$Name,200,replace=T),stringsAsFactors = F)
})

source(file.path("server", "srvMapFuncs.R"),local=TRUE)$value
source(file.path("server", "srvTriangulation.R"),local=TRUE)$value

tri_filtered_data<-reactive({
  data_in()[data_in()$freq==input$tri_frequency,]
})

tri_position_data<-reactive({
  data<-merge(tri_filtered_data(),global$receivers[,c("receiver","pos_x","pos_y")],by.x="station",by.y="receiver")
  data<-cbind(data,utm=wgstoutm(data[,"pos_x"],data[,"pos_y"]))
})



output$map_triangulation <- renderLeaflet({
    m <- leaflet() %>% addTiles() 
    m <- m %>% addStations(tri_position_data())
    m <- m %>% addBearings(tri_position_data())
    # print("timeslot_data")
    # View(tri_timeslot_data())
    # View(tri_position_data())
})

output$tri_positions_and_angles<-renderDataTable({tri_position_data()[,c("timestamp","station","angle","freq")]},
  options=list(
    #filter="none", autoHideNavigation = T, selection= "multiple", rownames = F, options=list(
    paging=F, searching=F,scrollY="400px"#),
  )
)

observe({
  validate(need(global$frequencies, "Please provide frequency data."))
    updateSelectizeInput(session, "tri_frequency", choices=global$frequencies$Name)
})

output$tri_ui_timeline<-renderUI({
    sliderInput("tri_timeline",NULL,min(data_in()$timestamp),max(data_in()$timestamp),min(data_in()$timestamp),width="100%", animate=animationOptions(interval=100), timeFormat="%F %T", ticks=T,step=1)
})
