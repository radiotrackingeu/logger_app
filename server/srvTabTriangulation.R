#expected input is data frame containing DoA data for all frequencies at all stations
# i.e. cols timestamp, station, angle, freq

# placeholder vars
data_in<-reactive({
  validate(need(global$receivers, "No receiver data provided!"))
  validate(need(global$frequencies, "Please provide frequency data."))
  data.frame(timestamp=as.POSIXct(sample(1520601506.61609:1520611506.61609,20,replace = T),tz="GMT",origin="1970-01-01"),station=sample(global$receivers$receiver,20,replace=T),angle=as.numeric(sample(0:359,20,replace=T)),freq=sample(global$frequencies$Name,20,replace=T),stringsAsFactors = F)
})

source(file.path("server", "srvMapFuncs.R"),local=TRUE)$value

tri_filtered_data<-reactive({
  data_in()[data_in()$freq==input$tri_frequency,]
})


output$map_triangulation <- renderLeaflet({
    m <- leaflet() %>% addTiles() 
    m <- m %>% addBearing(tri_filtered_data())
})

output$tri_positions_and_angles<-renderDT({
  datatable(tri_filtered_data(), filter="none", autoHideNavigation = T, selection= "multiple", rownames = F, options=list(paging=F, searching=F,scrollY="400px"),
    colnames = c("time","station","angle","tag")
  )
})

observe({
  validate(need(global$frequencies, "Please provide frequency data."))
    updateSelectizeInput(session, "tri_frequency", choices=global$frequencies$Name)
})

output$tri_ui_timeline<-renderUI({
    sliderInput("tri_timeline",NULL,min(data_in()$timestamp),max(data_in()$timestamp),min(data_in()$timestamp),width="100%", animate=animationOptions(interval=100), timeFormat="%F %T", ticks=T,step=1)
})
