### srvTabTriangulations.R


# upon button starts triangulations with a progress Bar
observeEvent(input$calc_triangulations,{
  req(global$bearing)
  withProgress(value=0, min = 0, max = length(unique(global$frequencies$Name)), message="Triangulating... ", expr = {
    global$triangulation<-triangulate(global$receivers,
                                      global$bearing,
                                      time_error_inter_station=input$time_error_inter_station,
                                      angles_allowed=input$slider_angles_allowed,
                                      progress=T)
  })
})

observeEvent(input$form_centroids,{
  req(global$triangulation)
  u_time<-unique(global$triangulation$timestamp)
  new_triang<-data.frame()
  for(i in u_time){
    tmp_tri<-subset(global$triangulation,timestamp==i)
    x<-mean(tmp_tri$utm.x)
    y<-mean(tmp_tri$utm.y)
    zone<-tmp_tri$utm.zone[1]
    location_wgs<-utmtowgs(x,y,zone)
    new_triang<-rbind(new_triang,cbind(timestamp=i,
                                       freq_tag=tmp_tri$freq_tag[1],
                                       pos=location_wgs))
  }
  global$triangulation<-new_triang
})



# filters the data using the distance filter
observeEvent(input$filter_distance,{
  req(global$triangulation)
  global$triangulation <- filter_distance(global$triangulation,input$tri_speed_slider)
})

output$tri_speed <- renderPlot({
  req(global$triangulation)
  ggplot(global$triangulation) + geom_point(aes(x=timestamp,y=speed,col=freq_tag))
})

output$tri_distance <- renderPlot({
  req(global$triangulation)
  ggplot(global$triangulation) + geom_point(aes(x=timestamp,y=distance,col=freq_tag))
})