### srvTabTriangulations.R


# upon button starts triangulations with a progress Bar
observeEvent(input$calc_triangulations,{
  req(global$bearing)
  withProgress(value=0, min = 0, max = length(unique(global$bearing$freq_tag)), message="Triangulating... ", expr = {
    global$triangulation<-triangulate(global$receivers,
                                      global$bearing,
                                      only_one=F,
                                      time_error_inter_station=input$time_error_inter_station,
                                      angles_allowed=input$slider_angles_allowed,
                                      tri_option=input$tri_option_dd,
                                      tm_method=input$tri_tm_method,
                                      spar=input$spar_in_tri,
                                      progress=T
                                      )
  })
  global$triangulation <- cbind(global$triangulation,speed_between_triangulations(global$triangulation$timestamp,global$triangulation$pos.X,global$triangulation$pos.Y))
})

# filters the data using the distance filter
observeEvent(input$filter_speed,{
  req(global$triangulation)
  global$triangulation <- subset(global$triangulation,speed<=input$tri_speed_slider)
})

output$distance_btw_points <- renderPlot({
  req(global$triangulation)
  ggplot()+geom_point(aes(x=global$triangulation$timestamp,y=global$triangulation$speed))
})

output$triangulation_points <- renderDataTable({
  req(global$triangulation)
  return(global$triangulation)
})

output$tri_distance <- renderPlot({
  req(global$triangulation)
  distances<-distm(data.frame(global$triangulation$pos.X,global$triangulation$pos.Y))[,1]
  ggplot()+geom_histogram(aes(x=distances))
})

output$one_distance <- renderPlot({
  req(global$triangulation)
  global$triangulation$speed<-speed_between_triangulations(global$triangulation$timestamp,global$triangulation$pos.X,global$triangulation$pos.Y)
  distances<-distm(data.frame(global$triangulation$pos.X,global$triangulation$pos.Y),data.frame(pos.X=input$compare_single_x,pos.Y=input$compare_single_y))[,1]
  ggplot()+geom_histogram(aes(x=distances))
})

output$single_distance <- renderText({
  req(global$triangulation)
  distances<-distm(data.frame(global$triangulation$pos.X,global$triangulation$pos.Y),data.frame(pos.X=input$compare_single_x,pos.Y=input$compare_single_y))[,1]
  x<-mean(global$triangulation$pos.utm.X,na.rm=T)
  y<-mean(global$triangulation$pos.utm.Y,na.rm=T)
  location_wgs<-utmtowgs(x,y,32)
  distances_centroid<-distm(data.frame(pos=location_wgs),data.frame(pos.X=input$compare_single_x,pos.Y=input$compare_single_y))[,1]
  return(paste("Arithmetic Mean:", mean(distances,na.rm=T),"Median:", median(distances,na.rm=T),"Distance to centroid:",distances_centroid))
})