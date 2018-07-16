############ srvTabBearings.R ############

#plot polar plots
output$polar_output <- renderPlot({
  test<-data.frame(degree=c(70,40,20,10,20,35),max_signal=c(16,16,16,44,40,30))
  ggplot(test)+geom_bar(aes(x=degree,y=max_signal),stat="identity")+
    coord_polar()+theme_minimal()+
    scale_x_continuous(breaks = c(0,90,180,270),limits = c(0, 359))
})

#calculate calibration factors
observeEvent(input$calibrate_signal_strength,{
  maxis<-NULL
  for(i in unique(as.character(filtered_data()$receiver))){
    tmp<-subset(filtered_data(),receiver == i)
    str(tmp)
    maxis<-rbind(maxis,tmp[which.max(tmp$max_signal),])
  }
  max_max<-maxis[which.max(maxis$max_signal),]
  maxis<-data.frame(correction = max_max$max_signal-maxis$max_signal, receiver=maxis$receiver, station = maxis$Name)
  global$calibration<-maxis
})

output$cal_factors <- renderDataTable({
  if (is.null(global$calibration))
    return(NULL)
  global$calibration
})

# calculate time match and DoA #1
output$doa<- renderDataTable({
  if(is.null(angle_linear()))
    return(NULL)
  angle_linear()[order(angle_linear()$time,decreasing=TRUE),]
})

# output DoA plot
output$doa_plot <- renderPlot({
  if(is.null(angle_linear()))
    return(NULL)
  ggplot() + geom_point(mapping=aes(x=angle_linear()$time,y=angle_linear()$angle,ymin=0,ymax=359),col="blue")
  #+ geom_point(mapping=aes(x=angle_linear()$time,y=angle_linear()$angle,ymin=0,ymax=359,colour=angle_linear()$freq)) #
})

# calculate time match and DoA #1
output$doa2<- renderDataTable({
  if(is.null(doa_data()))
    return(NULL)
  doa_data()[order(doa_data()$time,decreasing=TRUE),]
})

# output DoA plot
output$doa_plot2 <- renderPlot({
  if(is.null(doa_data()))
    return(NULL)
  ggplot() + geom_point(mapping=aes(x=doa_data()$time,y=doa_data()$angle,ymin=0,ymax=359),col="blue")
})

# calculate time match and DoA #1
output$doa3<- renderDataTable({
  if(is.null(doa_smoothed()))
    return(NULL)
  doa_smoothed()[order(doa_smoothed()$timestamp,decreasing=TRUE),]
})

# output DoA plot
output$doa_plot3 <- renderPlot({
  if(is.null(doa_smoothed()))
    return(NULL)
  ggplot() + geom_point(mapping=aes(x=doa_smoothed()$timestamp,y=doa_smoothed()$angle,ymin=0,ymax=359),col="blue")
})

output$smoothed_curves <- renderPlot({
  if(is.null(smoothed_curves()))
    return(NULL)
  ggplot()+geom_point(data=smoothed_curves(),aes(x=smoothed_curves()$timestamp,y=smoothed_curves()$max_signal,col=smoothed_curves()$receiver))
})

