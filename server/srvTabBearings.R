############ srvTabBearings.R ############

#input fields for manual calibration
correction_list<-reactive({
  if(is.null(filtered_data())){
    return(NULL)
  }
  isolate({
    correction_tag_list <- tagList()
    for(i in unique(filtered_data()$receiver)){
      start_value<-0
      if(!is.null(global$calibration[global$calibration$receiver==i,])){
        start_value<-global$calibration[global$calibration$receiver==i,]$correction
      }
      tmp<-numericInput(paste0("corr_", i),paste0("Correction Factor for ", i),start_value)
      
      correction_tag_list <- tagAppendChildren(correction_tag_list,tmp)
    }
  })
  return(correction_tag_list)
})

# wrapper for calculating bearings via splines
calculate_bearings_spline <- function (filtered_data, receivers, spar_value, live_mode, live_update_interval, progress=F) {
  d<-NULL
  withProgress(min=0, max=length(unique(filtered_data$receiver)), value=0, message="Matching timestamps...", expr={
    d <- smooth_to_time_match(filtered_data,receivers,spar_value, progress)
  })
  b <- doa(d, receivers, live_mode, live_update_interval, progress)
  return(b)
}

# wrapper for calculating bearings via time match
calculate_bearings_time_match <- function(filtered_data, receivers, station_time_error, live_mode, live_update_interval, progress=F) {
  d<-NULL
  withProgress(min=0, max=length(unique(filtered_data$station)), value=0, message="Matching timestamps...", expr={
    d <- time_match_signals(filtered_data, station_time_error, progress)
  })
  b <- doa(d, receivers, live_mode, live_update_interval, progress)
  return(b)
}

observeEvent(input$start_doa,{
  switch(input$time_matching_method,
         spline = {
           global$bearing <- calculate_bearings_spline(filtered_data(),global$receivers,input$spar_in, global$live_mode, global$live_update_interval, T)
         },
         tm = {
           global$bearing <- calculate_bearings_time_match(filtered_data(), global$receivers, input$time_error_inter_station, global$live_mode, global$live_update_interval, T)
         }
         )
})

output$correction_list <- renderUI({
  correction_list()
})

#plot polar plots
output$polar_output <- renderPlot({
  validate(need(doa_smoothed(), "No data found"))
  tmp<-doa_smoothed()[1:4,]
  tmp<-tmp[order(tmp$timestamp),]
  p<-ggplot(doa_smoothed()[1:4,])+
    geom_bar(aes(x=round(angle),y=strength),stat="identity",width=10)+
    coord_polar()+theme_minimal()+
    scale_x_continuous(breaks = c(0,90,180,270),limits = c(0, 359))
  if(input$filter_freq){
    p <- p + facet_wrap(~freq_tag)
  }
  p
})

#save manual calibration factors
observeEvent(input$change_manu,{
    cali<-NULL
    for(i in unique(as.character(filtered_data()$receiver))){
      tmp<-subset(filtered_data(),receiver == i)$Name[1]
      cali<-rbind(cali,data.frame(correction=input[[paste0("corr_",i)]],receiver=i,station=tmp))
    }
    global$calibration<-cali
})

observeEvent(input$calibrate_signal_strength,{
  global$calibration<-calibrate_auto(filtered_data())
  global$calibrated = TRUE
  })

#calculate automatic calibration factors
calibrate_auto <- function(filtered_data) {
  maxis<-NULL

  for(i in unique(as.character(filtered_data$receiver))){
    tmp<-subset(filtered_data,receiver == i)
    maxis<-rbind(maxis,tmp[which.max(tmp$max_signal),])
  }

  max_max<-maxis[which.max(maxis$max_signal),]
  maxis<-data.frame(correction = max_max$max_signal-maxis$max_signal, receiver=maxis$receiver, station = maxis$Name)
  return(maxis)
}

observeEvent(global$calibrated, {
  if (global$calibrated){
    output$calibration_state_warning <- renderUI({"This data has already been calibrated"})
  }
  else {
    output$calibration_state_warning <- renderUI({NULL})
  }
})

output$cal_factors <- renderDataTable({
  validate(need(global$calibration, "No calibration data found"))
  global$calibration
})

# calculate time match and DoA #1
output$doa<- renderDataTable({
  validate(need(global$bearing, "No data found"))
  global$bearing[order(global$bearing$timestamp,decreasing=TRUE),]
})

# output DoA plot
output$doa_plot <- renderPlot({
  validate(need(global$bearing, "No data found - first do calculation"))
  ggplot(global$bearing) + geom_point(mapping=aes(x=timestamp,y=angle,col=Station)) + facet_wrap(~freq_tag)+
    scale_x_datetime(labels = function(x) format(x, "%d-%m \n %H:%M:%S"))
})


