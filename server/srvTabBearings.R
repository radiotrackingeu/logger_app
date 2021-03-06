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
    d <- smooth_to_time_match(filtered_data,spar_value, progress)
  })
  b <- doa(d, receivers, dBLoss=input$dBLoss,live_mode, live_update_interval, progress)
  return(b)
}

# wrapper for calculating bearings via time match
calculate_bearings_time_match <- function(filtered_data, receivers, station_time_error, live_mode, live_update_interval, progress=F) {
  d<-NULL
  withProgress(min=0, max=length(unique(filtered_data$station)), value=0, message="Matching timestamps...", expr={
    d <- time_match_signals(filtered_data, station_time_error, progress=F)
  })
  b <- doa(d, receivers, dBLoss=input$dBLoss,live_mode, live_update_interval, progress)
  return(b)
}

# wrapper for calculating berings via time windows
calculate_bearings_time_window <- function(filtered_data, receivers, window_size, live_mode, live_update_interval, progress=F) {
  require(data.table)
  data<-copy(filtered_data)
  setDT(data)
  setorder(data, timestamp)
  start<-min(as.numeric(data$timestamp))
  withProgress(min=0, max=length(unique(filtered_data$station)), value=0, message="Matching timestamps...", expr={
    data[,time_matched:=as.POSIXct(start+floor((as.numeric(timestamp)-start)/window_size)*window_size, origin="1970-01-01", tz="GMT"), by=Name]
  })
  if (input$use_doa_fast){
    bearings<-doa_fast(data, receivers, dBLoss = input$dBLoss)
  } else {
    bearings<-doa(data[,timestamp:=time_matched], receivers, dBLoss = input$dBLoss, live_mode, live_update_interval, progress)
  }
  return(bearings)
}

doa_fast <- function(signals, receivers, dBLoss=14, doa_approx="automatic") {
  require(plyr)
  data<-as.data.table(receivers)[as.data.table(signals), on=c(Name="receiver", Station="Name")]
  data<-data[,.(max_signal=mean(max_signal)), by=.(time_matched,freq_tag, Station, Name, Orientation)]
  result<-ddply(.data = data, .variables = .(time_matched, freq_tag, Station), .fun = calc_doa, dBLoss=dBLoss, doa_approx="automatic", use_back_antenna=input$use_back_antenna, only_one_for_doa=input$only_one_for_doa)
  return(result)
}

observeEvent(input$start_doa,{
  cl <- parallel::makeCluster(detectCores())
  registerDoSNOW(cl)
  switch(input$time_matching_method,
         spline = {
           tmp <- calculate_bearings_spline(filtered_data(),global$receivers,input$spar_in, global$live_mode, global$live_update_interval, T)
         },
         tm = {
           tmp <- calculate_bearings_time_match(filtered_data(), global$receivers, input$intra_station_time_error, global$live_mode, global$live_update_interval, T)
         },
         win = {
           tmp <- calculate_bearings_time_window(filtered_data(), global$receivers, input$bearings_window_size, global$live_mode, global$live_update_interval)
         }
         )
  global$bearing<-subset(tmp, antennas>=input$min_doa_antennas)
  stopCluster(cl)
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
}, rownames=F)

# calculate time match and DoA #1
output$doa<- renderDataTable({
  validate(need(global$bearing, "No data found"))
  global$bearing[order(global$bearing$timestamp,decreasing=TRUE),]
}, rownames=F)

# output DoA plot
output$doa_plot <- renderPlot({
  validate(need(global$bearing, "No data found - first do calculation"))
  ggplot(global$bearing) + geom_point(mapping=aes(x=timestamp,y=angle,col=Station)) + facet_wrap(~freq_tag)+
    scale_x_datetime(labels = function(x) format(x, "%d-%m \n %H:%M:%S"))
})


