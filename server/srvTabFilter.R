############ srvTabFilter.R ############

# update UIs

observe({
  validate(
    need(pre_filtered_data(), "Please provide logger data.")#,
    # need(nrow(pre_filtered_data())>0, "No data in given date range. Please adjust in Filter tab.")
  )
  if (nrow(pre_filtered_data())>0){
    min_date<-min(pre_filtered_data()$timestamp)-1
    max_date<-max(pre_filtered_data()$timestamp)+1
    old_min <-isolate(input$slider_datetime[1])
    if(isolate(input$app_live_mode)) 
      updateSliderInput(session, "slider_datetime", min=min_date, max=max_date, value = c(old_min,max_date) )
    else
      updateSliderInput(session, "slider_datetime", min=min_date, max=max_date, value = c(min_date,max_date))
  } else {
    updateSliderInput(session, "slider_datetime", min=input$filter_for_dates[1], max=input$filter_for_dates[2]+1, value = c(input$filter_for_dates[1],input$filter_for_dates[2]+1) )
  }
})

observe({
  validate(
    need(global$signals, "Please provide logger data.")
  )
  min_date<-as.Date(min(global$signals$timestamp))
  max_date<-as.Date(max(global$signals$timestamp))
  updateDateRangeInput(session,"filter_for_dates",start=min_date,end=max_date,min=min_date,max=max_date)
})

observeEvent(input$plus_one_day, {
  validate(
    need(global$signals, "Please provide logger data.")
  )
  min_date<-as.Date(min(global$signals$timestamp))
  max_date<-as.Date(max(global$signals$timestamp))
  old_min <-isolate(input$filter_for_dates[1])
  old_max <-isolate(input$filter_for_dates[2])
  updateDateRangeInput(session,"filter_for_dates",start=min(old_min+1, max_date),end=min(old_max+1, max_date),min=min_date,max=max_date)
})

observeEvent(input$minus_one_day, {
  validate(
    need(global$signals, "Please provide logger data.")
  )
  min_date<-as.Date(min(global$signals$timestamp))
  max_date<-as.Date(max(global$signals$timestamp))
  old_min <-isolate(input$filter_for_dates[1])
  old_max <-isolate(input$filter_for_dates[2])
  updateDateRangeInput(session,"filter_for_dates",start=max(old_min-1, min_date),end=max(old_max-1, min_date),min=min_date,max=max_date)
})


observe({
  validate(
    need(global$signals, "Please provide file with antenna specifications.")
  )
  if(!is.null(global$signals)){
    rec_names <- c("all",as.character(unique(global$signals$receiver)))
    station_names <- c("all",as.character(unique(global$signals$Name)))
    updateSelectInput(session, "input_select_receiver", choices = rec_names, selected = "all")
    updateSelectInput(session, "input_select_station", choices = station_names, selected = "all")
  }
})

output$freq_tags <- renderUI({
  selectizeInput("choose_tag", multiple=TRUE, label=strong("Choose tags"), choices = unique(global$frequencies$Name), selected=unique(global$frequencies$Name))
})

pre_filtered_data <-reactive({
  validate(need(input$filter_for_dates[1]<=input$filter_for_dates[2], "Second date must be after first in date range."))
  if (is.null(global$signals))
    return(NULL)
  subset(global$signals, (timestamp>=as.POSIXct(input$filter_for_dates[1]))&(timestamp<=as.POSIXct(input$filter_for_dates[2]+1)))
})

# applying filters
filtered_data <- reactive({
  if (is.null(global$signals))
    return(NULL)
  tempo<-pre_filtered_data()#global$signals
  
  tempo<-subset(tempo, (tempo$timestamp>=input$slider_datetime[1])&(tempo$timestamp<=input$slider_datetime[2]) )

  if(input$filter_length){
    tempo<-filter_data_length(tempo,input$signal_length)
  }
  if(input$filter_type == "Custom frequency"){
    tempo<-filter_data_freq(tempo,input$single_freq,input$freq_error,input$center_freq,paste0(input$single_freq/1000," MHz"))
  }
  if(input$filter_type == "Multiple frequencies"){
    tempo<-filter_data_freq(tempo,global$frequencies$Frequency,input$freq_error,input$center_freq,global$frequencies$Name)
  }
  if(input$filter_strength){
    tempo<-filter_signal_strength(tempo,input$signal_strength)
  }
  if(input$filter_bw){
    tempo<-filter_signal_bandwidth(tempo,input$signal_bw)
  }
  if(input$filter_interval){
    tempo<-filter_data_time_interval(tempo,input$signal_interval)
  }
  if(input$filter_type == "Multiple frequencies"){
    tempo<-subset(tempo,tempo$freq_tag %in% input$choose_tag)
  }
  if(input$input_select_receiver!="all"&& !is.null(input$input_select_receiver) && input$input_select_receiver!=""){
    tempo<-subset(tempo,tempo$receiver %in% input$input_select_receiver)
  }
  if(input$input_select_station!="all"&& !is.null(input$input_select_station) && input$input_select_receiver!=""){
    tempo<-subset(tempo,tempo$Name %in% input$input_select_station)
  }
  if(xor(input$correct_signal_strength_auto,input$correct_signal_strength_manu)){
    if(!is.null(global$calibration)){
      for(i in unique(tempo$receiver)){
        tempo[tempo$receiver==i,]$max_signal<-global$calibration[global$calibration$receiver==i,]$correction+tempo[tempo$receiver==i,]$max_signal
      }
    }
  }
  return(tempo)
}) %>% debounce(millis=300)

plot_data <- reactive({
  validate(need(filtered_data(), "No data loaded"))
  validate(need(nrow(filtered_data())>0, "Oh no, there is no data to plot! Did you filter it all out?"))
  ggplot(filtered_data())
})

output$histo <- renderPlot({
   plot_data() + geom_histogram(aes(signal_freq),bins=200)+ scale_y_log10()
})

output$histo_length <- renderPlot({
  plot_data() + geom_histogram(aes(duration),bins= 100)
})

output$histo_strength <- renderPlot({
  plot_data() + geom_histogram(aes(max_signal),bins= 200)
})

output$histo_bandwidth<- renderPlot({
  plot_data() + geom_histogram(aes(signal_bw),bins= 200)
})

output$total_counts<-renderText({
  if (is.null(global$signals))
    return(NULL)

  return(paste("Number of observations in plot",dim(filtered_data())[1],"of total", dim(global$signals)[1]))
})

output$freq_hover<-renderText({
  switch (input$filter_plotTabs,
    "Frequency" = {
      if(is.null((input$plot_freq_hover))) 
        return(NULL)
      else
        return(paste("Freq:", round(input$plot_freq_hover$x,2), "kHz"))
    },
    "Duration" = {
      if(is.null((input$histo_length_hover))) 
        return(NULL)
      else
        return(paste("Duration:", round(input$histo_length_hover$x*1000,2), "ms"))
    },
    "Signal Strength" = {
      if(is.null((input$histo_strength_hover))) 
        return(NULL)
      else
        return(paste("Max signal strength:", round(input$histo_strength_hover$x,2), "dB"))
    },
    "Signal Bandwidth" = {
      if(is.null((input$histo_bandwidth_hover))) 
        return(NULL)
      else
        return(paste("Signal bandwith:", round(input$histo_bandwidth_hover$x,2), "Hz"))
    }
  )
  
})

