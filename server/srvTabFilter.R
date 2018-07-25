############ srvTabFilter.R ############

# update UIs

observe({
  validate(
   need(global$signals, "Please provide file with antennae specifications.")
  )
  if(!is.null(global$signals)){
    old_min <- isolate(input$slider_datetime[1])
    min_date<-min(global$signals$timestamp)-1
    max_date<-max(global$signals$timestamp)+1
    if (isolate(input$app_live_mode)) 
      updateSliderInput(session, "slider_datetime",min=min_date,max=max_date,value = c(old_min,max_date) )
    else
      updateSliderInput(session, "slider_datetime",min=min_date,max=max_date,value = c(min_date,max_date) )
  }
})

observe({
  validate(
    need(global$signals, "Please provide file with antennae specifications.")
  )
  if(!is.null(global$signals)){
    rec_names <- c("all",as.character(unique(global$signals$receiver)))
    station_names <- c("all",as.character(unique(global$signals$Name)))
    updateSelectInput(session, "input_select_receiver", label = "Select Receiver", choices = rec_names, selected = "all")
    updateSelectInput(session, "input_select_station", label = "Select Station", choices = station_names, selected = "all")
  }
})

observeEvent(input$filter_freq,
             {
               validate(
                 need(filtered_data(), "Please provide frequency information in data tab.")
               )
               updateSelectInput(session, "choose_tag", label = "Select Tag", choices = c("all",unique(filtered_data()$freq_tag)))
})

# applying filters
filtered_data <- reactive({
  if (is.null(global$signals))
    return(NULL)
  tempo<-global$signals
  
  tempo<-subset(tempo, (tempo$timestamp>=input$slider_datetime[1])&(tempo$timestamp<=input$slider_datetime[2]) )

  if(input$filter_length){
    tempo<-filter_data_length(tempo,input$signal_length)
  }
  if(input$filter_one_freq){
    tempo<-filter_data_freq(tempo,input$single_freq,input$freq_error,input$center_freq,paste0(input$single_freq/1000," MHz"))
  }
  if(input$filter_freq){
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
  if(input$filter_freq&&input$filter_one_freq){
    return(NULL)
  }
  if(input$choose_tag!="all"&& !is.null(input$choose_tag) && input$choose_tag!=""){
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
      for(i in unique(global$calibration$receiver)){
        tempo[tempo$receiver==i,]$max_signal<-global$calibration[global$calibration$receiver==i,]$correction+tempo[tempo$receiver==i,]$max_signal
      }
    }
  }
  validate(
    need(nrow(tempo)[1]>0, "Oh no, there is no data to plot! Did you filter it all out?")
  )
  return(tempo)
})

output$histo <- renderPlot({
  if(is.null(filtered_data()))
    return(NULL)
  ggplot(filtered_data()) + geom_histogram(aes(signal_freq),bins=200)+ scale_y_log10()
})

output$histo_length <- renderPlot({
  if(is.null(global$signals)){
    return(NULL)
  }

  ggplot(filtered_data()) + geom_histogram(aes(duration),bins= 100)
})

output$histo_strength <- renderPlot({
  if (is.null(filtered_data()))
    return(NULL)

  ggplot(filtered_data()) + geom_histogram(aes(max_signal),bins= 200)
})

output$histo_bandwidth<- renderPlot({
  if (is.null(filtered_data()))
    return(NULL)

  ggplot(filtered_data()) + geom_histogram(aes(signal_bw),bins= 200)
})

output$total_counts<-renderText({
  if (is.null(global$signals))
    return(NULL)

  return(paste("Number of observations in plot",dim(filtered_data())[1],"of total", dim(global$signals)[1]))
})

output$freq_hover<-renderText({
  req(input$plot_freq_hover)
  return(paste("kHz:",round(input$plot_freq_hover$x)))
})

