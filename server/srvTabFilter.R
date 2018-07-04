############ srvTabFilter.R ############

#update ui-elements

output$single_freq_num_input <- renderUI(
  if(input$filter_one_freq){
    numericInput("single_freq", "", value = 150175)
  }
)

#observe({
#  validate(
#    need(signal_data(), "Please provide file with antennae specifications.")
#  )
#  min_date<-min(signal_data()$timestamp)-1
#  max_date<-max(signal_data()$timestamp)+1
#  updateSliderInput(session, "slider_datetime",min=min_date,max=max_date,value = c(min_date,max_date) )
#})

# applying filters
filtered_data <- reactive({
  if (is.null(signal_data()))
    return(NULL)
  tempo<-signal_data()
  
  if(input$filter_length){
    tempo<-filter_data_length(tempo,input$signal_length)
  }
  if(input$filter_one_freq){
    tempo<-filter_data_freq(tempo,input$single_freq,input$freq_error,input$center_freq,paste0(input$single_freq/1000," MHz"))
  }
  if(input$filter_freq){
    tempo<-filter_data_freq(tempo,freqs()[["freq"]],input$freq_error,input$center_freq,freqs()[["label"]])
  }
  if(input$filter_strength){
    tempo<-filter_signal_strength(tempo,input$signal_strength)
  }
  if(input$filter_bw){
    tempo<-filter_signal_bandwidth(tempo,input$Bandwidth)
  }
  if(input$filter_freq&&input$filter_one_freq){
    return(NULL)
  }
  validate(
    need(nrow(tempo)[1]>0, "Oh no, there is no data to plot! Did you filter it all out?")
  )
  return(tempo)
})


output$facet <- renderPlot({
  if(is.null(filtered_data()))
    return(NULL)
  if(input$filter_freq){
    plot_time_signal(filtered_data(),TRUE)
  }
  else{
    plot_time_signal(filtered_data(),input$filter_one_freq)
  }
})

output$histo <- renderPlot({
  if(is.null(filtered_data()))
    return(NULL)
  ggplot(filtered_data()) + geom_histogram(aes(Frequency),bins=200)+ scale_y_log10()
})

output$histo_length <- renderPlot({
  if(is.null(signal_data())){
    return(NULL)
  }
  ggplot(filtered_data()) + geom_histogram(aes(Duration),bins= 100)
})

output$histo_strength <- renderPlot({
  if (is.null(filtered_data()))
    return(NULL)
  ggplot(filtered_data()) + geom_histogram(aes(Strength),bins= 200)
})

output$histo_bandwidth<- renderPlot({
  if (is.null(filtered_data()))
    return(NULL)
  ggplot(filtered_data()) + geom_histogram(aes(Bandwidth),bins= 200)
})

output$total_counts<-renderText({
  if (is.null(signal_data()))
    return(NULL)
  return(paste("Number of observations in plot",dim(filtered_data())[1],"of total", dim(signal_data())[1]))
})



