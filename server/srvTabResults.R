observeEvent(input$slider_datetime, {
    patientReactive("slider_datetime", 1000, redraw_results_plot)
})

global$invalidate_filtered_data <- 0
redraw_results_plot <- function() {
    global$invalidate_filtered_data <- global$invalidate_filtered_data + 1
}

#calculate time difference between two consecutive signals
filtered_data_td <- reactive({
  if (!input$filter_one_freq & !input$filter_freq)
    return(NULL)
  data<-filtered_data()
  #find for each receiver
  list_of_receivers<-unique(data$receiver)
  #and each frequency
  list_of_frequencies<-unique(data$freq_tag)
  #signals which appear betweeen time_distance[1] and time_distance[2] seconds
  return_td<-NULL
  for(i in list_of_receivers){
    tmp1 <- subset(data,receiver==i)
    for(k in list_of_frequencies){
      tmp2<-subset(tmp1,freq_tag==k)
      tmp2<-tmp2[order(tmp2$timestamp),]
      #calculate the time distance between the impulses
      if(nrow(tmp2)>1){
        td<-diff(tmp2$timestamp)
        if(attr(td,"units")=="secs"){
          td<-c(0,td)
          tmp2<-cbind(tmp2,td=td,temperature=calculate_temperature(td))
          return_td<-rbind(tmp2,return_td)
        }
      }
    }
  }
  return(return_td)
})

calculate_temperature <- function(td,a=20.307,b=0.0408) {
  return(log(60/as.numeric(td)/a)/b)
}

output$plot_x_y <- renderText({
  if(is.null(input$plot_hover$x)) return(NULL)
  return(paste0("Time: ", round(as.POSIXct(input$plot_hover$x,origin="1970-01-01", "UTC"))))
})

#Temperature would be great here

output$facet <- renderPlot({
  if(is.null(filtered_data())) return(NULL)
  switch(input$choose_plot,
         'Time-Strength-Receiver-Station'={
           ggplot(filtered_data()) +
           geom_point(aes(x=as.POSIXlt(timestamp, "UTC"), y=max_signal, color=receiver)) +
           labs(x="Time", y = "Signal Strength") +
           scale_x_datetime(labels = function(x) format(x, "%d-%m \n %H:%M:%S"))+
           facet_wrap(~Name)
         },
         'Time-Strength-Receiver-Station-Freq'={
           ggplot(filtered_data()) +
             geom_point(aes(x=as.POSIXlt(timestamp, "UTC"), y=max_signal, color=receiver,group=freq_tag,shape=freq_tag)) +
             labs(x="Time", y = "Signal Strength") +
             scale_x_datetime(labels = function(x) format(x, "%d-%m \n %H:%M:%S"))+
             facet_wrap(~Name)
         },
         'Time-Freq-Strength-Station'={
           ggplot(filtered_data()) +
             geom_point(aes(x=timestamp, y=signal_freq, color=max_signal)) +
             labs(x="Time", y = "Frequency") +
             scale_x_datetime(labels = function(x) format(x, "%d-%m \n %H:%M:%S"))+
             facet_wrap(~Name)
         },
         'Time-Strength-Frequency-Station'={
           ggplot(filtered_data()) +
             geom_point(aes(x=as.POSIXlt(timestamp, "UTC"), y=max_signal, color=freq_tag)) +
             labs(x="Time", y = "Signal Strength") +
             scale_x_datetime(labels = function(x) format(x, "%d-%m \n %H:%M:%S"))+
             facet_wrap(~Name)
         },
         'Time-Temperature-Station-Frequency'={
           ggplot(filtered_data_td())+
             geom_point(aes(x=as.POSIXlt(timestamp, "UTC"), y=temperature,color=freq_tag))+
             ylim(10,45)+
             facet_wrap(~Name)
         }
         )
})

