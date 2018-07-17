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

#display temperatur using A and B Coeficients
output$timediffs_plot <- renderPlot({
  validate(need(filtered_data_td(),"No data! One of the frequency filters must be enabled!"))
  tmp<-filtered_data_td()
  ggplot(data=filtered_data_td())+geom_point(aes(x=timestamp,y=temperature,color=freq_tag))+ylim(10,45)
})

#Temperature would be great here

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

plot_time_signal <- function(data, multifilter){
  p<-ggplot(data) +
    geom_point(aes(x=timestamp, y=max_signal, color=receiver)) +
    labs(x="Time", y = "Signal Strength") +
    scale_x_datetime(labels = function(x) format(x, "%d-%m \n %H:%M:%S"))+
    facet_wrap(~Name)
  if(multifilter){
    p + facet_wrap(~Name)
  }
  else{
    p
  }
}
