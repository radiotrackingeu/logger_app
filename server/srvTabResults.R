############ srvPlots.R ############

#calculate time difference between two consecutive signals
calc_time_distance <- function(data){
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
      #calcualte the time distance between the impulses
      if(nrow(tmp2)>1){
        td<-diff(tmp2$timestamp)
        if(attr(td,"units")=="secs"){
          td<-c(0,td)
          #tmp2<-cbind(tmp2,td=td)
          return_td<-rbind(td,return_td)
        }
      }
    }
  }
  return(return_td)
}

#display temperatur using A and B Coeficients
output$timediffs_plot <- renderPlot({
  tmp<-calc_time_distance(filtered_data())
  #y=20,307*exp(0,0408*x)<=>ln(y)=ln(20,307)+0,0408*x<=>ln(y/20,307)/0,0408=x 
  a<-20.307#input$temp_cal_a
  b<-0.0408#input$temp_cal_b
  tmp$temperature<-log(60/as.numeric(tmp$td)/a)/b
  ggplot()+geom_point(aes(x=tmp$timestamp,y=tmp$temperature))+ylim(10,45)
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
