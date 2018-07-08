############ srvPlots.R ############

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
    geom_point(aes(data[[input$select_x]], data[[input$select_y]], color=data[[input$select_col]]), size=I(0.8)) +
    labs(x="Time", y = "Signal Strength") +
    scale_x_datetime(labels = function(x) format(x, "%H:%M:%S")) +
    facet_wrap(~ data[[input$select_facet]])#freq_tag
  if(multifilter){
    p + facet_wrap(~ data[[input$select_facet]])#freq_tag
  }
  else{
    p
  }
}
