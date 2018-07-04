############ srvPlots.R ############

plot_time_signal <- function(data, multifilter){
  p<-ggplot(data) +
    geom_point(aes(Timestamp, Strength, color=Station), size=I(0.8)) +
    labs(x="Time", y = "Signal Strength") +
    scale_x_datetime(labels = function(x) format(x, "%H:%M:%S"))
  if(multifilter){
    p + facet_wrap(~ data$freq_tag)
  }
  else{
    p
  }
}
