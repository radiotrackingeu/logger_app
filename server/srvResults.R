# redraw_results_plot <- function() {
#   global$invalidate_filtered_data <- global$invalidate_filtered_data + 1
# }

#calculate time difference between two consecutive signals
calculate_delta_T <- function(data) {
  data$td<-NULL
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
          ab<-get_temp_coefficients(k)
          tmp2<-cbind(tmp2,td=td,temperature=calculate_temperature(td, ab$a, ab$b))
          return_td<-rbind(tmp2,return_td)
        }
      }
    }
  }
  return(return_td)
}

get_temp_coefficients <-function(freq_tag) {
  l<-switch(input$filter_type,
    "Multiple frequencies" = list(
      "a"=global$frequencies[global$frequencies$Name==freq_tag,]$Temp_A,
      "b"=global$frequencies[global$frequencies$Name==freq_tag,]$Temp_B
    ),
    "Custom frequency" = list(
      "a" = input$single_freq_temp_a,
      "b" = input$single_freq_temp_b
    ),
    "all" = list(
      "a" = 20.307,
      "b" = 0.0408
    )
  )
  return(l)
}

calculate_temperature <- function(td,a=19.449,b=0.0398) {
  return(log(60/as.numeric(td)/a)/b)
}


get_base_plot <- function(signal_data, signal_aes=aes(), style="none") {
  require(ggplot2)
  require(data.table)
  p<-ggplot(signal_data, signal_aes)
  if(
    !is.null(global$keepalives) & 
      nrow(global$keepalives)>0 & 
      as.data.table(global$keepalives)[td_fctr=="down" & timestamp %between% c(min(na.rm=T, signal_data$timestamp), max(na.rm=T, signal_data$timestamp)),.N]>0
    ) {
    if(!is.data.table(global$keepalives))
      setDT(x=global$keepalives)
    switch (style,
      "white" = p<-p+geom_rect(
        data=na.omit(
          cols=c("timestamp", "td","td_fctr"), 
          global$keepalives[td_fctr=="down" & timestamp %between% c(min(na.rm=T, signal_data$timestamp), max(na.rm=T, signal_data$timestamp))]
        ), 
        aes(ymin=-Inf, ymax=Inf, xmin=timestamp-60*td, xmax=timestamp-300, fill="white"), 
        inherit.aes=F
      )+
        scale_fill_identity(name="Status", guide='legend', labels = c("not all receivers running"))+
        guides(fill=guide_legend(title="Status", override.aes = list(color="grey"))),
      "alpha" = p<-p+geom_rect(
        data=na.omit(
          cols=c("timestamp", "td","td_fctr"), 
          global$keepalives[td_fctr=="down" & timestamp %between% c(min(na.rm=T, signal_data$timestamp), max(na.rm=T, signal_data$timestamp))]
        ), 
        aes(ymin=-Inf, ymax=Inf, xmin=timestamp-60*td, xmax=timestamp-300, fill=alpha("white", 2/length(unique(receiver)))), 
        inherit.aes=F
      )+
        scale_fill_identity(name="Status", guide='legend', labels = c("less receivers running -> more transparent"))+
        guides(fill=guide_legend(override.aes = list(color="grey", fill=alpha("white", 0.5))))
      ,
      "color" = p<-p+geom_rect(
        data=na.omit(
          cols=c("timestamp", "td","td_fctr"), 
          global$keepalives[td_fctr=="down" & timestamp %between% c(min(na.rm=T, signal_data$timestamp), max(na.rm=T, signal_data$timestamp))]
        ), 
        aes(ymin=-Inf, ymax=Inf, xmin=timestamp-60*td, xmax=timestamp-300, fill=receiver), 
        alpha=0.05, inherit.aes=F
      )+
        guides(fill=guide_legend(override.aes = list(alpha=0.5)))
    )
  } else {
    message("No keepalive data for base plot.")
  }
  return(p)
}