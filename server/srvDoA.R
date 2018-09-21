############ srvDoA.R ############

# linear approximation
#pre-condition: a<b and angle not bigger then 180
calc_angle_linear <- function(sig_a, sig_b, angle_a, angle_b,dbLoss){
  slope_a<--dbLoss/(angle_b-angle_a)
  slope_b<- dbLoss/(angle_b-angle_a)
  inter_a<-slope_a*angle_a*(-1)
  inter_b<-angle_b*slope_b*(-1)
  dif_angle=(sig_a-sig_b-inter_a+inter_b)/(slope_a-slope_b)
  if(dif_angle<0){
    dif_angle<-dif_angle+360
  }
  if(dif_angle>360){
    dif_angle<-dif_angle-360
  }
  return(dif_angle)
}

#condition angles above 0
angle_between <- function(angle_a,angle_b){
    ((((angle_b - angle_a) %% 360) + 540) %% 360) - 180
}

#calculates linear approx of doa between two antennas
get_angle_linear <- function(sig_a, sig_b, angle_a, angle_b, dbLoss){
  #first the one with the smaller angle
  #take the smaller angle between the two antennas
  if(angle_a<angle_b){
    if(angle_b-angle_a>180){
      result<-calc_angle_linear(sig_b, sig_a, angle_b, angle_a+360, dbLoss)#+180
    }else{
      result<-calc_angle_linear(sig_a, sig_b, angle_a, angle_b, dbLoss)
    }
  }else
  {
    if(angle_b-angle_a<(-180)){
      result<-calc_angle_linear(sig_a, sig_b, angle_a, angle_b+360, dbLoss)#+180
    }else{
      result<-calc_angle_linear(sig_b, sig_a, angle_b, angle_a, dbLoss)
    }
  }
  return(result)
}

doa_smoothed<-reactive({
  if(is.null(smoothed_curves())) return(NULL)
  if(is.null(global$receivers)) return(NULL)
  data<-merge(smoothed_curves(),global$receivers,by.x="receiver",by.y="Name")
  tmp_angles<-NULL
  #for each timestamp of the smoothed data
  if(!global$live_mode){
    time_to_look_for<-unique(data$timestamp)
  }else{
    if(length(unique(data$timestamp))<global$live_update_interval){
      end_point<-length(unique(data$timestamp))-1
    }else{
      end_point<-global$live_update_interval-1
    }
    time_to_look_for<-unique(data$timestamp)[order(unique(data$timestamp),decreasing = TRUE)][1:end_point]
  }
  
  for(t in time_to_look_for){
    #build subset for the timestamp
    data_t<-subset(data,timestamp==t)
    for (f in unique(data_t$freq_tag)) {
      #build subset for the frequency
      data_tf<-subset(data_t,freq_tag==f)
      for (s in unique(data_tf$Station)) {
        #build subset for the Station
        data_tfs<-subset(data_tf, Station==s)
        #sort using signal_strength
        data_tfs<-unique(data_tfs[order(data_tfs$max_signal, decreasing = TRUE, na.last=NA),])
        if(nrow(data_tfs)>1){
          #check angle between strongest and second strongest and if it is smaller then 90 degree, calc it linearly
          if(abs(angle_between(data_tfs[1,"Orientation"],data_tfs[2,"Orientation"]))<=90){
              angle<-get_angle_linear(data_tfs[1,"max_signal"],data_tfs[2,"max_signal"],data_tfs[1,"Orientation"],data_tfs[2,"Orientation"],input$dBLoss)
          }else{
            #back antenna plays a big role here
            if(nrow(data_tfs)>2){
              angle_1<-data_tfs[1,"Orientation"]
              angle_2<-get_angle_linear(data_tfs[1,"max_signal"],data_tfs[3,"max_signal"],data_tfs[1,"Orientation"],data_tfs[3,"Orientation"],input$dBLoss)
              angle<-angle_1+angle_between(angle_1,angle_2)/abs(angle_between(data_tfs[1,"Orientation"],data_tfs[2,"Orientation"]))*30
            }
            if(nrow(data_tfs)<=2){
              angle<-data_tfs[1,"Orientation"]
            }
          }
          tmp_angles<-rbind(cbind.data.frame(timestamp=as.POSIXct(t,origin="1970-01-01 00:00:00",tz="UTC"),angle=angle,antennas=nrow(data_tfs),Station=s,freq_tag=f,strength=max(data_tfs$max_signal),stringsAsFactors=F),tmp_angles)
        }
      }
    }
  }
  tmp_angles
})


#smoothing in second intervals
smoothed_curves <- reactive({
  if(is.null(filtered_data())) return(NULL)
  data<-filtered_data()
  smoothed_data<-NULL
  for(i in unique(data$receiver)){
    tmp1<-subset(data,receiver==i)
    for(l in unique(tmp1$freq_tag)){
      tmp2<-subset(tmp1,freq_tag==l)
      if (nrow(tmp2)<5) {
        print(paste0('skipping freq "',l,'" on receiver "',i,'": not enough signals (',nrow(tmp2),')'))
        next
      }
      time_seq<-unique(c(round(tmp2$timestamp))) #,round(tmp2$timestamp)+1,round(tmp2$timestamp)-1)
      smoothed<-data.frame(max_signal=predict(
        smooth.spline(tmp2$timestamp,tmp2$max_signal,spar=input$spar_in),
        as.numeric(time_seq))$y,
        timestamp=time_seq,
        receiver=i,
        freq_tag=l,stringsAsFactors = F)
      smoothed_data<-rbind(smoothed_data,smoothed)
    }
  }
  return(smoothed_data)
})

#smoothing in second intervals
smoothed_curves_2 <- reactive({
  if(is.null(filtered_data())) return(NULL)
  data<-filtered_data()
  data$station<-data$Name
  signal_repeat<-1 # one second a signal
  signal_time_error<-0.1 # error on one station 100ms
  smoothed_data<-NULL
  for(i in unique(data$station)){
    tmp1<-subset(data,station==i)
    for(l in unique(tmp1$freq_tag)){
      tmp2<-subset(tmp1,freq_tag==l)
      tmp2<-tmp2[order(tmp2$timestamp),]
      tmp2$td<-c(NA,diff(tmp2$timestamp))
      pre<-FALSE
      for(k in 2:nrow(tmp2)){
        if(tmp2$td[k]<signal_time_error&&pre==FALSE){
          tmp2$timestamp[k]<-tmp2$timestamp[k-1]
          time<-tmp2$timestamp[k-1]
          pre<-TRUE
          next
        }
        if(tmp2$td[k]<signal_time_error&&pre==TRUE){
          tmp2$timestamp[k]<-time
        }
        if(tmp2$td[k]>=signal_time_error){
          pre<-FALSE
        }
      }
      smoothed_data<-rbind(smoothed_data,tmp2)
    }
  }
  return(smoothed_data)
})