############ srvDoA.R ############

#angle in between two angles
angle_between <- function(angle_a,angle_b){
  ((((angle_b - angle_a) %% 360) + 540) %% 360) - 180
}

#caclulates the angle between two antennas with given singal strengths
calc_angle <- function(sig_a, sig_b, angle_a, angle_b, dbLoss, option){
  #options: linear, arcos, lookup, automatic, old_linear
  #lookup still to be implemented
  if(option=="automatic"){
    if(abs(angle_between(angle_a,angle_b))==90){
      option<-"arccos"
    }else{
      option<-"linear"
    }
  }
  alpha<-angle_between(angle_a,angle_b)
  #if the left antenna is the left one
  if(alpha>0){
    sig_l<-sig_a
    sig_r<-sig_b
    angle_l<-angle_a
    angle_r<-angle_b
  }
  #if the left antenna is the right one
  if(alpha<0){
    sig_l<-sig_b
    sig_r<-sig_a
    angle_l<-angle_b
    angle_r<-angle_a
    alpha<-angle_between(angle_l,angle_r)
  }
  #if the the angle ist the same
  if(alpha==0) return(NA)
  delta_m<-(sig_l-sig_r)/dbLoss
  if(abs(delta_m)>1){
    return(NA)
  }
  switch(option,
         linear = {
           angle <- 1/2*(alpha-alpha*delta_m/(sin(pi*a/180)^2))+angle_l
         },
         arccos = {
           angle <- acos(delta_m)*90/pi+angle_l
         }
  )
  if(angle<0){
    angle<-angle+360
  }
  if(angle>360){
    angle<-angle-360
  }
  return(angle)
}

time_match_signals <- function(data,station_time_error=0.1){
  data$station<-data$Name
  matched_data<-NULL
  #for each station
  for(i in unique(data$station)){
    tmp_s<-subset(data,station==i)
    #for each frequency tag
    for(l in unique(tmp_s$freq_tag)){
      tmp_sf<-subset(tmp_s,freq_tag==l)
      tmp_sf<-tmp_sf[order(tmp_sf$timestamp),]
      #calculate timedifference between the loggings
      tmp_sf$td<-c(NA,diff(tmp_sf$timestamp))
      pre<-FALSE
      for(k in 2:nrow(tmp_sf)){
        if(tmp_sf$td[k]<station_time_error&&pre==FALSE){
          tmp_sf$timestamp[k]<-tmp_sf$timestamp[k-1]
          time<-tmp_sf$timestamp[k-1]
          pre<-TRUE
          next
        }
        if(tmp_sf$td[k]<station_time_error&&pre==TRUE){
          tmp_sf$timestamp[k]<-time
        }
        if(tmp_sf$td[k]>=station_time_error){
          pre<-FALSE
        }
      }
      matched_data<-rbind(matched_data,tmp_sf)
    }
  }
  return(matched_data)
}

smooth_to_time_match <-function(data,receivers,spar_value=0.01){
  smoothed_data<-NULL
  #for each receiver
  for(i in unique(data$receiver)){
    tmp_r<-subset(data,receiver==i)
    #for each frequency tag
    for(l in unique(tmp_r$freq_tag)){
      tmp_rf<-subset(tmp_r,freq_tag==l)
      if (nrow(tmp_rf)<5) {
        print(paste0('skipping freq "',l,'" on receiver "',i,'": not enough signals (',nrow(tmp_rf),')'))
        next
      }
      time_seq<-unique(c(round(tmp_rf$timestamp))) #,round(tmp_rf$timestamp)+1,round(tmp_rf$timestamp)-1)
      smoothed<-data.frame(max_signal=predict(
        smooth.spline(tmp_rf$timestamp,tmp_rf$max_signal,spar=spar_value),
        as.numeric(time_seq))$y,
        timestamp=time_seq,
        receiver=i,
        freq_tag=l,stringsAsFactors = F)
      smoothed_data<-rbind(smoothed_data,smoothed)
    }
  }
  return(smoothed_data)
}

doa <- function(signals,receivers){
  data<-merge(signals,receivers,by.x="receiver",by.y="Name")
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
    for(f in unique(data_t$freq_tag)) {
      #build subset for the frequency
      data_tf<-subset(data_t,freq_tag==f)
      for(s in unique(data_tf$Station)) {
        #build subset for the Station
        data_tfs<-subset(data_tf, Station==s)
        #sort using signal_strength
        data_tfs<-unique(data_tfs[order(data_tfs$max_signal, decreasing = TRUE, na.last=NA),])
        if(anyNA(data_tfs[1:2,]))
          next
        if(nrow(data_tfs)>1){
          #check angle between strongest and second strongest and if it is smaller then 90 degree, calc it linearly
          if(abs(angle_between(data_tfs[1,"Orientation"],data_tfs[2,"Orientation"]))<=90){
            angle<-calc_angle(data_tfs[1,"max_signal"],data_tfs[2,"max_signal"],data_tfs[1,"Orientation"],data_tfs[2,"Orientation"],input$dBLoss,input$doa_option_approximation)
          }else{
            #back antenna plays a big role here
            if(nrow(data_tfs)>2){
              angle_1<-data_tfs[1,"Orientation"]
              angle_2<-calc_angle(data_tfs[1,"max_signal"],data_tfs[3,"max_signal"],data_tfs[1,"Orientation"],data_tfs[3,"Orientation"],input$dBLoss,input$doa_option_approximation)
              angle<-angle_1+angle_between(angle_1,angle_2)/abs(angle_between(data_tfs[1,"Orientation"],data_tfs[2,"Orientation"]))*30
            }
            #if(nrow(data_tfs)<=2){
            #  angle<-data_tfs[1,"Orientation"]
            #}
          }
          tmp_angles<-rbind(cbind.data.frame(timestamp=as.POSIXct(t,origin="1970-01-01 00:00:00",tz="UTC"),angle=angle,antennas=nrow(data_tfs),Station=s,freq_tag=f,strength=max(data_tfs$max_signal),stringsAsFactors=F),tmp_angles)
        }
      }
    }
  }
  return(tmp_angles)
}

#smoothing in second intervals
smoothed_curves <- eventReactive(input$start_doa,{
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