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
      angle <- 1/2*(alpha-alpha*delta_m/(sin(pi*alpha/180)^2))+angle_l
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

time_match_signals <- function(data,station_time_error=0.3, progress=F){
  data$station<-data$Name
  matched_data<-NULL
  cnt_stats=0
    #for each station
    for(i in unique(data$station)){
      if (progress) {
        setProgress(value=cnt_stats)
        incProgress(amount=0, detail = paste0("Station: ",i))
      }
      tmp_s<-subset(data,station==i)
      num_tags=length(unique(tmp_s$freq_tag))
      #for each frequency tag
      for(l in unique(tmp_s$freq_tag)){
        tmp_sf<-subset(tmp_s,freq_tag==l)
        tmp_sf<-tmp_sf[order(tmp_sf$timestamp),]
        #calculate timedifference between the loggings
        tmp_sf$td<-c(0,diff(tmp_sf$timestamp))
        tmp_s$ti <- as.POSIXct(NA, origin = "1970-01-01")
        gc<-0
        tmp_sf$ti[1]<-tmp_sf$timestamp[1]
        for(i in 2:nrow(tmp_sf)){
          if(sum(tmp_sf$td[(i-gc):i])<=station_time_error){
            tmp_sf$ti[i]<- tmp_sf$timestamp[i-gc-1]
            if(any(duplicated(tmp_sf$receiver[(i-gc-1):i]))){
              tmp_sf$ti[i]<- tmp_sf$timestamp[i]
              gc<--1
            }
            gc<-gc+1
          }else{
            tmp_sf$ti[i]<- tmp_sf$timestamp[i]
            gc<-0
          }
        }
        matched_data<-rbind(matched_data,tmp_sf)
      }
      cnt_stats<-cnt_stats+1
    }
  
  matched_data$timestamp<-as.POSIXct(matched_data$ti, origin = "1970-01-01")
  return(matched_data)
}

smooth_to_time_match <-function(data,spar_value=0.01, progress=F){
  smoothed_data<-NULL
  cnt_recs=0
    #for each receiver
    for(i in unique(data$receiver)){
      if (progress) {
        setProgress(value=cnt_recs)
        incProgress(amount=0, detail = paste0("Receiver: ",i))
      }
      tmp_r<-subset(data,receiver==i)
      num_tags=length(unique(tmp_r$freq_tag))
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
        if(progress)
          incProgress(amount=1/num_tags)
      }
      cnt_recs<-cnt_recs+1
    }
  return(smoothed_data)
}

doa <- function(signals, receivers, live_mode=FALSE, live_update_interval=15, progress = F){
  data<-merge(signals,receivers,by.x="receiver",by.y="Name")
  # time_to_look_for<-NULL
  #for each timestamp of the smoothed data
  if(!live_mode){
    time_to_look_for<-unique(data$timestamp)
  }else{
    if(length(unique(data$timestamp))<live_update_interval){
      end_point<-length(unique(data$timestamp))-1
    }else{
      end_point<-live_update_interval-1
    }
    time_to_look_for<-unique(data$timestamp)[order(unique(data$timestamp),decreasing = TRUE)][1:end_point]
  }
  if (progress)
    withProgress(min=0, max=length(time_to_look_for), value=0, expr={ 
      doa_internal(data, time_to_look_for, progress,dBLoss=input$dBLoss,doa_approx=input$doa_option_approximation)
    })
  else
    doa_internal(data, time_to_look_for, doa_approx="automatic", progress=F)
}

doa_internal <- function(data, time_to_look_for, dBLoss=14, doa_approx="automatic", progress=F) {
  tmp_angles<-NULL
  cnt_timestamp=0
  for(t in time_to_look_for){
    if (progress)
      setProgress(value=cnt_timestamp, message = "Computing Bearings... ")
    #build subset for the timestamp
    data_t<-subset(data,timestamp==t)
    num_tags = length(data_t$freq_tag)
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
            angle<-calc_angle(data_tfs[1,"max_signal"],data_tfs[2,"max_signal"],data_tfs[1,"Orientation"],data_tfs[2,"Orientation"],dBLoss,doa_approx)
            tmp_angles<-rbind(cbind.data.frame(timestamp=as.POSIXct(t,origin="1970-01-01",tz="UTC"),angle=angle,antennas=nrow(data_tfs),Station=s,freq_tag=f,strength=max(data_tfs$max_signal),stringsAsFactors=F),tmp_angles)
          }else{
            #back antenna plays a big role here
            if(nrow(data_tfs)>2){
              angle_1<-data_tfs[1,"Orientation"]
              angle_2<-calc_angle(data_tfs[1,"max_signal"],data_tfs[3,"max_signal"],data_tfs[1,"Orientation"],data_tfs[3,"Orientation"],2*dBLoss,"linear")
              angle<-angle_1+angle_between(angle_1,angle_2)/abs(angle_between(data_tfs[1,"Orientation"],data_tfs[2,"Orientation"]))*60
              tmp_angles<-rbind(cbind.data.frame(timestamp=as.POSIXct(t,origin="1970-01-01",tz="UTC"),angle=angle,antennas=nrow(data_tfs),Station=s,freq_tag=f,strength=max(data_tfs$max_signal),stringsAsFactors=F),tmp_angles)
            }
            if(nrow(data_tfs)<=2){
              angle<-data_tfs[1,"Orientation"]
              tmp_angles<-rbind(cbind.data.frame(timestamp=as.POSIXct(t,origin="1970-01-01",tz="UTC"),angle=angle,antennas=nrow(data_tfs),Station=s,freq_tag=f,strength=max(data_tfs$max_signal),stringsAsFactors=F),tmp_angles)
            }
          }
        }
        cnt_timestamp<-cnt_timestamp+1
      }
    }
  }
  return(tmp_angles)
}