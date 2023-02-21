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
  #if the the angle is the same
  if(alpha==0) return(NA)
  print(str(dbLoss))
  if(length(dbLoss)==1) { # normal operation
    delta_m<-(sig_l-sig_r)/dbLoss
  } else if (length(dbLoss)==2) {
    if (alpha >= 85) { # normal 90 degree neighbours
      delta_m<-(sig_l-sig_r)/max(dbLoss, na.rm=T)
      print(paste0("dBLoss==2 - alpha >= 85, using dBLoss ", max(dbLoss, na.rm=T),"\n"))
    } else if (alpha<85) { # assume octologger
      delta_m<-(sig_l-sig_r)/min(dbLoss, na.rm=T)
      print(paste0("dBLoss==2 - alpha < 85, using dBLoss ", min(dbLoss, na.rm=T),"\n"))
    }
  } else {
    warning("Error: calc_angle - Illegal dbLoss: ", dbLoss, " Returning NA.")
    return(NA)
  }
  
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
  matched_data<-NULL
  cnt_stats=0
    #for each station
    for(i in unique(data$Name)){
      if (progress) {
        setProgress(value=cnt_stats)
        incProgress(amount=0, detail = paste0("Station: ",i))
      }
      tmp_s<-subset(data,Name==i)
      num_tags=length(unique(tmp_s$freq_tag))
      #for each frequency tag
      for(l in unique(tmp_s$freq_tag)){
        tmp_sf<-subset(tmp_s,freq_tag==l)
        tmp_sf<-tmp_sf[order(tmp_sf$timestamp),]
        #calculate timedifference between the loggings
        tmp_sf$td<-c(0,diff(tmp_sf$timestamp))
        tmp_s$ti <- as.POSIXct(NA, origin = "1970-01-01", tz="UTC")
        gc<-0
        tmp_sf$ti[1]<-tmp_sf$timestamp[1]
        if (nrow(tmp_sf)>=2){
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
        }else{
          message("Error:  not enough points?")
        }
        matched_data<-rbind(matched_data,tmp_sf)
      }
      cnt_stats<-cnt_stats+1
    }
  
  matched_data$timestamp<-as.POSIXct(matched_data$ti, origin = "1970-01-01", tz="UTC")
  return(matched_data)
}

smooth_to_time_match <-function(data,spar_value=0.01, progress=F){
  smoothed_data<-NULL
  cnt_recs=0
    #for each receiver
    for(i in unique(data$receiver)){
      if (progress) {
        setProgress(value=cnt_recs)
        incProgress(amount=0, detail = paste0("Antenna: ",i))
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

doa <- function(signals, receivers,dBLoss=14, live_mode=FALSE, live_update_interval=15, progress = F){
  data<-merge(signals,receivers,by.x="receiver",by.y="Name")
  # time_to_look_for<-NULL
  #for each timestamp of the smoothed data
  progress = F
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
  if(progress)
    withProgress(min=0, max=length(time_to_look_for), value=0, expr={ 
      doa_internal(data, time_to_look_for, progress,dBLoss=dBLoss(),doa_approx=input$doa_option_approximation,use_back_antenna=input$use_back_antenna,only_one_for_doa=input$only_one_for_doa)
    })
  else
    doa_internal(data, time_to_look_for,dBLoss=dBLoss, doa_approx="automatic", progress=F,use_back_antenna=input$use_back_antenna,only_one_for_doa=input$only_one_for_doa)
}

doa_internal <- function(data, time_to_look_for, dBLoss=14, doa_approx="automatic", progress=F,use_back_antenna=FALSE,only_one_for_doa=FALSE) {
  cnt_timestamp=0
  # data$td_flat<-NULL
  split<-foreach(t=time_to_look_for,
          .export=c("angle_between","calc_angle"),
          .combine=rbind,
          .inorder=F) %do% {
    if (progress)
      setProgress(value=cnt_timestamp, message = "Computing Bearings... ")
    #build subset for the timestamp
    data_t<-subset(data,timestamp==t)
    num_tags = length(data_t$freq_tag)
    for(f in unique(data_t$freq_tag)) {
      #build subset for the frequency
      data_tf<-subset(data_t,freq_tag==f)
      output<-NULL
      for(s in unique(data_tf$Station)) {
        result<-NULL
        #build subset for the Station
        data_tfs<-subset(data_tf, Station==s)
        #sort using signal_strength
        data_tfs<-unique(data_tfs[order(data_tfs$max_signal, decreasing = TRUE, na.last=NA),])
        if(nrow(data_tfs)>1){
          if(anyNA(data_tfs[1:2,c("max_signal","Orientation")]))
            next
          #check angle between strongest and second strongest and if it is smaller then 90 degree, calc it linearly
          if(abs(angle_between(data_tfs[1,"Orientation"],data_tfs[2,"Orientation"]))<=120){
            angle<-calc_angle(data_tfs[1,"max_signal"],data_tfs[2,"max_signal"],data_tfs[1,"Orientation"],data_tfs[2,"Orientation"],dBLoss,doa_approx)
            result<-cbind.data.frame(timestamp=as.POSIXct(t,origin="1970-01-01",tz="UTC"),angle=angle,antennas=nrow(data_tfs),Station=s,freq_tag=f,strength=max(data_tfs$max_signal),stringsAsFactors=F)
          }else{
            #back antenna plays a big role here
            if(nrow(data_tfs)>2){
              num_angle_2<-2
              if ((data_tfs[1,"Orientation"]+180)%%360==data_tfs[2,"Orientation"]){
                num_angle_2<-3
              }
              angle<-calc_angle(data_tfs[1,"max_signal"],data_tfs[num_angle_2,"max_signal"],data_tfs[1,"Orientation"],data_tfs[num_angle_2,"Orientation"],1.5*dBLoss,"linear")
              result<-cbind.data.frame(timestamp=as.POSIXct(t,origin="1970-01-01",tz="UTC"),angle=angle,antennas=nrow(data_tfs),Station=s,freq_tag=f,strength=max(data_tfs$max_signal),stringsAsFactors=F)
            }
            if(nrow(data_tfs)==2&use_back_antenna){
              angle<-data_tfs[1,"Orientation"]
              result<-cbind.data.frame(timestamp=as.POSIXct(t,origin="1970-01-01",tz="UTC"),angle=angle,antennas=nrow(data_tfs),Station=s,freq_tag=f,strength=max(data_tfs$max_signal),stringsAsFactors=F)
            }
          }
        }
        if(nrow(data_tfs)==1&only_one_for_doa){
          if(anyNA(data_tfs[1,]))
            next
          angle<-data_tfs[1,"Orientation"]
          result<-cbind.data.frame(timestamp=as.POSIXct(t,origin="1970-01-01",tz="UTC"),angle=angle,antennas=nrow(data_tfs),Station=s,freq_tag=f,strength=max(data_tfs$max_signal),stringsAsFactors=F)
        }
        cnt_timestamp<-cnt_timestamp+1
        output<-rbind(output,result)
      }
    }
    output
  }
  return(split)
}

# adapted version of doa_internal used by fast_doa method
calc_doa <- function(data_tfs, dBLoss, doa_approx,use_back_antenna=FALSE,only_one_for_doa=FALSE) {
  #sort using signal_strength
  data_tfs<-unique(data_tfs[order(data_tfs$max_signal, decreasing = TRUE, na.last=NA),])
  t<-data_tfs$time_matched[[1]]
  s<-data_tfs$Station[[1]]
  f<-data_tfs$freq_tag[[1]]
  if(nrow(data_tfs)>1){
    if(anyNA(data_tfs[1:2,]))
      return(data.frame())
    #check angle between strongest and second strongest and if it is smaller then 120 degree, calc it linearly
    if(abs(angle_between(data_tfs[1,"Orientation"],data_tfs[2,"Orientation"]))<=120){
      angle<-calc_angle(data_tfs[1,"max_signal"],data_tfs[2,"max_signal"],data_tfs[1,"Orientation"],data_tfs[2,"Orientation"],dBLoss,doa_approx)
      return(data.frame(timestamp=as.POSIXct(t,origin="1970-01-01",tz="UTC"),angle=angle,antennas=nrow(data_tfs),Station=s,freq_tag=f,strength=max(data_tfs$max_signal),method="neighbours",recs=paste(data_tfs$Name[[1]], data_tfs$Name[[2]], sep = ","), stringsAsFactors=F))
    }else{
      # ignore back antenna and use third-strongest instead
      if(nrow(data_tfs)>2){
        num_angle_2<-2
        if ((data_tfs[1,"Orientation"]+180)%%360==data_tfs[2,"Orientation"]){
          num_angle_2<-3
        }
        angle<-calc_angle(data_tfs[1,"max_signal"],data_tfs[num_angle_2,"max_signal"],data_tfs[1,"Orientation"],data_tfs[num_angle_2,"Orientation"],dBLoss,"linear")
        return(data.frame(timestamp=as.POSIXct(t,origin="1970-01-01",tz="UTC"),angle=angle,antennas=nrow(data_tfs),Station=s,freq_tag=f,strength=max(data_tfs$max_signal), method="ignore_back", recs=paste(data_tfs$Name[[1]],data_tfs$Name[[2]], data_tfs$Name[[3]], sep = ","),stringsAsFactors=F))
      }
      # use back antenna 
      if(nrow(data_tfs)==2 & use_back_antenna){
        angle<-data_tfs[1,"Orientation"]
        return(data.frame(timestamp=as.POSIXct(t,origin="1970-01-01",tz="UTC"),angle=angle,antennas=nrow(data_tfs),Station=s,freq_tag=f,strength=max(data_tfs$max_signal), method="frontback", recs=paste(data_tfs$Name[[1]], data_tfs$Name[[2]], sep = ","), stringsAsFactors=F))
      }
    }
  }
  if(nrow(data_tfs)==1 & only_one_for_doa){
    if(anyNA(data_tfs[1,]))
      return(data.frame(timestamp=as.POSIXct(character()), angle=numeric(), antennas=numeric(), Station=character(), freq_tag=character(), strength=numeric(), method=character(), recs=character()))
    angle<-data_tfs[1,"Orientation"]
    return(data.frame(timestamp=as.POSIXct(t,origin="1970-01-01",tz="UTC"),angle=angle,antennas=nrow(data_tfs),Station=s,freq_tag=f,strength=max(data_tfs$max_signal), method="onlyone", recs=data_tfs$Name[[1]],stringsAsFactors=F))
  }
  return(data.frame())
}
