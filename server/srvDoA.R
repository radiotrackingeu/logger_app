############ srvDoA.R ############

# calculate time match and DoA #2
doa_data<- reactive({
  if (is.null(filtered_data()))
    return(NULL)
  tmp_angles<-NULL
  #tmp<-subset(filtered_data(), filtered_data()$timestamp>max(filtered_data()$timestamp)-20) # nur die letzten 20 sec??
  tmp<-filtered_data()
  tmp_new<-list_data_time_receiver(tmp)
  tmp_new$angle<-NA
  for(i in 1:nrow(tmp_new)){
    tmp_sort<-data.frame(receiver=names(tmp_new[,3:ncol(tmp_new)]),max_signal=as.numeric(tmp_new[i,3:ncol(tmp_new)]))    
    tmp_sort<-tmp_sort[order(tmp_sort$max_signal, decreasing = TRUE, na.last=NA),]
    if(nrow(tmp_sort)>1){
      tmp_front<-subset(antennae_data(),receiver==tmp_sort$receiver[1])
      
      tmp_angle_front<-tmp_front$orientation[1]
      tmp_angle_left<-tmp_angle_front-90
      if (tmp_angle_left<0)
        tmp_angle_left<-tmp_angle_left+360
      tmp_angle_right<-tmp_angle_front+90
      if (tmp_angle_right>360)
        tmp_angle_right<-tmp_angle_right-360
      tmp_angle_rear<-tmp_angle_front+180
      if(tmp_angle_rear>360)
        tmp_angle_rear<-tmp_angle_rear-360
      
      rx_front<-tmp_front$receiver[1]
      rx_left<-subset(antennae_data(),orientation==tmp_angle_left)$receiver[1]
      rx_right<-subset(antennae_data(),orientation==tmp_angle_right)$receiver[1]
      rx_rear<-subset(antennae_data(),orientation==tmp_angle_rear)$receiver[1]
      
      sig_front<-subset(tmp_sort,receiver==rx_front)$max_signal
      sig_left<-subset(tmp_sort,receiver==rx_left)$max_signal
      if(length(sig_left)==0){sig_left<-0}
      sig_right<-subset(tmp_sort,receiver==rx_right)$max_signal
      if(length(sig_right)==0){sig_right<-0}
      sig_rear<-subset(tmp_sort,receiver==rx_rear)$max_signal
      if(length(sig_rear)==0){
        sig_rear<-0
      }
      
      if(sig_left>sig_right){
        tmp_new$angle[i]<-get_angle_linear(sig_left,sig_front,tmp_angle_left,tmp_angle_front,60,60)
      }
      else{
        tmp_new$angle[i]<-get_angle_linear(sig_front,sig_right,tmp_angle_front,tmp_angle_right,60,60)
      }
      
      "if(sig_left!=0&&sig_right!=0){
        tmp_new$angle[i]<-calc_angle(sig_left,sig_right,tmp_angle_left,tmp_angle_right,60,60)
      }else{
        if(sig_left!=0){
          tmp_new$angle[i]<-calc_angle(sig_left,sig_front,tmp_angle_left,tmp_angle_front,60,60)
        }
        if(sig_right!=0){
          tmp_new$angle[i]<-calc_angle(sig_front,sig_right,tmp_angle_front,tmp_angle_right,60,60)
        }
      }"
    }
  }
  tmp_new
})

angle_linear<-reactive({
  if (is.null(filtered_data()))
    return(NULL)
  tmp_angles<-NULL
  #tmp<-subset(filtered_data(), filtered_data()$timestamp>max(filtered_data()$timestamp)-20)
  tmp<-filtered_data()
  tmp_new<-list_data_time_receiver(tmp)
  tmp_new$angle<-NA
  for(i in 1:nrow(tmp_new)){
    tmp_sort<-data.frame(receiver=names(tmp_new[,3:ncol(tmp_new)]),max_signal=as.numeric(tmp_new[i,3:ncol(tmp_new)]))    
    tmp_sort<-tmp_sort[order(tmp_sort$max_signal, decreasing = TRUE, na.last=NA),]
    if(nrow(tmp_sort)>1){
      tmp_w1<-subset(antennae_data(),receiver==tmp_sort$receiver[1])$orientation[1]
      tmp_w2<-subset(antennae_data(),receiver==tmp_sort$receiver[2])$orientation[1]
      tmp_new$angle[i]<-get_angle_linear(tmp_sort$max_signal[1],tmp_sort$max_signal[2],tmp_w1,tmp_w2,60,60)
    }
  }
  tmp_new
})

#time match between receiver signals
list_data_time_receiver <- function(data){
  #find for each receiver
  list_of_receivers<-unique(data$Receiver)
  #and each frequency
  list_of_frequencies<-unique(data$freq_tag)
  #signals which appear betweeen time_distance[1] and time_distance[2] seconds
  return_tmp<-NULL
  for(k in list_of_frequencies){
    print(k)
    for(t in seq(min(data$Timestamp),max(data$Timestamp),3)){
      t<-as.POSIXct(t, tz = "UTC", origin='1970-01-01')
      timeslot_data <- subset(data,data$Timestamp>=t&data$Timestamp<t+3)
      tmp<-data.frame(time=t, freq=k)
      for(i in list_of_receivers){
        one_receiver <- subset(timeslot_data,Receiver==i)
        sig<-mean(one_receiver$max_signal)
        tmp[[i]]<-sig
      }
      #First a second column never NA - so excluded
      if(!all(is.na(tmp[3:ncol(tmp)]))){
        return_tmp<-rbind(return_tmp,tmp)
      }
    }
  }
  return(return_tmp)
}
