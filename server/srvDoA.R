############ srvDoA.R ############

# linear approximation
#pre-condition: a<b and angle not bigger then 180
calc_angle_linear <- function(sig_a, sig_b, angle_a, angle_b, oe_winkel_a, oe_winkel_b){
  #half_gain_dBm<-3
  slope_a<--input$dBLoss/input$angle_sep #(-2)*half_gain_dBm/oe_winkel_a
  slope_b<- input$dBLoss/input$angle_sep#2*half_gain_dBm/oe_winkel_b
  inter_a<-slope_a*angle_a*(-1)
  inter_b<-angle_b*slope_b*(-1)
  dif_angle=(sig_a-sig_b-inter_a+inter_b)/(slope_a-slope_b)
  if(dif_angle<0){
    dif_angle<-dif_angle+360
  }
  return(dif_angle)
}

#calculates linear approx of doa between two antennas
get_angle_linear <- function(sig_a, sig_b, angle_a, angle_b, oe_winkel_a, oe_winkel_b){
  #first the one with the smaller angle
  #take the smaller angle between the two antennas
  if(angle_a<angle_b){
    if(angle_b-angle_a>180){
      result<-calc_angle_linear(sig_b, sig_a, angle_b, angle_a, oe_winkel_b, oe_winkel_a)+180
    }else{
      result<-calc_angle_linear(sig_a, sig_b, angle_a, angle_b, oe_winkel_a, oe_winkel_b)
    }
  }else
  {
    if(angle_b-angle_a<(-180)){
      result<-calc_angle_linear(sig_a, sig_b, angle_a, angle_b, oe_winkel_a, oe_winkel_b)+180
    }else{
      result<-calc_angle_linear(sig_b, sig_a, angle_b, angle_a, oe_winkel_b, oe_winkel_a)
    }
  }
  return(result)
}

# calculate time match and DoA #2
# !!! new implementation 05072018 !!!
doa_data<- reactive({
  if (is.null(filtered_data()))
    return(NULL)
  tmp_angles<-NULL
  ##debug
  lin_range_threshold <- 11
  #tmp<-subset(filtered_data(), filtered_data()$timestamp>max(filtered_data()$timestamp)-20) # nur die letzten 20 sec??
  tmp<-smoothed_curves() #replace here?
  tmp_new<-list_data_time_receiver(tmp)
  tmp_new$angle<-NA
  #for each line of data
  for(i in 1:nrow(tmp_new)){
    # sort by signal strength
    tmp_sort<-data.frame(receiver=names(tmp_new[,3:ncol(tmp_new)]),max_signal=as.numeric(tmp_new[i,3:ncol(tmp_new)]))
    tmp_sort<-tmp_sort[order(tmp_sort$max_signal, decreasing = TRUE, na.last=NA),]
    # signal received by more than 1 antenna?
    if(nrow(tmp_sort)>1){
      # delta between the 2 strongest signals
      delta_siglev_1_2 <- tmp_sort$max_signal[1] - tmp_sort$max_signal[2]
      if (delta_siglev_1_2 > lin_range_threshold){
        # use 3 antenna signals for approximation
        
        # find left/right and rear antenna
        tmp_front<-subset(global$receivers,Name==tmp_sort$receiver[1])
        tmp_angle_front<-tmp_front$Orientation[1]
        tmp_angle_left<-tmp_angle_front-90
        if (tmp_angle_left<0)
          tmp_angle_left<-tmp_angle_left+360
        tmp_angle_right<-tmp_angle_front+90
        if (tmp_angle_right>360)
          tmp_angle_right<-tmp_angle_right-360
        tmp_angle_rear<-tmp_angle_front+180
        if(tmp_angle_rear>360)
          tmp_angle_rear<-tmp_angle_rear-360
        
        # get data for left, right, rear antenna
        rx_front<-tmp_front$receiver[1]
        rx_left<-subset(global$receivers,Orientation==tmp_angle_left)$Name[1]
        rx_right<-subset(global$receivers,Orientation==tmp_angle_right)$Name[1]
        rx_rear<-subset(global$receivers,Orientation==tmp_angle_rear)$Name[1]
        
        sig_front<-subset(tmp_sort,receiver==rx_front)$max_signal
        sig_left<-subset(tmp_sort,receiver==rx_left)$max_signal
        if(length(sig_left)==0){sig_left<-0}
        sig_right<-subset(tmp_sort,receiver==rx_right)$max_signal
        if(length(sig_right)==0){sig_right<-0}
        sig_rear<-subset(tmp_sort,receiver==rx_rear)$max_signal
        if(length(sig_rear)==0){
          sig_rear<-0
        }
        
        # delta front ant. to other
        delta_siglev_1_left <- sig_front - sig_left
        delta_siglev_1_right <- sig_front - sig_right
        delta_siglev_1_rear <- sig_front <- sig_rear
        
      }else{
        # linear approximation
        tmp_w1<-subset(global$receivers,Name==tmp_sort$receiver[1])$Orientation[1]
        tmp_w2<-subset(global$receivers,Name==tmp_sort$receiver[2])$Orientation[1]
        tmp_new$angle[i]<-get_angle_linear(tmp_sort$max_signal[1],tmp_sort$max_signal[2],tmp_w1,tmp_w2,60,60)
      }
    }
  }
  tmp_new
})

angle_linear<-reactive({
  if (is.null(filtered_data()))
    return(NULL)
  tmp_angles<-NULL
  #tmp<-subset(filtered_data(), filtered_data()$timestamp>max(filtered_data()$timestamp)-20)
  tmp<-smoothed_curves()
  tmp_new<-list_data_time_receiver(tmp)
  tmp_new$angle<-NA
  for(i in 1:nrow(tmp_new)){
    tmp_sort<-data.frame(receiver=names(tmp_new[,3:ncol(tmp_new)]),max_signal=as.numeric(tmp_new[i,3:ncol(tmp_new)]))    
    tmp_sort<-tmp_sort[order(tmp_sort$max_signal, decreasing = TRUE, na.last=NA),]
    if(nrow(tmp_sort)>1){
      tmp_w1<-subset(receiver_list(),Name==tmp_sort$receiver[1])$Orientation[1]
      tmp_w2<-subset(receiver_list(),Name==tmp_sort$receiver[2])$Orientation[1]
      tmp_new$angle[i]<-get_angle_linear(tmp_sort$max_signal[1],tmp_sort$max_signal[2],tmp_w1,tmp_w2,60,60)
    }
  }
  tmp_new
})

#time match between receiver signals
list_data_time_receiver <- function(data){
  #find for each receiver
  list_of_receivers<-unique(data$receiver)
  #and each frequency
  list_of_frequencies<-unique(data$freq_tag)
  #signals which appear betweeen time_distance[1] and time_distance[2] seconds
  return_tmp<-NULL
  for(k in list_of_frequencies){
    print(k)
    for(t in seq(min(data$timestamp),max(data$timestamp),3)){
      t<-as.POSIXct(t, tz = "UTC", origin='1970-01-01')
      timeslot_data <- subset(data,data$timestamp>=t&data$timestamp<t+3)
      tmp<-data.frame(time=t, freq=k)
      for(i in list_of_receivers){
        one_receiver <- subset(timeslot_data,receiver==i)
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
