# calculates the intersection of bearings of the same frequency and time interval.
# receivers: data frame of all receivers with station name and position
# bearings: data frame produced by doa-function.
# progress: TRUE if function is wrapped in withProgress() call
triangulate <- function(receivers, bearings, only_one=F,time_error_inter_station=0.6,angles_allowed,tri_option,tm_method = "spline",spar=0.01, progress=F) {
  progress=F
  positions<-data.frame()
  #Calc UTM of Stations and add them
  stations<-na.omit(unique(receivers[,c("Station","Longitude","Latitude")]))
  stations<-stations[!duplicated(stations$Station),]
  stations_utm<-cbind(stations,utm=wgstoutm(stations[,"Longitude"],stations[,"Latitude"]))
  
  if(length(unique(stations_utm$utm.zone))>1){
    print("UTM Zone Problem")
  }
  #for each frequency tag
  freq_names=unique(bearings$freq_tag)
  num_freq_names=length(freq_names)
  cnt_freq_names=0
  result<-NULL
  for(i in freq_names){
    tmp_f <- subset(bearings,freq_tag==i)
    tmp_f <- switch(tm_method,
                    tm = timematch_inter(tmp_f,time_error_inter_station),
                    spline = smooth_to_time_match_bearings(tmp_f,receivers,spar))
    timestamps_unique<-unique(tmp_f$timestamp)
    num_timestamps_unique<-length(timestamps_unique)
    #for each times interval
    split<-foreach(j=timestamps_unique,
                   .export=c("tri_one","tri_two","tri_centroid","utmtowgs","coordinates","angle_between","triang"),
                   .packages=c("sp"),
                   .combine=rbind,
                   .inorder=F) %dopar% {
                     if(progress)
                       setProgress(value=cnt_freq_names)
                     tmp_ft <- subset(tmp_f,timestamp==j)
                     tmp_fts <- merge(tmp_ft,stations_utm,by.x="Station",by.y="Station")
                     #calculate positions for two or more bearings in one slot
                     if(nrow(tmp_fts)==1&only_one){
                       positions<-cbind(timestamp=j,freq_tag=i,pos=tri_one(tmp_fts))
                     }
                     if(nrow(tmp_fts)>=2){
                       positions<-cbind(
                         timestamp=j,
                         freq_tag=i,
                         pos=switch(tri_option,
                                    centroid =  tri_centroid(tmp_fts,angles_allowed),
                                    two_strongest = tri_two(tmp_fts,angles_allowed)
                         )
                       )
                     }
                     positions
                   }
    result<-rbind(split,result)
    cnt_freq_names<-cnt_freq_names+1
  }
  if(nrow(result)>0){
    return(result[order(result$timestamp),])
  }
}

#function to match times between two or more station
timematch_inter <- function(data,inter_error=0.6){
  if(nrow(data)==1) return(data)
  data<-data[complete.cases(data), ]
  tmp_s<-data[order(data$timestamp),]
  tmp_s$td <- c(0,diff(tmp_s$timestamp))
  tmp_s$ti <- NA
  gc<-0
  tmp_s$ti[1]<-tmp_s$timestamp[1]
  for(i in 2:nrow(tmp_s)){
    if(sum(tmp_s$td[(i-gc):i])<=inter_error){
      tmp_s$ti[i]<- tmp_s$timestamp[i-gc-1]
      if(any(duplicated(tmp_s$Station[(i-gc-1):i]))){
        tmp_s$ti[i]<- tmp_s$timestamp[i]
        gc<--1
      }
      gc<-gc+1
    }else{
      tmp_s$ti[i]<- tmp_s$timestamp[i]
      gc<-0
    }
  }
  tmp_s$timestamp<-as.POSIXct(tmp_s$ti,origin="1970-01-01")
  tmp_s$ti<-NULL
  tmp_s$td<-NULL
  return(tmp_s)
}

# does the actual triangulation
triang <- function(x1,y1,alpha1,x2,y2,alpha2){
  # For Triangulation GK Coordinates are necesarry!
  # First calculate tan keeping in mind that 0° in geo-coordinates are 90° in a x-y plane
  ta1 <- (alpha1%%360)/180*pi
  ta2 <- (alpha2%%360)/180*pi
  if(((alpha1-alpha2)%%180)==0){#print("No triangulation possible: all three points are on one line")
    return(c(NA,NA))}
  
  # Findinf Intersection Using solver
  b<-c(x2-x1,y2-y1)
  a1<-c(sin(ta1),cos(ta1))
  a2<-c(-sin(ta2),-cos(ta2))
  a<-matrix(c(a1,a2),nrow=2)
  l<-solve(a,b)
  px<-x1+l[1]*sin(ta1)
  py<-y1+l[1]*cos(ta1)
  
  if(l[2]>0&l[1]>0)
  {
    return(c(px,py))
  }
  else{
    return(c(NA,NA))
  }
}

# calculates theoretical bat speed between two triangulations
# vectorized
speed_between_triangulations <- function(timestamp,longitude,latitude){
  tmp<-data.frame(timediff = c(0,as.numeric(diff(timestamp))), 
                  distance = distm(data.frame(longitude,latitude))[,1]
  )
  tmp$speed = tmp$distance/tmp$timediff
  return(tmp)
}

#requires one time and one frequency
# angle 
# utm.X
# utm.Y
tri_centroid <- function(tmp_fts,angles_allowed){
  tmp_positions<-data.frame()
  for(c in 1:dim(combn(nrow(tmp_fts),2))[2]){
    e<-combn(nrow(tmp_fts),2)[1,c]
    z<-combn(nrow(tmp_fts),2)[2,c]
    #order using singal strength and take first two
    #tmp_fts<-tmp_fts[order(tmp_fts$strength,decreasing = TRUE, na.last=NA),]
    if(anyNA(tmp_fts[c(e,z),]))
      next
    if(abs(angle_between(tmp_fts$angle[e],tmp_fts$angle[z]))<angles_allowed[1]|abs(angle_between(tmp_fts$angle[z],tmp_fts$angle[e]))>angles_allowed[2]) 
      next
    location<-triang(tmp_fts$utm.X[e],tmp_fts$utm.Y[e],tmp_fts$angle[e],tmp_fts$utm.X[z],tmp_fts$utm.Y[z],tmp_fts$angle[z])
    if(anyNA(location))
      next
    tmp_positions<-rbind(tmp_positions,cbind(utm.x=location[1],utm.y=location[2],utm.zone=tmp_fts$utm.zone[1]))
  }
  if(nrow(tmp_positions)>0){
    x<-mean(tmp_positions$utm.x,na.rm = T)
    y<-mean(tmp_positions$utm.y,na.rm = T)
    zone<-tmp_positions$utm.zone[1]
    location_wgs<-utmtowgs(x,y,zone)
    return(data.frame(location_wgs,utm.X=x,utm.Y=y))
  }else{
    return(data.frame(X=NA,Y=NA,utm.X=NA,utm.Y=NA))
  }
}
tri_one <- function(tmp_fts){
  str_mod<-50000
  location<-data.frame(utm.X=tmp_fts$utm.X+cospi((90-tmp_fts$angle)/180)/tmp_fts$strength*str_mod, utm.Y=tmp_fts$utm.Y+sinpi((90-tmp_fts$angle)/180)/tmp_fts$strength*str_mod)
  location_wgs<-utmtowgs(location$utm.X,location$utm.Y,tmp_fts$utm.zone)
  return(data.frame(location_wgs,location))
}

tri_two <- function(tmp_fts,angles_allowed){
  tmp_positions<-data.frame()
  #order using singal strength and take first two
  tmp_fts<-tmp_fts[order(tmp_fts$strength,decreasing = TRUE, na.last=NA),]
  if(anyNA(tmp_fts))
    return(data.frame(X=NA,Y=NA,utm.X=NA,utm.Y=NA))
  if(abs(angle_between(tmp_fts$angle[1],tmp_fts$angle[2]))<angles_allowed[1]|abs(angle_between(tmp_fts$angle[2],tmp_fts$angle[1]))>angles_allowed[2]) 
    return(data.frame(X=NA,Y=NA,utm.X=NA,utm.Y=NA))
  location<-triang(tmp_fts$utm.X[1],tmp_fts$utm.Y[1],tmp_fts$angle[1],tmp_fts$utm.X[2],tmp_fts$utm.Y[2],tmp_fts$angle[2])
  if(anyNA(location))
    return(data.frame(X=NA,Y=NA,utm.X=NA,utm.Y=NA))
  location_wgs<-utmtowgs(location[1],location[2],tmp_fts$utm.zone[1])
  if(nrow(location_wgs)>0){
    return(data.frame(location_wgs,utm.X=location[1],utm.Y=location[2]))
  }else{
    return(data.frame(X=NA,Y=NA,utm.X=NA,utm.Y=NA))
  }
}

smooth_to_time_match_bearings <-function(data,receivers,spar_value=0.01, progress=F){
  smoothed_data<-NULL
  cnt_recs=0
  #for each receiver
  for(i in unique(data$Station)){
    if (progress) {
      setProgress(value=cnt_recs)
      incProgress(amount=0, detail = paste0("Station: ",i))
    }
    tmp_r<-subset(data,Station==i)
    num_tags=length(unique(tmp_r$freq_tag))
    #for each frequency tag
    for(l in unique(tmp_r$freq_tag)){
      tmp_rf<-na.omit(subset(tmp_r,freq_tag==l))
      if (nrow(tmp_rf)<5) {
        print(paste0('skipping freq "',l,'" on receiver "',i,'": not enough signals (',nrow(tmp_rf),')'))
        next
      }
      time_seq<-unique(c(round(tmp_rf$timestamp))) #,round(tmp_rf$timestamp)+1,round(tmp_rf$timestamp)-1)
      smoothed<-data.frame(angle=predict(
        smooth.spline(tmp_rf$timestamp,tmp_rf$angle,spar=spar_value),
        as.numeric(time_seq))$y,
        timestamp=time_seq,
        Station=i,
        strength=predict(
          smooth.spline(tmp_rf$timestamp,tmp_rf$strength,spar=spar_value),
          as.numeric(time_seq))$y,
        antennas=0,
        freq_tag=l,stringsAsFactors = F)
      smoothed_data<-rbind(smoothed_data,smoothed)
      if(progress)
        incProgress(amount=1/num_tags)
    }
    cnt_recs<-cnt_recs+1
  }
  return(smoothed_data)
}


centroid_fun <- function(tri_data,time,s_time,method="mean"){
  min_time<-min(tri_data$timestamp)
  max_time<-max(tri_data$timestamp)
  time_seq<-seq(round(min_time,"mins")-60,round(max_time,"mins")+60,by=60*time)
  utm<-foreach(i=time_seq,
               .combine=rbind,
               .inorder=F) %dopar% {
                 tmp<-subset(tri_data,timestamp>=i&timestamp<i+60*s_time)
                 zone<-(floor((tmp$pos.X[1] + 180)/6) %% 60) + 1
                 if(nrow(tmp)>0){
                   data.frame(timestamp=i,
                              freq_tag=tmp$freq_tag[1],
                              pos.utm.X=switch(method,
                                mean=mean(tmp$pos.utm.X),
                                median=median(tmp$pos.utm.X)),
                              pos.utm.Y=switch(method,
                                               mean=mean(tmp$pos.utm.Y),
                                               median=median(tmp$pos.utm.Y)),
                              utm.zone=zone)
                 }
                 
               }
  location_wgs<-utmtowgs(utm$pos.utm.X,utm$pos.utm.Y,utm$utm.zone)
  return(cbind(utm,pos=location_wgs))
}

nigth_day <- function(tri_data){
  min_time<-as.Date(min(tri_data$timestamp))
  max_time<-as.Date(max(tri_data$timestamp))
  time_seq<-seq(min_time,max_time,by="days")
  utm<-foreach(i=time_seq,
               .combine=rbind,
               .inorder=F) %dopar% {
                 sunrise<-getSunlightTimes(i,lat=tri_data$pos.Y[2],lon=tri_data$pos.X[2])$sunrise
                 sunset<-getSunlightTimes(i,lat=tri_data$pos.Y[2],lon=tri_data$pos.X[2])$sunset
                 tmp<-subset(tri_data,timestamp>=sunrise-60*t_error&timestamp<=sunset+60*t_error)
                 tmp$day_night<-"day"
               }
}


centroid_roost <- function(tri_data,time="day",t_error=30,method="mean"){
  min_time<-as.Date(min(tri_data$timestamp))
  max_time<-as.Date(max(tri_data$timestamp))
  time_seq<-seq(min_time,max_time,by="days")
  utm<-foreach(i=time_seq,
               .combine=rbind,
               .inorder=F) %dopar% {
                 sunrise<-getSunlightTimes(i,lat=tri_data$pos.Y[2],lon=tri_data$pos.X[2])$sunrise
                 sunset<-getSunlightTimes(i,lat=tri_data$pos.Y[2],lon=tri_data$pos.X[2])$sunset
                 tmp<-subset(tri_data,timestamp>=sunrise-60*t_error&timestamp<=sunset+60*t_error)
                 zone<-(floor((tmp$pos.X[1] + 180)/6) %% 60) + 1
                 if(nrow(tmp)>0){
                   data.frame(timestamp=i,
                              freq_tag=tmp$freq_tag[1],
                              pos.utm.X=switch(method,
                                               mean=mean(tmp$pos.utm.X),
                                               median=median(tmp$pos.utm.X)),
                              pos.utm.Y=switch(method,
                                               mean=mean(tmp$pos.utm.Y),
                                               median=median(tmp$pos.utm.Y)),
                              utm.zone=zone)
                 }
                 
               }
  location_wgs<-utmtowgs(utm$pos.utm.X,utm$pos.utm.Y,utm$utm.zone)
  return(cbind(utm,pos=location_wgs))
}

get_closest_line_in_history <- function(x, history){
  time_diffs <- abs(difftime(x, history))
  res <- which.min(time_diffs)
  if (length(res) != 1){
    return(res[1])
  }else{
    return(res)
  }
}