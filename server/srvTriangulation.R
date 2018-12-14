# calculates the intersection of bearings of the same frequency and time interval.
# receivers: data frame of all receivers with station name and position
# bearings: data frame produced by doa-function.
# progress: TRUE if function is wrapped in withProgress() call
triangulate <- function(receivers, bearings, time_error_inter_station=0.6,angles_allowed,tri_option,tm_method = "spline",spar=0.01, progress) {
  positions<-data.frame()
  #Calc UTM of Stations and add them
  stations<-na.omit(unique(receivers[,c("Station","Longitude","Latitude")]))
  stations_utm<-cbind(stations,utm=wgstoutm(stations[,"Longitude"],stations[,"Latitude"]))
  
  if(length(unique(stations_utm$utm.zone))>1){
    print("UTM Zone Problem")
  }
  #for each frequency tag
  freq_names=unique(bearings$freq_tag)
  num_freq_names=length(freq_names)
  cnt_freq_names=0
  for(i in freq_names){
    tmp_f <- subset(bearings,freq_tag==i)
    tmp_f <- switch(tm_method,
      tm = timematch_inter(tmp_f,time_error_inter_station),
      spline = smooth_to_time_match_bearings(tmp_f,receivers,spar))
    timestamps_unique<-unique(tmp_f$timestamp)
    num_timestamps_unique<-length(timestamps_unique)
    #for each times interval
    for(j in timestamps_unique){
      if(progress)
        setProgress(value=cnt_freq_names)
      tmp_ft <- subset(tmp_f,timestamp==j)
      tmp_fts <- merge(tmp_ft,stations_utm,by.x="Station",by.y="Station")
      #calculate positions for two or more bearings in one slot
      if(nrow(tmp_fts)>=2){
        positions<-rbind(positions,
                         cbind(
                           timestamp=j,
                           freq_tag=i,
                           pos=switch(tri_option,
                                    centroid =  tri_centroid(tmp_fts,angles_allowed),
                                    two_strongest = tri_two(tmp_fts,angles_allowed)
                                    )
                           ))
      }
    }
    cnt_freq_names<-cnt_freq_names+1
  }
  if(nrow(positions)>0){
    return(positions[order(positions$timestamp),])
  }
}

#function to match times between two or more station
timematch_inter <- function(data,inter_error=0.6){
  data[complete.cases(data), ]
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
    x<-mean(tmp_positions$utm.x)
    y<-mean(tmp_positions$utm.y)
    zone<-tmp_positions$utm.zone[1]
    location_wgs<-utmtowgs(x,y,zone)
    return(data.frame(location_wgs,utm.X=x,utm.Y=y))
  }else{
    return(data.frame(X=NA,Y=NA,utm.X=NA,utm.Y=NA))
  }
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
        freq_tag=l,stringsAsFactors = F)
      smoothed_data<-rbind(smoothed_data,smoothed)
      if(progress)
        incProgress(amount=1/num_tags)
    }
    cnt_recs<-cnt_recs+1
  }
  return(smoothed_data)
}
