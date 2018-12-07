# calculates the intersection of bearings of the same frequency and time interval.
# receivers: data frame of all receivers with station name and position
# bearings: data frame produced by doa-function.
# progress: TRUE if function is wrapped in withProgress() call
triangulate <- function(receivers, bearings, time_error_inter_station=0.6,angles_allowed,progress) {
  positions<-NULL
  #Calc UTM of Stations and add them
  stations<-unique(receivers[,c("Station","Longitude","Latitude")])
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
    tmp_f <- timematch_inter(tmp_f,time_error_inter_station)
    timestamps_unique<-unique(tmp_f$ti)
    num_timestamps_unique<-length(timestamps_unique)
    print(num_timestamps_unique)
    #for each times interval
    for(j in timestamps_unique){ #no error in timestamp allowed
      if(progress)
        setProgress(value=cnt_freq_names)
      tmp_ft <- subset(tmp_f,ti==j)
      #calculate positions
      if(nrow(tmp_ft)>=2){
        for(c in 1:dim(combn(nrow(tmp_ft),2))[2]){
          e<-combn(nrow(tmp_ft),2)[1,c]
          z<-combn(nrow(tmp_ft),2)[2,c]
          tmp_fts<-merge(tmp_ft,stations_utm,by.x="Station",by.y="Station")
          #order using singal strength and take first two
          tmp_fts<-tmp_fts[order(tmp_fts$strength,decreasing = TRUE, na.last=NA),]
          if(anyNA(tmp_ft))
            next
          if(abs(angle_between(tmp_fts$angle[e],tmp_fts$angle[z]))<angles_allowed[1]|abs(angle_between(tmp_fts$angle[z],tmp_fts$angle[e]))>angles_allowed[2]) 
            next
          location<-triang(tmp_fts$utm.X[e],tmp_fts$utm.Y[e],tmp_fts$angle[e],tmp_fts$utm.X[z],tmp_fts$utm.Y[z],tmp_fts$angle[z])
          
          if(anyNA(location))
            next
          location_wgs<-utmtowgs(location[1],location[2],tmp_fts$utm.zone[1])#zone same as 1
          positions<-rbind(positions,cbind(timestamp=j,freq_tag=i,pos=location_wgs,utm.x=location[1],utm.y=location[2],utm.zone=tmp_fts$utm.zone[1]))
        }
      }
    }
    cnt_freq_names<-cnt_freq_names+1
  }
  tmp<-positions[order(positions$timestamp),]
  return(cbind(tmp,speed_between_triangulations(tmp$timestamp,tmp$pos.X,tmp$pos.Y), stringsAsFactors = F))
}

timematch_inter <- function(data,inter_error=0.6){
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

filter_distance <- function(triangulations, max_speed) {
  return(subset(triangulations, speed<=max_speed))
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
    # print("No triangulation possible: lines don't intersect")
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