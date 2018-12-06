observeEvent(input$calc_triangulations,{
  req(global$bearing)
  withProgress(value=0, min = 0, max = length(unique(global$frequencies$Name)), message="Triangulating... ", expr = {
    global$triangulation<-triangulate(global$receivers,
                                      global$bearing,
                                      time_error_inter_station=input$time_error_inter_station,
                                      angles_allowed=input$slider_angles_allowed,
                                      progress=T)
  })
})

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
      timestamps_unique<-unique(tmp_f$timestamp)
      num_timestamps_unique<-length(timestamps_unique)
      #for each times interval
      for(j in timestamps_unique){ #no error in timestamp allowed
        if(progress)
          setProgress(value=cnt_freq_names)
        tmp_ft <- subset(tmp_f,timestamp==j)
        #calculate positions
        if(nrow(tmp_ft)>=2){
          tmp_fts<-merge(tmp_ft,stations_utm,by.x="Station",by.y="Station")
          #order using singal strength and take first two
          tmp_fts<-tmp_fts[order(tmp_fts$strength,decreasing = TRUE, na.last=NA),]
          if(anyNA(tmp_ft))
            next
          if(abs(angle_between(tmp_fts$angle[1],tmp_fts$angle[2]))<angles_allowed[1]|abs(angle_between(tmp_fts$angle[1],tmp_fts$angle[2]))>angles_allowed[2]) 
            next
          location<-triang(tmp_fts$utm.X[1],tmp_fts$utm.Y[1],tmp_fts$angle[1],tmp_fts$utm.X[2],tmp_fts$utm.Y[2],tmp_fts$angle[2])
          if(anyNA(location))
            next
          location_wgs<-utmtowgs(location[1],location[2],tmp_fts$utm.zone[1])#zone same as 1
          positions<-rbind(positions,cbind(timestamp=j,freq_tag=i,pos=location_wgs))
        }
      }
      cnt_freq_names<-cnt_freq_names+1
    }
  tmp<-positions[order(positions$timestamp),]
  return(cbind(tmp,speed_between_triangulations(tmp$timestamp,tmp$pos.X,tmp$pos.Y), stringsAsFactors = F))
}

timematch_inter <- function(data,intrer_error){
  tmp_s<-data[order(data$timestamp),]
  tmp_s$td <- c(0,diff(tmp_s$timestamp))
  gc<-0
  tmp_s$timestamp[1]<-tmp_s$timestamp[1]
  for(i in 2:nrow(tmp_s)){
    if(sum(tmp_s$td[(i-gc):i])<=intrer_error){
      tmp_s$timestamp[i]<- tmp_s$timestamp[i-gc-1]
      if(any(duplicated(tmp_s$Station[(i-gc-1):i]))){
        tmp_s$timestamp[i]<- tmp_s$timestamp[i]
        gc<--1
      }
      gc<-gc+1
    }else{
      tmp_s$timestamp[i]<- tmp_s$timestamp[i]
      gc<-0
    }
  }
  tmp_s$ti<-as.POSIXct(tmp_s$ti,origin="1970-01-01")
  return(tmp_s)
}

output$tri_speed <- renderPlot({
  req(global$triangulation)
  ggplot(global$triangulation) + geom_point(aes(x=timestamp,y=speed))
})

output$tri_distance <- renderPlot({
  req(global$triangulation)
  ggplot(global$triangulation) + geom_point(aes(x=timestamp,y=distance))
})

filter_distance <- function(triangulations, max_speed) {
  return(subset(triangulations, speed<=max_speed))
}
observeEvent(input$filter_distance,{
  req(global$triangulation)
  global$triangulation <- filter_distance(global$triangulation,input$tri_speed_slider)
})

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

tri_map <- renderLeaflet({
  req(global$receivers)
  map()%>%addStations(global$receivers, color="black", radius=10, group="Stations")
})