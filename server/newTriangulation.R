#1) Calc Doa: timestamp in full seconds, angle, number of antennas, station, freq_tag, strength
#doa_smoothed() does that in srvDoA.R
#2) Find matching stations: time + freq_tag
observeEvent(input$calc_triangulations,{
  req(global$bearing)
  positions<-NULL
  #Calc UTM of Stations and add them
  stations<-unique(global$receivers[,c("Station","Longitude","Latitude")])
  stations_utm<-cbind(stations,utm=wgstoutm(stations[,"Longitude"],stations[,"Latitude"]))
  if(length(unique(stations_utm$utm.zone))>1){
    print("UTM Zone Problem")
  }
  #for each frequency tag
  for(i in unique(global$frequencies$Name)){
    tmp_f <- subset(global$bearing,freq_tag==i)
    #for each times interval
    for(j in unique(tmp_f$timestamp)){ #no error in timestamp allowed
      tmp_ft <- subset(tmp_f,timestamp>j-input$time_error_inter_station&timestamp<j+input$time_error_inter_station)
      #3) Calc Positions
      if(nrow(tmp_ft)>=2){
        tmp_fts<-merge(tmp_ft,stations_utm,by.x="Station",by.y="Station")
        #order using singal strength and take first two
        tmp_fts<-tmp_fts[order(tmp_fts$strength,decreasing = TRUE, na.last=NA),]
        if(anyNA(tmp_ft))
          next
        if(abs(angle_between(tmp_fts$angle[1],tmp_fts$angle[2]))<input$slider_angles_allowed[1]|abs(angle_between(tmp_fts$angle[1],tmp_fts$angle[2]))>input$slider_angles_allowed[2]) 
          next
        location<-triang(tmp_fts$utm.X[1],tmp_fts$utm.Y[1],tmp_fts$angle[1],tmp_fts$utm.X[2],tmp_fts$utm.Y[2],tmp_fts$angle[2])
        if(anyNA(location))
          next
        location_wgs<-utmtowgs(location[1],location[2],tmp_fts$utm.zone[1])#zone same as 1
        positions<-rbind(positions,cbind(timestamp=j,freq_tag=i,pos=location_wgs))
      }
    }
  }
  global$triangulation<-positions
})



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