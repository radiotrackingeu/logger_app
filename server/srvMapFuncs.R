# adds circles at antennae positions on given map.
addAntennaePositions<-function(m) {
  m %>% addCircles(group="antennae_positions", lng=antennae_data()$pos_x,lat = antennae_data()$pos_y)
}

# adds the cone outline to antennae on given map.
addAntennaeCones<- function(m) {
  for(name in names(antennae_cones())) {
    m<-m %>% addPolygons(group="antennae_cones", lng=antennae_cones()[[name]]$x, lat=antennae_cones()[[name]]$y, fill=FALSE, opacity=0.5, stroke=TRUE, weight=1)
  }
  return(m)
}

#' adds lines representing bearings. Starts at station position, with the given angle. Length is adjusted according to signal strength
#'
#' @param m the map to add to
#' @param data data.frame with at least columns station(char),angle(num),pos_x(num),pos_y(num),utm.X(num),utm.Y(num),utm.zone(num). Values in station must be from global$receivers. column strength(num) is optional
#' @param strength determines length of line drawn. default is 25
#' @param color color to be used for the lines. default is "red"
#' @param group leaflet group for the lines, default is "bearings"
#'
#' @return a map object containing m and the new lines
addBearings <-function(m, data, strength=25, color="red", group="bearings") {
  if (is.null(data))
    return(m)
  str_mod<-50
  if(!("strength" %in% colnames(data)))
    data<-cbind(data, strength=strength)
  data<-cbind(data, utm2.X=data$utm.X+cospi((90-data$angle)/180)*data$strength*str_mod, utm2.Y=data$utm.Y+sinpi((90-data$angle)/180)*data$strength*str_mod)
  data<-cbind(data, pos2=utmtowgs(data$utm2.X,data$utm2.Y,data$utm.zone))
  for (i in 1:nrow(data)) {
    line<-data[i,]
    m<-m %>% addPolylines(lng=c(line$pos_x,line$pos2.X),lat=c(line$pos_y,line$pos2.Y),color=color,group = group, weight = 2, label = htmltools::HTML(sprintf("angle: %i <br/> strength: %.2f",line$angle, line$strength)))
  }
  return(m)
}


#' Adds circles representing stations to given map.
#'
#' @param m the map to add to
#' @param data data.frame with at least columns station(char),pos_x(num),pos_y(num). Values in station must be from global$receivers.
#' @param color color to be used default "blue"
#' @param size size of circle to be drawn, default 5
#' @param group leaflet group, default "stations"
#'
#' @return a new map object containing the old plus the stations
addStations <-function(m, data, color="blue", size=5, group="stations") {
  data<-data[!duplicated(data[,"station"]),]
  m<-m%>% addCircles(lng = data$pos_x, lat=data$pos_y, label=data$station, group = group)
}

addTriangulations <- function(m, data, colorDot="red", sizeDot=5, colorZone="blue", groupDot="triangulations", groupZone=triangulationsError) {
  # list_of_timestamps<-unique(data$timestamp)
  # 
  # for (i in list_of_timestamps) {
  #   
  # }
}

tri_timeslot_data <- reactive ({
  return_tmp<-data.frame(timestamp=NA,freq=NA,station=NA,angle=NA)
  data<-tri_position_data()
  #find for each receiver
  list_of_stations<-unique(data$station)
  #and each frequency
  list_of_frequencies<-unique(data$freq)
  #signals which appear betweeen time_distance[1] and time_distance[2] seconds
  for (t in seq(min(data$timestamp),max(data$timestamp),5)) {
    data_t<-subset(data, data$timestamp >= t & data$timestamp < t+5)
    for (f in unique(data_t$freq)) {
      data_tf <- subset(data_t, data_t$freq==f)
      for (s in unique(data_tf$station)) {
        data_tfs <- subset(data_tf, data_tf$station==s)
        # print(data_tfs)
        if (nrow(data_tfs > 0)) {
          line<-c(timestamp=t,freq=f,station=s,angle=mean(data_tfs$angle))
          # print(line)
          return_tmp<-rbind(return_tmp,line)
        }
        else 
          print(paste("No data for ",t,f,s))
      }
    }
  }
  print(return_tmp)
})


# calculates cone shapes
antennae_cones<-reactive({
  cones=NULL
  if (!is.null(antennae_data())) {
    for(a in 1:nrow(antennae_data())){
      antennae<-antennae_data()[a,]
      x<-antennae$pos_x
      y<-antennae$pos_y
      direction<-antennae$orientation
      bw<-antennae$beam_width
      len<-100*antennae$gain
      wgs<-kegelcreation(x,y,direction,len,bw) # Many warnings...
      cones[[antennae$receiver]]<-list(x=c(wgs$X,x), y=c(wgs$Y,y))
    }
  }
  cones
})

# calculates corner coordinates of antenna reception area
kegelcreation<-function(x,y,dir,length,deg){
  # first convert to utm
  utm<-wgstoutm(x,y)
  
  # then calc new coords
  
  kr <- (dir-deg/2)/180*pi
  kl <- (dir+deg/2)/180*pi
  
  utm_x1 <- utm$X + sin(kr) * length
  utm_y1 <- utm$Y + cos(kr) * length
  utm_zone1 <- utm$zone # if the zones switch, this setting needs to be changed 
  
  utm_x2 <- utm$X + sin(kl) * length
  utm_y2 <- utm$Y + cos(kl) * length
  utm_zone2 <- utm$zone # if the zones switch, this setting needs to be changed 
  
  # convert back to wgs
  wgs<-utmtowgs(c(utm_x1,utm_x2),c(utm_y1,utm_y2),c(utm_zone1,utm_zone2))
  return(wgs)
}

# WGS to UTM conversion
# in 2 numeric
# out data.frame
wgstoutm<-function(x,y){
  zone<-(floor((x + 180)/6) %% 60) + 1
  #xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  xy <- data.frame(cbind("X"=x,"Y"=y))
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(cbind.data.frame(X=res$X,Y=res$Y,zone))
}

# UTM to WGS conversion
utmtowgs<-function(x,y,zone){
  #xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  xy <- data.frame(cbind("X"=x,"Y"=y))
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS(paste0("+proj=utm +zone=",zone," +datum=WGS84"))  ## for example
  res <- spTransform(xy, CRS("+proj=longlat +datum=WGS84"))
  return(as.data.frame(res))
}