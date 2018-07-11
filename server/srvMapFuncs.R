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

# adds a line representing a bearing. Starts at station position, with the given angle. Length is adjusted according to signal strength
addBearing <-function(m, data, strength=25, color="red") {
  if (is.null(data))
    return(m)
  str_mod<-50
  data<-merge(data,global$receivers[,c("receiver","pos_x","pos_y")],by.x="station",by.y="receiver")
  if(!("strength" %in% colnames(data)))
    data<-cbind(data, strength=strength)
  utmcoords<-wgstoutm(data[,"pos_x"],data[,"pos_y"])
  utmcoords<-cbind(utmcoords, pos_x_2=utmcoords$X+cospi((90-data$angle)/180)*data$strength*str_mod, pos_y_2=utmcoords$Y+sinpi((90-data$angle)/180)*data$strength*str_mod)
  data<-cbind(data, pos2=utmtowgs(utmcoords$pos_x_2,utmcoords$pos_y_2,utmcoords$zone)[,c(1,2)])
  for (i in 1:nrow(data)) {
    print(paste("adding line for",data[i,]$freq))
    m<-m %>% addPolylines(lng=c(data[i,]$pos_x,data[i,]$pos2.X),lat=c(data[i,]$pos_y,data[i,]$pos2.Y),color=color,group = "bearings",weight = 2, label = htmltools::HTML(sprintf("angle: %i <br/> strength: %.2f",data[i,]$angle, data[i,]$strength)))
  }
  for (i in unique(data$station)){
    station<-global$receivers[global$receivers$receiver==i,]
    m<-m %>% addCircles(lng=station$pos_x, lat=station$pos_y,label = i)
  }
  return(m)
}

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