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
addBearings <-function(m, data, strength=25, ...) {
  if (is.null(data) || nrow(data)<=0)
    return(m)
  str_mod<-50
  data$strength[is.na(data$strength)]<-strength
  data<-cbind(data, utm2.X=data$utm.X+cospi((90-data$angle)/180)*data$strength*str_mod, utm2.Y=data$utm.Y+sinpi((90-data$angle)/180)*data$strength*str_mod)
  data<-cbind(data, pos2=utmtowgs(data$utm2.X,data$utm2.Y,data$utm.zone))
  for (i in 1:nrow(data)) {
    line<-data[i,]
    m<-m %>% addPolylines(lng=c(line$pos_x,line$pos2.X),lat=c(line$pos_y,line$pos2.Y), label = htmltools::HTML(sprintf("angle: %f <br/> strength: %.2f",line$angle, line$strength)),color="red", group="Bearings" )
  }
  return(m)
}


#' Adds circles representing stations to given map.
#'
#' @param m the map to add to
#' @param data data.frame with at least columns Station(char),Longitude(num),Latitude(num). Duplicates will be ignored
#' @param ... Further options passed to addCircles
#'
#' @return a new map object containing the old plus the stations
addStations <-function(m, data, ...) {
  if (is.null(data) || nrow(data)<=0)
    return(m)
  data<-data[!duplicated(data[,c("Station","Longitude","Latitude")]),]
  m<-m%>% addCircles(lng = data$Longitude, lat=data$Latitude, label=data$Station, ...)
}

#' Calculates triangulated positions and adds circles and dashed bearings to the map
#'
#' @param m the map to add to
#' @param data data.frame containing at least columns timestamp, station, angle, utm.X, utm.Y, utm.zone
#' @param showBearings logical, whether to draw bearings of successfully triangulated positions
#' @param error numeric, whether to draw triangulation error (0 or >0), and size of error zone
#' @param errorColor color to be used to draw the triangulation error
#' @param errorGroup leaflet group for triangulation error
#' @param errorOpacity opacity for triangulation error
#' @param ... additional arguments passed to addCircles for the triangulated positions
#'
#' @return the altered map
addTriangulations <- function(m, data, showBearings=T, error=0,  errorColor="blue", errorGroup="Tri Error", errorOpacity=0.3, ...) {
  list_of_timestamps<-unique(data$timestamp)
  triangulations<-data.frame(stringsAsFactors = F)
  bearings<-data.frame(stringsAsFactors = F)
  
  for (t in list_of_timestamps) {
    slot<-data[data$timestamp==t,]

    coords<-triang(slot[1,]$utm.X,slot[1,]$utm.Y,slot[1,]$angle,slot[2,]$utm.X,slot[2,]$utm.Y,slot[2,]$angle)
    if (anyNA(coords))
      next
    triangulations<-rbind(triangulations,list(timestamp=t,utm.X=coords[1],utm.Y=coords[2],utm.zone=slot[1,]$utm.zone,st1.X=slot[1,]$pos_x,st1.Y=slot[1,]$pos_y,st2.X=slot[2,]$pos_x,st2.Y=slot[2,]$pos_y),stringsAsFactors=F)

    if (error > 0) {
      x<-c(slot[1,]$utm.X,slot[2,]$utm.X)
      y<-c(slot[1,]$utm.Y,slot[2,]$utm.Y)
      alpha<-c(slot[1,]$angle,slot[2,]$angle)
      x<-c(x,x,x)
      y<-c(y,y,y)
      alpha<-c(alpha,alpha-error,alpha+error)
      points_utm<-triang_n(x,y,alpha)
      hull<-chull(points_utm)
      hull_wgs<-utmtowgs(points_utm$Easting[hull],points_utm$Northing[hull],slot[1,]$utm.zone)
      m <- m %>% addPolygons(hull_wgs$X, hull_wgs$Y, stroke = FALSE, opacity=errorOpacity, group=errorGroup, color=errorColor)
    }
    
  }
  if (nrow(triangulations)<=0)
    return(m)

  triangulations<-cbind(triangulations, pos=utmtowgs(triangulations$utm.X,triangulations$utm.Y,triangulations$utm.zone))
  
  pal <- colorNumeric(
    palette = "Spectral",
    domain = triangulations$timestamp)
  
  if (showBearings) {
    for (i in seq_len(nrow(triangulations))){
      t<-triangulations[i,]
      m <- m %>% addPolylines(lng=c(t$st1.X,t$pos.X,t$st2.X),lat=c(t$st1.Y,t$pos.Y,t$st2.Y),group="Tri Bearing", dashArray='4,4', color="blue", weight = 1)
      }
  }
  
  m <- m %>% addCircles(lng=triangulations$pos.X, lat=triangulations$pos.Y,label=paste("time:",triangulations$timestamp),color=pal(triangulations$timestamp), ...) #as.POSIXct(triangulations$timestamp,tz="UTC",origin="1970-01-01"
}

#' Draws a filled cone for every antenna, that has detected a bat
#'
#' @param m the map to add to
#'
#' @return the map with added cone
addDetectionCones<-function(m,data) {
  # print(paste("total", nrow(sorted_data()),"unique",length(unique(sorted_data()$timestamp)),"receivers",length(unique(sorted_data()$receiver))))
  #data<-subset(data_in,timestamp==timestamp[input$map_choose_single_data_set])
  if(nrow(data)==0) 
    return(NULL)
  validate(
    need(data, "Please have a look at the filter settings.")
  )
  for(p in 1:nrow(data)){
    if(!(data$receiver[p] %in% global$receivers$Name)) {
      next
    }
    a<-antennae_cones()[[data$receiver[p]]]
    label_kegel <- paste0("Signal Properties:",br(),
      "Receiver: ",data$receiver[p], br(),
      "Date and Time: ", data$time[p],br(),
      "Strength: ", data$max_signal[p],br(),
      "Length: ", data$duration[p],br(),
      "Bandwidth: ", data$signal_bw[p],br(),
      "Frequency: ",data$freq_tag[p]
    )
    m<- m %>% addPolygons(lng=a$x, lat=a$y, fillColor = color_palette()(data$max_signal[p]), fillOpacity=0.8, stroke=FALSE, popup=label_kegel, group="bats")
  }
  return(m)
}

# calculates cone shapes
antennae_cones<-reactive({
  cones=list()
  if (!is.null(global$receivers)) {
    r<-global$receivers
    for (a in seq_len(nrow(r))) {
      x<-r[a,]$Longitude
      y<-r[a,]$Latitude
      direction<-r[a,]$Orientation
      bw<-45
      len<-100
      wgs<-kegelcreation(x,y,direction,len,bw) # Many warnings...
      cones[[r[a,]$Name]]<-list(x=c(wgs$X,x), y=c(wgs$Y,y))
    }
  }
  return(cones)
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