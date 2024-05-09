addAntennaeCones <- function(m, cones, group = "antennae_cones") {
  m <- m %>% clearGroup(group)
  for (name in names(cones)) {
    if (length(cones[[name]]) > 1) {
      m <-
        m %>% addPolygons(
          lng = cones[[name]]$x,
          lat = cones[[name]]$y,
          label = paste0(name, ": ", cones[[name]]$orientation, "°"),
          group = group,
          fill = FALSE,
          opacity = 0.5,
          stroke = TRUE,
          weight = 1
        )
    } else{
      m <-
        m %>% addCircles(
          lng = cones[[name]]$x,
          lat = cones[[name]]$y,
          fill = FALSE,
          opacity = 0.5,
          stroke = TRUE,
          weight = 1,
          radius = 10,
          group = group
        )
    }
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
  stations<-data[!duplicated(data[,c("Station","Longitude","Latitude")]),]
  setorder(data, Orientation)
  for (i in 1:nrow(stations)) {
    m <- m %>% addMarkers(
      lng = stations$Longitude[i],
      lat = stations$Latitude[i], 
      label = stations$Station[i],
      icon = makeIcon(iconUrl = "icon_mast.png", iconWidth = 25, iconHeight = 25, iconAnchorX = 25*0.49, iconAnchorY = 25*0.95),
      popup = HTML(
        "<b>", stations$Station[i], "</b><br>", 
        paste0(round(stations$Latitude[i], 4), "°N ", round(stations$Longitude[i], 4), "°E<br>"), 
        data[Name==stations[i]$Name & Longitude==stations[i]$Longitude & Latitude==stations[i]$Latitude, paste0(Name, ": ", sprintf("%03d", Orientation), "°", collapse = "<br>")]
      ),
      layerId = stations$Station[i],
      ...
    )
  }
  m
}

#' #' Calculates triangulated positions and adds circles and dashed bearings to the map
#' #'
#' #' @param m the map to add to
#' #' @param data data.frame containing at least columns timestamp, station, angle, utm.X, utm.Y, utm.zone
#' #' @param showBearings logical, whether to draw bearings of successfully triangulated positions
#' #' @param error numeric, whether to draw triangulation error (0 or >0), and size of error zone
#' #' @param errorColor color to be used to draw the triangulation error
#' #' @param errorGroup leaflet group for triangulation error
#' #' @param errorOpacity opacity for triangulation error
#' #' @param ... additional arguments passed to addCircles for the triangulated positions
#' #'
#' #' @return the altered map
#' addTriangulations <- function(m, data, showBearings=T, error=0,  errorColor="blue", errorGroup="Tri Error", errorOpacity=0.3, ...) {
#'   list_of_timestamps<-unique(data$timestamp)
#'   triangulations<-data.frame(stringsAsFactors = F)
#'   bearings<-data.frame(stringsAsFactors = F)
#'   
#'   for (t in list_of_timestamps) {
#'     slot<-data[data$timestamp==t,]
#' 
#'     coords<-triang(slot[1,]$utm.X,slot[1,]$utm.Y,slot[1,]$angle,slot[2,]$utm.X,slot[2,]$utm.Y,slot[2,]$angle)
#'     if (anyNA(coords))
#'       next
#'     triangulations<-rbind(triangulations,list(timestamp=t,utm.X=coords[1],utm.Y=coords[2],utm.zone=slot[1,]$utm.zone,st1.X=slot[1,]$pos_x,st1.Y=slot[1,]$pos_y,st2.X=slot[2,]$pos_x,st2.Y=slot[2,]$pos_y),stringsAsFactors=F)
#' 
#'     if (error > 0) {
#'       x<-c(slot[1,]$utm.X,slot[2,]$utm.X)
#'       y<-c(slot[1,]$utm.Y,slot[2,]$utm.Y)
#'       alpha<-c(slot[1,]$angle,slot[2,]$angle)
#'       x<-c(x,x,x)
#'       y<-c(y,y,y)
#'       alpha<-c(alpha,alpha-error,alpha+error)
#'       points_utm<-triang_n(x,y,alpha)
#'       hull<-chull(points_utm)
#'       hull_wgs<-utmtowgs(points_utm$Easting[hull],points_utm$Northing[hull],slot[1,]$utm.zone)
#'       m <- m %>% addPolygons(hull_wgs$X, hull_wgs$Y, stroke = FALSE, opacity=errorOpacity, group=errorGroup, color=errorColor)
#'     }
#'     
#'   }
#'   if (nrow(triangulations)<=0)
#'     return(m)
#' 
#'   triangulations<-cbind(triangulations, pos=utmtowgs(triangulations$utm.X,triangulations$utm.Y,triangulations$utm.zone))
#'   
#'   pal <- colorNumeric(
#'     palette = "Spectral",
#'     domain = triangulations$timestamp)
#'   
#'   if (showBearings) {
#'     for (i in seq_len(nrow(triangulations))){
#'       t<-triangulations[i,]
#'       m <- m %>% addPolylines(lng=c(t$st1.X,t$pos.X,t$st2.X),lat=c(t$st1.Y,t$pos.Y,t$st2.Y),group="Tri Bearing", dashArray='4,4', color="blue", weight = 1)
#'       }
#'   }
#'   global$triangulation <- triangulations
#'   m <- m %>% addCircles(lng=triangulations$pos.X, lat=triangulations$pos.Y,label=paste("time:",triangulations$timestamp),color=pal(triangulations$timestamp), ...)#as.POSIXct(triangulations$timestamp,tz="UTC")
#' }

#' #' Draws a filled cone for every antenna, that has detected a bat
#' #'
#' #' @param m the map to add to
#' #'
#' #' @return the map with added cone
#' addDetectionCones<-function(m, data) {
#'   # print(paste("total", nrow(sorted_data()),"unique",length(unique(sorted_data()$timestamp)),"antennas",length(unique(sorted_data()$receiver))))
#'   #data<-subset(data_in,timestamp==timestamp[input$map_choose_single_data_set])
#'   if(nrow(data)==0) 
#'     return(NULL)
#'   shiny::validate(
#'     need(data, "Please have a look at the filter settings.")
#'   )
#'   for(p in 1:nrow(data)){
#'     if(!(data$receiver[p] %in% global$receivers$Name)) {
#'       next
#'     }
#'     a<-antennae_cones()[[data$receiver[p]]]
#'     label_kegel <- paste0("Signal Properties:",br(),
#'       "Antenna: ",data$receiver[p], br(),
#'       "Date and Time: ", data$time[p],br(),
#'       "Strength: ", data$max_signal[p],br(),
#'       "Length: ", data$duration[p],br(),
#'       "Bandwidth: ", data$signal_bw[p],br(),
#'       "Frequency: ",data$freq_tag[p]
#'     )
#'     m<- m %>% addPolygons(lng=a$x, lat=a$y, fillColor = color_palette()(data$max_signal[p]), fillOpacity=0.8, stroke=FALSE, popup=label_kegel, group="bats")
#'   }
#'   return(m)
#' }

addDetectionCones <- function(m, cones, bearings, zIndex=300) {
  a_ply(.data = bearings, .margins = 1, .fun = function(b) {
    receivers <- tstrsplit(b$recs_all, "/")
    strengths <- tstrsplit(b$strengths,"/", type.convert = T)
    num_signals <- str_count(tstrsplit(b$sIds, "/"), "\\\\")+1
    for (r in seq_len(length(receivers))) {
      cone<-cones[[receivers[[r]]]]
      if (!is.null(cone)) {
        label_cone <- paste0(
          "Signal Properties:",br(),
          "Receiver: ",receivers[r], br(),
          "Date and Timeslot: ",as.POSIXct(b$time_matched, origin="1970-01-01", tz= "GMT"),br(),
          "Number of signals in Timeslot: ", num_signals[1], br() ,
          "Average Strength: ", round(strengths[[r]],4)," dB"
        )
        m <<- m %>%
          addPolygons(
            lng=cone$x, lat=cone$y, fillColor = color_palette()(strengths[[r]]), fillOpacity=0.8, stroke=FALSE, popup=label_cone, group="cones", options = tileOptions(zIndex = zIndex)
          )
      }
    }
  })
  return(m )
}

# calculates cone shapes
calculate_antennae_cones <- function(receivers) {
  cones=list()
  if (is.null(receivers)) 
    return (cones)
  for (a in seq_len(nrow(receivers))) {
    if(!any(is.na(receivers[a,]))){
      x<-receivers[a,]$Longitude
      y<-receivers[a,]$Latitude
      direction<-receivers[a,]$Orientation
      bw<-45
      len<-100
      wgs<-calculate_cone_corners(x,y,direction,len,bw) # Many warnings...
      cones[[receivers[a,]$Name]]<-list(x=c(wgs$X,x), y=c(wgs$Y,y))
    }else{
      cones[[receivers[a,]$Name]]<-list(x=receivers[a,]$Longitude, y=receivers[a,]$Latitude)
    }
  }
  return(cones)
}

# calculates corner coordinates of antenna reception area
calculate_cone_corners<-function(x,y,dir,length,deg){
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

##WGS to UTM conversion
##in 2 numeric
#out data.frame
# wgstoutm<-function(x,y){
#   start_time <- proc.time()  # Start timing
#   tmp<-data.frame(X = numeric(),Y= numeric(),zone= numeric())
#   for(i in 1:length(x)){
#     zone<-(floor((x[i] + 180)/6) %% 60) + 1
#     xy <- data.frame(cbind("X"=x[i],"Y"=y[i]))
#     sp::coordinates(xy) <- c("X", "Y")
#     proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
#     result <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
#     result <- as.data.frame(result)
#     names(result) <- c("X","Y")
#     tmp<-rbind(tmp,data.frame(cbind(X=result$X,Y=result$Y,zone)))
#   }
# 
#   end_time <- proc.time() - start_time  # Calculate elapsed time
#   print(end_time)  # Print the timing information
#   return(tmp)
# }

wgstoutm <- function(x, y) {
  # Create an sf object
  data <- data.frame(X = x, Y = y)
  sf_data <- st_as_sf(data, coords = c("X", "Y"), crs = 4326)  # WGS84 Lat Long

  # Function to calculate UTM zone based on longitude
  get_utm_zone <- function(longitude) {
    (floor((longitude + 180) / 6) %% 60) + 1
  }
  # Create a data frame to store results
  results <- data.frame(X = numeric(length(x)), Y = numeric(length(y)), zone = integer(length(x)))

  # Loop through each row to calculate the UTM zone and transform
  for (i in seq_along(x)) {
    zone <- get_utm_zone(x[i])
    crs_string <- sprintf("+proj=utm +zone=%d +ellps=WGS84 +datum=WGS84 +units=m +no_defs", zone)
    transformed <- st_transform(sf_data[i, ], crs = crs_string)

    # Extract transformed coordinates
    coords <- st_coordinates(transformed)
    results$X[i] <- coords[1, "X"]
    results$Y[i] <- coords[1, "Y"]
    results$zone[i] <- zone
  }
    

  return(results)
}

# UTM to WGS conversion

utmtowgs <- function(x,y, zone) {
  # Create an sf object
  data <- data.frame(X = x, Y = y, zone = zone)

  # Initialize an empty data frame to store results
  results <- data.frame(X = numeric(length(x)), Y = numeric(length(y)))
  
  #Process each point individually
  for (i in 1:length(x)) {
    #Define the CRS for the UTM coordinates based on the zone
    utm_crs <- sprintf("+proj=utm +zone=%d +ellps=WGS84 +datum=WGS84 +units=m +no_defs", zone[i])
    
    #Create an sf object with the appropriate UTM CRS
    xy_sf <- st_as_sf(data.frame(X = x[i], Y = y[i]), coords = c("X", "Y"), crs = utm_crs)
    
    # Transform the coordinates to WGS84
    transformed <- st_transform(xy_sf, crs = "+proj=longlat +datum=WGS84")
    
    # Extract the longitude and latitude
    coords <- st_coordinates(transformed)
    results$X[i] <- coords[1, "X"]
    results$Y[i] <- coords[1, "Y"]
  }
  
  return(results)
}

# utmtowgs<-function(x,y,zone){
#   tmp<-data.frame()
#   for(i in 1:length(x)){
#     xy <- data.frame(cbind("X"=x[i],"Y"=y[i]))
#     coordinates(xy) <- c("X", "Y")
#     proj4string(xy) <- CRS(paste0("+proj=utm +zone=",zone[i]," +datum=WGS84"))  ## for example
#     res <- spTransform(xy, CRS("+proj=longlat +datum=WGS84"))
#     tmp<-rbind(tmp,as.data.frame(res))
#   }
# 
#   return(tmp)
# }

estimateDist <- function(max_signal, negativeSigStrength = TRUE, minLength = NULL, maxLength = NULL, gain = 49, tagStrength = 600, magicNumber = 15) {
  if(negativeSigStrength)
    max_signal <- max_signal + 100
  consts <- gain + 10*log(tagStrength) + magicNumber - 20*log10(150000000) - 20*log10(4*pi/300000000)
  result <- 10 ^ ((consts - max_signal) / 20)
  if(!is.null(minLength))
    result[result < minLength] <- minLength
  if(!is.null(maxLength))
    result[result > maxLength] <- maxLength
  return(result)
}
