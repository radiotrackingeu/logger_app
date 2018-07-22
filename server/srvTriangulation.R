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

triang_n <- function(x,y,alpha){
  # For Triangulation GK Coordinates are necesarry!
  # First calculate tan keeping in mind that 0° in geo-coordinates are 90° in a x-y plane
  talpha <- (alpha%%360)/180*pi
  #punkte auf einer linie:
  #if(((alpha1-alpha2)%%180)==0){print("No triangulation possible: all three points are on one line")
  #  return(c(NA,NA))}

  s<-combn(seq(1,length(x)),2)

  intersections <- NULL

  for(i in 1:dim(s)[2]){
    #i Findinf Intersection Using solver
    b<-c(x[s[2,i]]-x[s[1,i]],y[s[2,i]]-y[s[1,i]])
    a1<-c(sin(talpha[s[1,i]]),cos(talpha[s[1,i]]))
    a2<-c(-sin(talpha[s[2,i]]),-cos(talpha[s[2,i]]))
    a<-matrix(c(a1,a2),nrow=2)
    l<-solve(a,b)
    px<-x[s[1,i]]+l[1]*sin(talpha[s[1,i]])
    py<-y[s[1,i]]+l[1]*cos(talpha[s[1,i]])

    if(l[2]>0&l[1]>0)
    {
      intersections <-rbind(intersections,c(px,py))
    }
    else{
      # print("No triangulation possible: lines don't intersect")
    }
  }
  intersections<-as.data.frame(intersections)
  names(intersections)<-c("Easting","Northing")
  return(intersections)
}

map_n_hull_error <- function(x,y,alpha,alpha_error){




  m =
  return(m)
}

gktowgs <-function(x,y){
  require(rgdal)
  zone<-as.numeric(substring(x,1,1))
  GK <- data.frame(cbind("X_GK"=x,"Y_GK"=y))
  #Spatial Information, Coordinates Transform
  projection<-paste0("+proj=tmerc +lat_0=0 +lon_0=",zone*3," +k=1 +x_0=",zone,"500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs")
  coordinates(GK) <- c("X_GK", "Y_GK")
  #print(projection)
  proj4string(GK) <- CRS(projection) # Defining Gauss Krüger
  GK.WGS84 <- spTransform(GK, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) # tranforming to WGS84 longlat
  return(GK.WGS84)
}

wgstogk <-function(x,y){
  require(rgdal)
  GK <- data.frame(cbind("X_GK"=x,"Y_GK"=y))
  mid_lat<-floor((x+1.5)/3)
  #print(mid_lat)
  #Spatial Information, Coordinates Transform
  projection<-paste0("+proj=tmerc +lat_0=0 +lon_0=",mid_lat*3," +k=1 +x_0=",mid_lat,"500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs")
  coordinates(GK) <- c("X_GK", "Y_GK")
  #print(projection)
  proj4string(GK) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Defining Gauss Krüger
  GK.WGS84 <- spTransform(GK, CRS(projection)) # tranforming to WGS84 longlat
  return(GK.WGS84)
}

writeDataSQL <- function(df){
  require(DBI)

  con <- dbConnect(MySQL(), user = "triApp", password="triapp", dbname= "Triangulation")
  old_df<-dbReadTable(con, "angles")
  new_df<-unique(rbind(old_df,df))
  dbWriteTable(con, "angles",value= new_df, overwrite=TRUE ,row.names = FALSE)
  dbDisconnect(con)

}

showintersectiongk_error <- function(x1gk,y1gk,alpha1,x2gk,y2gk,alpha2,r, server = "http://localhost/projects/Tiles/{z}/{x}/{y}.png",error){

  require(leaflet)
  if(error>0){
    x<-c(x1gk,x2gk)
    y<-c(y1gk,y2gk)
    alpha<-c(alpha1,alpha2)
    x<-c(x,x,x)
    y<-c(y,y,y)
    alpha<-c(alpha,alpha-error,alpha+error)
    wgs<-gktowgs(x,y)
    points_gk<-triang_n(x,y,alpha)
    hull<-chull(points_gk)
    hull_wgs<-gktowgs(points_gk$Easting[hull],points_gk$Northing[hull])
  }


  #Start and End Point of Lines to show Intersection and conversion to WGS
  pl1a <- gktowgs(x1gk,y1gk)@coords
  pl1b <- gktowgs(x1gk+r*cos((90-alpha1)/180*pi),y1gk+r*sin((90-alpha1)/180*pi))@coords[]
  pl2a <- gktowgs(x2gk,y2gk)@coords
  pl2b <- gktowgs(x2gk+r*cos((90-alpha2)/180*pi),y2gk+r*sin((90-alpha2)/180*pi))@coords[]

  # Find point of Intersection
  coordinates<-triang(x1gk,y1gk,alpha1,x2gk,y2gk,alpha2)

  # Plotting using leaflet showing just directions and/or intersection
  if(substring(server,1,4)=="http"){m = leaflet() %>%  addTiles(urlTemplate = server)}
  else {m = leaflet() %>%  addProviderTiles(server)}

  if(is.na(coordinates[1])){
    m  %>% addPolylines(c(pl1a[1],pl1b[1]),c(pl1a[2],pl1b[2]),col="red") %>% addPolylines(c(pl2a[1],pl2b[1]),c(pl2a[2],pl2b[2]),col="green") %>% addCircles(c(pl1a[1],pl2a[1]),c(pl1a[2],pl2a[2]),col=c("red","green"),radius=40)
  }
  else
  {
    # Conversion to WGS
    sr<-gktowgs(coordinates[1],coordinates[2])@coords
    m <- m %>% addPolylines(c(pl1a[1],sr[1]),c(pl1a[2],sr[2]),col="red") %>% addPolylines(c(pl2a[1],sr[1]),c(pl2a[2],sr[2]),col="green") %>% addCircles(c(pl1a[1],pl2a[1]),c(pl1a[2],pl2a[2]),col=c("red","green"),radius=40) %>% addMarkers(sr[1], sr[2], popup = paste0("WGS84-LON: ",round(sr[1],6),"<br/>", " WGS84-LAT: ", round(sr[2],6),"<br/>","GK-E: ",round(coordinates[1]),"<br/>","GK-N: ",round(coordinates[2])))
    if(error>0) {
      m <- m %>% addPolygons(hull_wgs$X_GK,hull_wgs$Y_GK,stroke = FALSE, opacity=0.3)
    }
    m
    }
}

showintersectiongk <- function(x1gk,y1gk,alpha1,x2gk,y2gk,alpha2,r, server = "http://localhost/projects/Tiles/{z}/{x}/{y}.png"){

  require(leaflet)

  #Start and End Point of Lines to show Intersection and conversion to WGS
  pl1a <- gktowgs(x1gk,y1gk)@coords
  pl1b <- gktowgs(x1gk+r*cos((90-alpha1)/180*pi),y1gk+r*sin((90-alpha1)/180*pi))@coords[]
  pl2a <- gktowgs(x2gk,y2gk)@coords
  pl2b <- gktowgs(x2gk+r*cos((90-alpha2)/180*pi),y2gk+r*sin((90-alpha2)/180*pi))@coords[]

  # Find point of Intersection
  coordinates<-triang(x1gk,y1gk,alpha1,x2gk,y2gk,alpha2)

  # Plotting using leaflet showing just directions and/or intersection
  if(substring(server,1,4)=="http"){m = leaflet() %>%  addTiles(urlTemplate = server)}
  else {m = leaflet() %>%  addProviderTiles(server)}

  if(is.na(coordinates[1])){
    m  %>% addPolylines(c(pl1a[1],pl1b[1]),c(pl1a[2],pl1b[2]),col="red") %>% addPolylines(c(pl2a[1],pl2b[1]),c(pl2a[2],pl2b[2]),col="green") %>% addCircles(c(pl1a[1],pl2a[1]),c(pl1a[2],pl2a[2]),col=c("red","green"),radius=40)
  }
  else
  {
    # Conversion to WGS
    sr<-gktowgs(coordinates[1],coordinates[2])@coords
    m %>% addPolylines(c(pl1a[1],sr[1]),c(pl1a[2],sr[2]),col="red") %>% addPolylines(c(pl2a[1],sr[1]),c(pl2a[2],sr[2]),col="green") %>% addCircles(c(pl1a[1],pl2a[1]),c(pl1a[2],pl2a[2]),col=c("red","green"),radius=40) %>% addMarkers(sr[1], sr[2], popup = paste0("WGS84-LON: ",round(sr[1],6),"<br/>", " WGS84-LAT: ", round(sr[2],6),"<br/>","GK-E: ",round(coordinates[1]),"<br/>","GK-N: ",round(coordinates[2])))
    }
}

showintersectionsgk <- function(tridata, server = "http://localhost/projects/Tiles/{z}/{x}/{y}.png"){

  require(leaflet)

  coordinates<-NULL
  #Loop to triangulate using the triang function
  for(i in 1:nrow(tridata)){
    coordinates<-rbind(coordinates,triang(tridata$X1[i],tridata$Y1[i],tridata$A1[i],tridata$X2[i],tridata$Y2[i],tridata$A2[i]))
  }

  # Creates unique set of Signal Receiving Points
  peilpunkte<-unique(data.frame(tridata$X1,tridata$Y1,tridata$X2,tridata$Y2))

  #Exclude triangulations which weren't possible
  coordinates2<-na.exclude(cbind(as.data.frame(coordinates),paste("Date: ",tridata$Date," Time: ",tridata$Time," Comment: ",tridata$Comment)))

  #Converting to WGS System for leaflet

  sr<-gktowgs(coordinates2[,1],coordinates2[,2])@coords
  sp1<-gktowgs(peilpunkte[,1],peilpunkte[,2])@coords
  sp2<-gktowgs(peilpunkte[,3],peilpunkte[,4])@coords

  #Plot using leaflet and OpenStreetmap Online or Offline - for offline usage tiles must be downloaded (Maperitive) and hosted (using WAMP or EasyPHP)
  #More Maps at http://leaflet-extras.github.io/leaflet-providers/preview/ using addProviderTiles("Esri.WorldImagery")
  if(substring(server,1,4)=="http"){m = leaflet() %>%  addTiles(urlTemplate = server)}
  else {m = leaflet() %>%  addProviderTiles(server)}

  m %>% addCircles(sr[,1], sr[,2], popup = coordinates2[,3]) %>% addCircles(sp1[,1],sp1[,2], color = "red") %>% addCircles(sp2[,1],sp2[,2],color = "yellow")

}

leaflet_triang_gk_csv <- function(filename, server = "http://localhost/projects/Tiles/{z}/{x}/{y}.png"){

  require(leaflet)

  # Load Data from CSV File
  tridata<-read.csv2(filename, stringsAsFactors = FALSE)

  #Loop to triangulate using the triang function
  coordinates<-NULL
  for(i in 1:nrow(tridata)){
    coordinates<-rbind(coordinates,triang(tridata[i,1],tridata[i,2],tridata[i,3],tridata[i,4],tridata[i,5],tridata[i,6]))
  }

  peilpunkte<-unique(tridata[,c(1:2,4:5)])

  #Exclude triangulations which weren't possible
  coordinates2<-na.exclude(cbind(as.data.frame(coordinates),tridata[,7]))

  #Converting to WGS System for leaflet
  sr<-gktowgs(coordinates2[,1],coordinates2[,2])@coords
  sp1<-gktowgs(peilpunkte[,1],peilpunkte[,2])@coords
  sp2<-gktowgs(peilpunkte[,3],peilpunkte[,4])@coords

  #Plot using leaflet and OpenStreetmap Online or Offline - for offline usage tiles must be downloaded (Maperitive) and hosted (using WAMP or EasyPHP)
  m = leaflet() %>% addTiles(urlTemplate = server)
  m %>% addMarkers(sr[,1], sr[,2], popup = coordinates2[,3]) %>% addCircles(sp1[,1],sp1[,2], color = "red") %>% addCircles(sp2[,1],sp2[,2],color = "yellow")
}
