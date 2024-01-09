#' Read DJI Flight logs
#' 
#' Reads and reformats csv files generated from dji flight logs using the *DJI Flight Log Viewer* web app. (Created by Mike Singer)
#' 
#' @details The time and date exported by the web app should be the local time zone at the place of recording, however this does not seem to work properly. 
#' To correct this, use param tz. Usually you can get the local time from the file name.
#' The following columns are imported:
#' * CUSTOM.date [local] - The date in local time zone. 
#' * CUSTOM.updateTime [local] - The time in local time zone.
#' * OSD.latitude - The latitude of current drone position.
#' * OSD.longitude - The longitude of current drone position.
#' * OSD.height - The height difference of current drone position to start position in ft.
#' * OSD.altitude - The height in ft above sea level.
#' * OSD.gpsNum - The number of gps satellite the drone is using.
#' * OSD.gpsLevel - A gps position quality metric defined by DJI, ranging 1-5, 5 being best.
#' * DETAILS.aircraftName - Name of the drone.
#' * DETAILS.aircraftSerial - Serial number of the drone.
#' 
#' The return value holds the following columns:
#' * timestamp - The time and date in UTC as POSIXct
#' * latitude - The latitude
#' * longitude - The Longitude
#' * height - Height in feet as compared to the start height.
#' * altitude - Altitude in feet above sea level.
#' * aircraftName - Drone name
#' * aircraftSerial - Drone serial number
#' * gpsNum - Number of GPS satellite the drone is using
#' * gpsLevel - GPS position quality metric defined by DJI, ranging 1-5, 5 being best.
#' 
#'
#' @param fpath string - File path to csv file
#' @param tz Time zone the input uses.
#' @param uniquify_ts boolean - Whether to aggregate positions with identical timestamp using [stats::median()].
#'
#' @return data.table - A table of the data read. See details for list of columns
#' @export
#'
readDJI <- function(fpath, tz="Etc/GMT+4", uniquify_ts=TRUE) {
  require(lubridate)
  require(data.table)
  options(digits.secs=3)
  options(digits = 10)
  dta<-fread(
    fpath, 
    select = c(
      "CUSTOM.date [local]", 
      "CUSTOM.updateTime [local]", 
      "OSD.latitude", 
      "OSD.longitude", 
      "OSD.height [ft]", 
      "OSD.altitude [ft]", 
      "OSD.gpsNum", 
      "OSD.gpsLevel", 
      "DETAILS.aircraftName", 
      "DETAILS.aircraftSerial"
    )
  )
  
  dta[, timestamp:=with_tz(parse_date_time(paste(`CUSTOM.date [local]`, `CUSTOM.updateTime [local]`), "%m/%d/%Y %I:%M:%OS %p", tz = tz), tzone = "GMT")]
  dta[, `CUSTOM.date [local]`:=NULL] 
  dta[, `CUSTOM.updateTime [local]`:=NULL]
  dta[, `OSD.height [ft]`:=`OSD.height [ft]`*0.3048]
  dta[, `OSD.altitude [ft]`:=`OSD.altitude [ft]`*0.3048]
  
  setnames(
    dta, 
    c(
      "OSD.latitude", 
      "OSD.longitude", 
      "OSD.height [ft]", 
      "OSD.altitude [ft]", 
      "OSD.gpsNum", 
      "OSD.gpsLevel", 
      "DETAILS.aircraftName", 
      "DETAILS.aircraftSerial"
    ),
    c(
      "latitude", 
      "longitude", 
      "height", 
      "altitude", 
      "gpsNum", 
      "gpsLevel", 
      "aircraftName", 
      "aircraftSerial"
    )
  )
  
  setcolorder(
    dta, 
    c(
      "timestamp",
      "latitude", 
      "longitude", 
      "height", 
      "altitude", 
      "aircraftName", 
      "aircraftSerial", 
      "gpsNum", 
      "gpsLevel"
    )
  )
  
  if(isTRUE(uniquify_ts)) {
    dta <- dta[, 
      .(
        "latitude"=median(latitude, na.rm = T), 
        "longitude"=mean(longitude, na.rm = T), 
        "height"=mean(height, na.rm = T), 
        "altitude"=mean(altitude, na.rm = T), 
        "gpsNum"=mean(gpsNum, na.rm = T), 
        "gpsLevel"=mean(gpsLevel, na.rm = T)
      ), 
      by=.(timestamp, aircraftName, aircraftSerial)
    ]
  }
  return(dta)
}