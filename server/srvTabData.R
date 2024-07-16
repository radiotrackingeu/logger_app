#_########### srvTabData.R ############

# maybe predefine content of tables

#global$connections to store information how to connect to dbs
#global$active_connections to store information on which connection to connect to
#global$receivers to list all used receivers with the orientation and location
#global$frequencies to list the transmitters porperties like frequency, etc.
#global$calibration to list the sensitivy of each receiver
#global$signals is a the dataframe to store all received signals
#global$map_markers is a dataframe containing positions and labels of markers added to the map
#global$calibrated is a boolean indicating whether or not the currently loaded data has already been calibrated
#global$bearing is a data frame containing the calculated bearings
#global$triangulation is a data frame containing the trinagulated points
#global$keepalices is a data frame containing all keepalive signals
global$calibrated = FALSE
#global$extra_points <- list()


### observe and add data ###

# Add Data Button is pressed
observeEvent(input$add_data,{
  global$connections <- unique.data.frame(rbind(remote_connections(), global$connections))
  global$receivers <- unique(rbind(receiver_list(), global$receivers), fill=T)
  global$frequencies <- unique.data.frame(rbind(frequencies_list(), global$frequencies))
  global$calibration <- unique.data.frame(rbind(calibration_list(), global$calibration))
  global$map_markers <- unique.data.frame(rbind(map_markers(), global$map_markers))
  global$man_points <- unique.data.frame(rbind(man_points(), global$man_points))
  
  if (!is.null(bearings_list())) {
    if (!is.null(global$bearing) && nrow(global$bearing) > 0){
      global$bearing$bId <- global$bearing$bId*10
      bearings_list()[, bId:=bId*10+1]
    }
    global$bearing <- unique(rbindlist(list(bearings_list(), global$bearing)), by=names(global$bearing)[!names(global$bearing) == "bId"] )
  }
  
  if (!is.null(triangulations_list())) {
    if (!is.null(global$triangulation) && nrow(global$triangulation) > 0){
      global$triangulation$tId <- global$triangulation$tId*10
      global$triangulation$bearing_bIds <- laply(strsplit(global$triangulation$bearing_bIds, split="/", fixed = T), .fun = function(bs){ paste(as.numeric(bs) * 10, collapse = "/")})
      triangulations_list()[, tId:=tId*10+1]
      triangulations_list()[, bearing_bIds:= laply(strsplit(bearing_bIds, split="/", fixed = T), .fun = function(bs){ paste(as.numeric(bs) * 10+1, collapse = "/")})]
    }
    global$triangulation <- as.data.frame(unique(rbindlist(list(triangulations_list(), global$triangulation)), by=names(global$triangulation)[!names(global$triangulation) %in% c("bearing_bIds", "tId")] ))
  }
  if (!is.null(global$extra_points) && length(global$extra_points) > 0) {
    # Append new data
    #global$extra_points <- unique(c(global$extra_points, gpx_data()))
    combined_data <- c(global$extra_points, gpx_data())
    unique_data <- unique(combined_data)
    
    # Validate to ensure no duplicates are added
    if (!length(unique_data) == length(combined_data)) {
      showNotification(ui="Duplicate track(s) uploaded, only unique track(s) were added.", type = "warning")
    }
    global$extra_points <- unique_data
  } else {
    # If no existing data, just add new data
    global$extra_points <- gpx_data()
  }
  global$calibrated <- FALSE

  # if(input$data_type_input == "Data folder" && !is.null(local_logger_data())) {
  #   tmp<-local_logger_data()
  #   global$signals<-as.data.frame(unique(rbindlist(list(tmp,global$signals), fill=T)))
  #   global$keepalives<-as.data.frame(unique(rbindlist(list(extract_keepalives(tmp), global$keepalives), fill=T)))
  #   
  #   print(paste("Added",nrow(local_logger_data()),"points of data from local files."))
  # }
  # add signal data if either SQLite or Logger Files has been selected
  # if(input$data_type_input=="Logger Files" || input$data_type_input=="SQLite File" && !input$data_type_input == "Data folder"){
  if(input$data_type_input=="SQLite File"){
    tmp<-get_signals()
    if(is.null(tmp$freq_tag)){
      tmp$freq_tag<-as.character(NA)
    }
    if(!is.null(global$signals) && is.null(global$signals$freq_tag)){
      global$signals$freq_tag<-as.character(NA)
    }
    if(is.null(tmp$signal_bw)){
      tmp$signal_bw<-NA
    }
    if(!is.null(global$signals) && is.null(global$signals$signal_bw)){
      global$signals$signal_bw<-NA
    }
    if(is.null(tmp$td)){
      tmp$td<-NA
    }
    if(!is.null(global$signals) && is.null(global$signals$td)){
      global$signals$td<-NA
    }
    if(!is.null(tmp) && input$data_type_input != "SQLite File"){
        global$signals<-as.data.frame(unique(rbindlist(list(cbind(tmp,receiver = input$receiver_name_input, Name = input$station_name_input), global$signals), fill=T)))
        global$keepalives<-as.data.frame(unique(rbindlist(list(extract_keepalives(cbind(tmp,receiver = input$receiver_name_input, Name = input$station_name_input)), global$keepalives), fill=T)))
    }
    else {
        global$signals<-as.data.frame(unique(rbindlist(list(tmp, global$signals), fill=T)))
        for (file in input$SQLite_filepath[, "datapath"]) {
          con <- dbConnect(RSQLite::SQLite(), file)
          if (dbExistsTable(con, "rteu_calibrated")) {
            calibrated <- dbReadTable(con, "rteu_calibrated")
            global$calibrated <- (calibrated[1, 1] == 1)
          }
          # if (dbExistsTable(con, "rteu_keepalives")) {
          #   global$keepalives <- as.data.frame(unique(rbindlist(list(dbReadTable(con, "rteu_keepalives"), global$keepalives), fill=T)))
          # } else {
          #   global$keepalives<-as.data.frame(unique(rbindlist(list(extract_keepalives(tmp), global$keepalives), fill=T)))
          # }
          if (is.null(global$keepalives))
            global$keepalives<-data.frame(timestamp=as.POSIXct(character()), Name=character(), receiver=character(), Orientation=numeric(), td=numeric(), td_fctr=as.factor(character()))
          dbDisconnect(con)
        }
    }
  }
})

observeEvent(global$signals, ignoreNULL = T, ignoreInit = F, priority = 10, {
  isolate({
    if (!"sId" %in% names(global$signals))
      global$signals$sId <- as.numeric(NA)
    maxId <- max(global$signals$sId, na.rm=T)
    if (maxId < 0 )
      maxId <- 0
    global$signals$sId[is.na(global$signals$sId)] <- maxId + seq_along(global$signals$sId[is.na(global$signals$sId)])
  })
})

observe({
    input$clear_logger_data
    input$clear_logger_data_from_live
    global$signals <- NULL
})

observe({
    input$clear_receivers_data
    global$receivers <- NULL
})

observe({
    input$clear_frequencies_data
    global$frequencies <- NULL
})

observe({
    input$clear_bearings_data
    global$bearing <- NULL
})

observe({
    input$clear_frequencies_data
    global$triangulation <- NULL
})

observe({
    input$clear_connections_data
    input$clear_connections_data_from_live
    global$connections <- NULL
})

observe({
    input$clear_calibration_data
    global$calibration <- NULL
})

observe({
    input$clear_map_markers_data
    global$map_markers <- NULL
})

observe({
    input$clear_man_points_data
    global$man_points <- NULL
})

observe({
    input$clear_keepalive_data
    global$keepalives <- NULL
})

for (label in c(
  "Logger Data",
  "Antennas",
  "Remote Connections",
  "Frequencies",
  "Bearings",
  "Triangulations",
  "Calibration",
  "Map Markers",
  "Manual Positions",
  "Keepalives"
)) {
  addCssClass(selector = paste0("#data_tab_tabset a:contains('", label, "')"), class = "needed")
}


update_single_tab_title_colour <- function(data, label) {
    if (!is.null(data)) {
      addCssClass(selector = paste0("#data_tab_tabset a:contains('", label, "')"), class = "available")
    }
    else {
      removeCssClass(selector = paste0("#data_tab_tabset a:contains('", label, "')"), class = "available")
    }
}

observe({update_single_tab_title_colour(global$signals, "Logger Data")})
observe({update_single_tab_title_colour(global$receivers, "Antennas")})
observe({update_single_tab_title_colour(global$connections, "Remote Connections")})
observe({update_single_tab_title_colour(global$frequencies, "Frequencies")})
observe({update_single_tab_title_colour(global$bearing, "Bearings")})
observe({update_single_tab_title_colour(global$triangulation, "Triangulations")})
observe({update_single_tab_title_colour(global$calibration, "Calibration")})
observe({update_single_tab_title_colour(global$map_markers, "Map Markers")})
observe({update_single_tab_title_colour(global$man_points, "Manual Positions")})
observe({update_single_tab_title_colour(global$keepalives, "Keepalives")})

### get data stored in the data folder ###

# get remote connection info
remote_connections <- reactive({
  tmp<-NULL
    switch(input$data_type_input,
           "Data folder" = {
                tmp<-safe_read_excel_silent("data/RemoteConnections.xlsx")
                tmp<-tmp[, names(tmp)[names(tmp) %in% c("Name","Host","Database","Table","Port","User","Password")]]
           },
           "SQLite File" = {
              tmp <- NULL
              for (file in input$SQLite_filepath[, "datapath"]) {
                con <- dbConnect(RSQLite::SQLite(), file)
                if (dbExistsTable(con, "rteu_connections")) {
                    tmp <- rbind(tmp, dbReadTable(con, "rteu_connections"))
                }
                dbDisconnect(con)
              }
              tmp<-tmp[, names(tmp)[names(tmp) %in% c("Name","Host","Database","Table","Port","User","Password")]]
           },
           "Excel Files" = {
             if(input$excel_data_content=="Connections") {
               if(is.null(input$excel_filepath_remote)) {
                 return(NULL)
               }
               tmp<-safe_read_excel(input$excel_filepath_remote$datapath)
               if(is.null(tmp$Table)){
                 tmp$Table<-"signals"
               }
               tmp<-tmp[, names(tmp)[names(tmp) %in% c("Name","Host","Database","Table","Port","User","Password")]]
             }
           }
    )
  return(tmp)
})

frequencies_list <- reactive({
  tmp<-NULL
    switch(input$data_type_input,
            "Data folder" = {
                tmp<-safe_read_excel_silent("data/Frequencies.xlsx")
            },
           "SQLite File" = {
              for (file in input$SQLite_filepath[, "datapath"]) {
                con <- dbConnect(RSQLite::SQLite(), file)
                if (dbExistsTable(con, "rteu_freqs")) {
                    tmp <- rbind(tmp, dbReadTable(con, "rteu_freqs"))
                }
                dbDisconnect(con)
              }
              tmp <- unique(tmp)
              tmp
           },
           "Excel Files" = {
             if(input$excel_data_content=="Frequencies"){
               if(is.null(input$excel_filepath_frequencies))
                 return(NULL)
               tmp<-safe_read_excel(input$excel_filepath_frequencies$datapath)
             }
           }
    )
  return(tmp)
})

receiver_list <- reactive({
  tmp<-NULL
  switch(
    input$data_type_input,
    "Data folder" = {
      tmp<-safe_read_excel_silent("data/Antennas.xlsx")
      setDT(tmp)
    },
    "SQLite File" = {
      for (file in input$SQLite_filepath[, "datapath"]) {
        con <- dbConnect(RSQLite::SQLite(), file)
        #old db structure
        if (dbExistsTable(con, "rteu_antenna")) {
          tmp_data <- dbReadTable(con, "rteu_antenna")
          setDT(tmp_data)
          tmp <- rbind(tmp, tmp_data, fill=T)
        #new db structure
        } else if (dbExistsTable(con, "runs")) {
          tmp_data <- dbReadTable(con, "runs")
          setDT(tmp_data)
          tmp_data[, Name:=paste(hostname, device, orientation, sep = "_")]
          setnames(tmp_data,c("hostname", "latitude", "longitude", "orientation"), c("Station", "Latitude", "Longitude", "Orientation"))
          tmp_data <- unique(tmp_data, by = c("Name", "Station", "Latitude", "Longitude", "Orientation"))
          tmp <- rbind(tmp, tmp_data[,.(Name, Station, Latitude, Longitude, Orientation)], fill=T)
        }
        dbDisconnect(con)
      }
      tmp <- unique(tmp)
      # tmp
    },
    "Excel Files" = {
      if(input$excel_data_content=="Antennas"){
        if(is.null(input$excel_filepath_receivers))
          return(NULL)
        
        tmp<-safe_read_excel(input$excel_filepath_receivers$datapath)
        setDT(tmp)
      }
    }
  )
  if (!is.null(tmp)) {
    if ("Name" %in% names(tmp))
      tmp[, Name:=trimws(Name, "right")]
    if ("Station" %in% names(tmp))
      tmp[, Station:=trimws(Station, "right")]
  }
  return(tmp)
})

calibration_list <- reactive({
    tmp <- NULL

        switch(input$data_type_input,
            "Data folder" = {
                tmp <- safe_read_excel_silent("data/Calibration.xlsx")
            },
            "SQLite File" = {
              for (file in input$SQLite_filepath[, "datapath"]) {
                con <- dbConnect(RSQLite::SQLite(), file)
                if (dbExistsTable(con, "rteu_calibration")) {
                  tmp <- rbind(tmp, dbReadTable(con, "rteu_calibration"))
                }
                dbDisconnect(con)
              }
              tmp <- unique(tmp)
              tmp
            },
            "Excel Files" = {
                if (input$excel_data_content == "Calibration" && !is.null(input$excel_filepath_calibration)) {
                    tmp <- safe_read_excel(input$excel_filepath_calibration$datapath)
                }
            }
        )
    tmp
})

bearings_list  <- reactive({
  tmp <- data.table()
  req(input$data_type_input == "SQLite File")
  if (nrow(input$SQLite_filepath) > 99)
    stop("Can't open more than 99 files at the same time")
    
  blist <- alply(.data=cbind(input$SQLite_filepath, num=seq_len(nrow(input$SQLite_filepath))), .margins = 1, .expand = F, .fun = function(file) {
    b <- NULL
    con <- dbConnect(RSQLite::SQLite(), file$datapath)
    if (dbExistsTable(con, "rteu_bearings")) {
      b <- dbReadTable(con, "rteu_bearings")
      setDT(b)
      b[, bId:=bId*100+file$num]
    }
    dbDisconnect(con)
    b
  })
  
  blist <- rbindlist(blist, fill=T)
  if (nrow(blist) > 0){
    blist[, timestamp:=as.POSIXct(timestamp, tz = "UTC", origin="1970-01-01 00:00:00 UTC")]
    blist[, time_matched:=as.POSIXct(time_matched, tz = "UTC", origin="1970-01-01 00:00:00 UTC")]
    return(unique(blist))
  } else
    return(NULL)
})

triangulations_list  <- reactive({
  tmp <- data.table()
  req(input$data_type_input == "SQLite File")
  if (nrow(input$SQLite_filepath) > 99)
    stop("Can't open more than 99 files at the same time")
    
  tlist <- alply(.data=cbind(input$SQLite_filepath, num=seq_len(nrow(input$SQLite_filepath))), .margins = 1, .expand = F, .fun = function(file) {
    t <- NULL
    con <- dbConnect(RSQLite::SQLite(), file$datapath)
    if (dbExistsTable(con, "rteu_triangulations")) {
      t <- dbReadTable(con, "rteu_triangulations")
      setDT(t)
      t[, tId:=tId*100+file$num]
      t[, bearing_bIds:=laply(strsplit(bearing_bIds, split="/", fixed = T), .fun = function(bs){ paste(as.numeric(bs) * 100 + 1, collapse = "/")})]
    }
    dbDisconnect(con)
    t
  })
  
  tlist <- rbindlist(tlist, fill=T)
  if (nrow(tlist) > 0){
    tlist[, timestamp:=as.POSIXct(timestamp, tz = "UTC", origin="1970-01-01 00:00:00 UTC")]
    return(unique(tlist))
  } else 
    return(NULL)
})

gpx_data <- reactive({
  mytrack<-NULL
  switch(input$data_type_input,
         "Miscellaneous" = {
           if(input$misc_type_input == "GPX" && !is.null(input$coordinates_filepath)) {
             mygpx <- tryCatch({
             readGPX(input$coordinates_filepath$datapath, waypoints = FALSE)
             }, error = function(e){
               
               # Read the GPX file as text lines
               gpx_lines <- readLines(input$coordinates_filepath$datapath)
               
               # Filter out lines with comments
               clean_lines <- gpx_lines[!grepl("<!--|-->", gpx_lines)]
               
               # Combine cleaned lines
               clean_gpx_string <- paste(clean_lines, collapse = "\n")
               
               # Temporarily save cleaned data to process it
               temp_file <- tempfile(fileext = ".gpx")
               writeLines(clean_gpx_string, temp_file)
               mygpx_inner  <- tryCatch({
                 readGPX(temp_file)
               }, error = function(e) {
                 return(NULL)
               })
               unlink(temp_file)
               return(mygpx_inner)
             })
             convert_timestamps <- function(track_segment) {
               # Assuming 'time' is the column with the timestamps
               if ("time" %in% names(track_segment)) {
                 track_segment$timestamp <- as.POSIXct(track_segment$time, format="%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
               } else {
                 warning("Time column not found in the provided track segment.")
               }
               return(track_segment)
             }
             filename <- input$coordinates_filepath$name
             converted_tracks <- lapply(mygpx$tracks, function(track) {
               tmp<-lapply(track, convert_timestamps)
               names<-lapply(tmp, function(t) {
                 paste0(
                   format.POSIXct(x=min(t$timestamp, na.rm=T), format = "%Y-%m-%d %H-%M", tz = "UTC"),
                   " (",
                   ifelse(nchar(filename) > 10, paste0(substring(filename, 1, 10), "..."), filename),
                   ")"
                 )
               })
               names(tmp) <- names
               tmp
             })
             mytrack <- converted_tracks
             mytrack$extensions<-NULL
             
             
           }
           if(input$misc_type_input == "KML" && !is.null(input$coordinates_filepath)) {
             mytrack<-readOGR(input$coordinates_filepath$datapath)
             #maybe variables need to be renamed
             mytrack<-data.frame(Lat=mytrack@coords[,1],Lon=mytrack@coords[,2],timestamp=mytrack$Name)
           }
           if(input$misc_type_input == "KMZ" && !is.null(input$coordinates_filepath)) {
             filename<-unzip(input$coordinates_filepath$datapath)
             mytrack<-readOGR(filename)
             file.remove(filename)
             #maybe variables need to be renamed
             mytrack<-data.frame(Lat=mytrack@coords[,1],Lon=mytrack@coords[,2],timestamp=mytrack$Name)
           }
           if(input$misc_type_input == "readOGR" && !is.null(input$coordinates_filepath)) {
             spdf<-readOGR(input$coordinates_filepath$datapath)
             mytrack<-spTransform(spdf,CRS("+init=epsg:4326"))
           }
           if(input$misc_type_input == "readcsv" && !is.null(input$coordinates_filepath)) {
             spdf<-read.csv(input$coordinates_filepath$datapath)
             mytrack<-spdf
           }
         }
  )
  mytrack
})

map_markers <- reactive({
  markers <-NULL
  switch(input$data_type_input,
    "Data folder" = {
      markers<-safe_read_excel_silent("data/MapMarkers.xlsx")
    },
    "Excel Files" = {
      if (input$excel_data_content == "Map Markers" && !is.null(input$excel_filepath_map_markers)) {
        markers <- safe_read_excel(input$excel_filepath_map_markers$datapath)
      }
    },
    "SQLite File" = {
        for (file in input$SQLite_filepath[, "datapath"]) {
            con <- dbConnect(RSQLite::SQLite(), file)
            if (dbExistsTable(con, "rteu_map_markers")) {
                markers <- rbind(markers, dbReadTable(con, "rteu_map_markers"))
            }
            dbDisconnect(con)
        }
        markers <- unique(markers)
    }
  )
  return(markers)
})

man_points <- reactive({
  points <- NULL
  switch(input$data_type_input,
    # "Data folder" = {
    #   markers<-safe_read_excel_silent("data/MapMarkers.xlsx")
    # },
    "Excel Files" = {
      if (input$excel_data_content == "Manual Positions" && !is.null(input$excel_filepath_man_points)) {
        points <- safe_read_excel(input$excel_filepath_man_points$datapath)
        setDT(points)
        setnames(points, c("Time", "Individual", "Longitude", "Latitude"),  c("timestamp", "freq_tag", "longitude", "latitude"), skip_absent = T)
        points <- points[, .(timestamp, freq_tag, longitude, latitude)]
        points <- unique(points)
        points[, id:=seq_len(.N)]
        points[, timestamp := as.POSIXct(timestamp, origin="1970-01-01", tz="UTC")]
      }
    },
    "SQLite File" = {
        for (file in input$SQLite_filepath[, "datapath"]) {
            con <- dbConnect(RSQLite::SQLite(), file)
            if (dbExistsTable(con, "rteu_man_points")) {
                points <- rbind(points, dbReadTable(con, "rteu_man_points"))
            }
            dbDisconnect(con)
        }
      if (!is.null(points)){
        setDT(points)
        points <- unique(points)
        points[, id:=seq_len(.N)]
        points[, timestamp := as.POSIXct(timestamp, origin="1970-01-01", tz="UTC")]
      }
    }
  )
  return(points)
})

local_logger_data <- reactive({
    tmp <- NULL
    if (input$data_type_input == "Data folder") {
       tmp<-read_logger_folder()
    }
    return(tmp)
})

### read Signal data from files ###

get_signals <- reactive({
    switch(input$data_type_input,
            # 'Data folder' = {
            #     read_logger_folder()
            # },
            # 'Logger Files' = {
            #   data <- NULL
            #   for (file in input$logger_filepath[, "datapath"]) {
            #     tmp <- read_logger_data(file)
            #     if(!is.null(tmp)){
            #       data <- rbind(data, read_logger_data(file))
            #     }
            #   }
            #   data
            # },
            'SQLite File' = {
                data <- NULL
                for (file in input$SQLite_filepath[, "datapath"]) {
                    con <- dbConnect(RSQLite::SQLite(), file)
                    # old data structure
                    if (dbExistsTable(con, "rteu_logger_data")) {
                        data <- rbindlist(list(data, dbReadTable(con, "rteu_logger_data")), fill=T)
                    # new data structure
                    } else if(dbExistsTable(con, "signals")){
                      query <- "SELECT signals.*, hostname, device, orientation, latitude, longitude FROM signals INNER JOIN runs ON signals.run = runs.id"
                      tmp_data <- dbGetQuery(con, query)
                      setDT(tmp_data)
                      tmp_data[, receiver := paste(hostname, device, str_pad(orientation, 3, side="left", pad="0"), sep = "_")]
                      tmp_data[, device:=NULL]
                      # fix column names 
                      setnames(tmp_data,c("hostname"), c("Station"))
                      data <- rbindlist(list(data, tmp_data), fill = T)
                    }
                    dbDisconnect(con)
                }
                if (!is.null(data)) {
                    data <- unique(data)
                    data$timestamp <- as.POSIXct(data$timestamp, tz = "UTC", origin="1970-01-01 00:00:00 UTC")
                    if (!is.null(data$station)){
                      data$Name <- data$station
                    }
                }
                data
            }
    )
  data
})

preview_content <- reactive({
    switch(input$data_type_input,
        "Excel Files" = {
            switch(input$excel_data_content,
                Antennas = {
                    tmp <- receiver_list()
                },
                Frequencies = {
                    tmp <- frequencies_list()
                },
                Connections = {
                    tmp <- remote_connections()
                },
                Calibration = {
                    tmp <- calibration_list()
                },
                "Map Markers" = {
                    tmp <- map_markers()
                },
                "Manual Positions" = {
                    tmp <- man_points()[, .("Time"=timestamp, "Individual"=freq_tag, "Longitude"=longitude, "Latitude"=latitude)]
                }
            )
        },
        # "Logger Files" = {
        #     tmp <- get_signals()
        # },
        "SQLite File" = {
            files_count <- nrow(input$SQLite_filepath)

            tmp <- NULL
            if (!is.null(files_count) && files_count > 0 ) {
                for (file_id in files_count) {
                    file <- input$SQLite_filepath[file_id, ]

                    con <- dbConnect(RSQLite::SQLite(), file$datapath)
                    tables <- dbListTables(con)

                    rows <- NULL
                    for (table in tables) {
                        query <- paste("SELECT count(*) FROM ", table)
                        result <- dbGetQuery(con, query)

                        rows <- rbind(rows, data.frame(table, result, file$name))
                    }

                    tmp <- rbind(tmp, rows)
                    dbDisconnect(con)
                }
            }

            if (!is.null(tmp)) {
                colnames(tmp) <- c("Table", "Entries count", "File")
            }

            tmp
        },
        "Data folder" = {
            tmp <- NULL

            files <- c("Antennas", "Frequencies", "RemoteConnections", "Calibration", "MapMarkers")

            for (file in files) {
                filepath = paste0("data/", file, ".xlsx")

                if (file.exists(filepath)) {
                    # data read once and thrown away, ok since small files
                    data <- safe_read_excel_silent(filepath)

                    if (is.null(data)) {
                        row <- c(file, "yes", "error while reading", filepath)
                    }
                    else {
                        row <- c(file, "yes", nrow(data), filepath)
                    }
                }
                else {
                    row <- c(file, "no", 0, filepath)
                }

                tmp <- rbind(tmp, row)
            }

            # logger_files <- get_logger_files()

            # if (length(logger_files) > 0) {
            #     row <- c("Logger data", "yes", length(logger_files), "/data/logger/")
            # }
            # else {
            #     row <- c("Logger data", "no", 0, "/data/logger/")
            # }
            # 
            # tmp <- rbind(tmp, row)

            if (!is.null(tmp)) {
                colnames(tmp) <- c("Information type", "Found", "Count", "Filepath")
            }

            tmp
        },
        
        "Miscellaneous" = {
          input_type <- isolate(input$misc_type_input)
           tmp <- NULL
           if (any(input$misc_type_input == c("GPX","KML","KMZ"))) {
             tmp <- gpx_data()  
             extract_info <- function(tmp) {
               track_df <- bind_rows(tmp)
               data.frame(
                 "Starts at" = min(track_df$timestamp),
                 "Ends at" = max(track_df$timestamp),
                 "Min longitude" = min(track_df$lon),
                 "Max longitude" = max(track_df$lon),
                 "Min latitude" = min(track_df$lat),
                 "Max latitude" = max(track_df$lat),
                 "Label" = names(tmp)
               )
             }
             track_info_df <- do.call(rbind, lapply(tmp, extract_info))
             track_info_unique <- unique(track_info_df)
             track_info_unique
           }
        })
})

### render Tables ###
output$data_tab_preview <- renderDataTable({
    shiny::validate(need(preview_content(), "Please select a file."))
    preview_content()
}, options = list(pageLength = 10), rownames=F)

output$data_tab_logger_table <- renderDataTable({
  shiny::validate(need(global$signals, "Please provide signals data."))
  global$signals
}, options = list(pageLength = 10), colnames=c("antenna"="receiver"), rownames=F)

output$data_tab_freq_table <- renderDataTable({
  shiny::validate(need(global$frequencies, "Please provide frequency data file."))
  global$frequencies
}, options = list(pageLength = 10), rownames=F)

output$data_tab_bearings_table <- renderDataTable({
  shiny::validate(need(global$bearing, "Please provide or calculate bearings data."))
  global$bearing[,.(timestamp, station, angle, freq_tag, strength)]
}, options = list(pageLength = 10), rownames=F)

output$data_tab_triangulations_table <- renderDataTable({
  shiny::validate(need(global$triangulation, "Please provide or calculate triangulations data."))
  global$triangulation[,c("timestamp", "freq_tag", "pos.X", "pos.Y", "tri_stations")]
}, options = list(pageLength = 10), rownames=F)

output$data_tab_calibration_table <- renderDataTable({
  shiny::validate(need(global$calibration, "Please provide calibration data file."))
  global$calibration
}, options = list(pageLength = 10), rownames=F)

output$data_tab_map_markers_table <- renderDataTable({
  shiny::validate(need(global$map_markers, "Please provide map markers data file."))
  global$map_markers
}, options = list(pageLength = 10), rownames=F)

output$data_tab_man_points_table <- renderDataTable({
    shiny::validate(need(global$man_points, "Please provide manual positions data file."))
    as.data.table(global$man_points)[, .("Time"=timestamp, "Individual"=freq_tag, "Longitude"=longitude, "Latitude"=latitude)]
  }, 
  options = list(
    pageLength = 10,
    columns = list(
      list(title="Time"),
      list(title="Individual"),
      list(title="Longitude"),
      list(title="Latitude")
    )
  ), 
  rownames=F
)

output$data_tab_antennae_table <- renderDataTable({
  shiny::validate(need(global$receivers, "Please provide antenna data file."))
  global$receivers
}, options = list(pageLength = 10), rownames=F)

output$data_tab_remote_con_table <- renderDataTable({
  shiny::validate(need(global$connections, "Please provide remote connection data file."))
  global$connections
}, options = list(pageLength = 10), rownames=F)

output$data_tab_keepalive_table <- renderDataTable({
  shiny::validate(need(global$keepalives, "Please provide keepalive data."))
  global$keepalives
}, options = list(pageLength = 10), rownames=F)

observeEvent(input$SQLite_filepath, ignoreNULL = T, {
  if (!is.null(input$SQLite_filepath$datapath))
    enable(id='add_data')
})

observe({
  enable(id = "add_data")
  switch (input$data_type_input,
    "SQLite File" = {
      if (is.null(input$SQLite_filepath))
        disable(id="add_data")
    },
    "Excel Files" = {
      switch(input$excel_data_content,
        "Antennas" = {
          if (is.null(input$excel_filepath_receivers))
            disable(id="add_data")
        },
        "Frequencies" = {
          if (is.null(input$excel_filepath_frequencies))
            disable(id="add_data")
        },
        "Connections" = {
          if (is.null(input$excel_filepath_remote))
            disable(id="add_data")
        },
        "Calibration" = {
          if (is.null(input$excel_filepath_calibration))
            disable(id="add_data")
        },
        "Map Markers" = {
          if (is.null(input$excel_filepath_map_markers))
            disable(id="add_data")
        },
        "Manual Positions" = {
          if (is.null(input$excel_filepath_man_points))
            disable(id="add_data")
        }
      )
    },
    "Miscellaneous" = {
      if (is.null(input$coordinates_filepath))
        disable(id="add_data")
    }
  )
})
