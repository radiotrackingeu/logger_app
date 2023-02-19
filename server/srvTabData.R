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
#global$bearing is a data frame containing the calculted bearings
#global$triangulation is a data frame containing the trinagulated points
#global$keepalices is a data frame containing all keepalive signals
global$calibrated = FALSE


### observe and add data ###

# Add Data Button is pressed
observeEvent(input$add_data,{
  global$connections<-unique.data.frame(rbind(remote_connections(),global$connections))
  global$receivers<-unique.data.frame(rbind(receiver_list(),global$receivers))
  global$frequencies<-unique.data.frame(rbind(frequencies_list(),global$frequencies))
  global$calibration <- unique.data.frame(rbind(calibration_list(), global$calibration))
  global$map_markers <- unique.data.frame(rbind(map_markers(), global$map_markers))
  global$extra_points <- gpx_data()
  global$calibrated <- FALSE

  if(input$data_type_input == "Data folder" && !is.null(local_logger_data())) {
    tmp<-local_logger_data()
    global$signals<-unique.data.frame(rbind(tmp,global$signals))
    global$keepalives<-unique.data.frame(rbind(extract_keepalives(tmp), global$keepalives))
    
    print(paste("Added",nrow(local_logger_data()),"points of data from local files."))
  }
  # add signal data if either SQLite or Logger Files has been selected
  if(input$data_type_input=="Logger Files"||input$data_type_input=="SQLite File"&&!input$data_type_input == "Data folder"){
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
        global$signals<-unique.data.frame(rbind(cbind(tmp,receiver = input$receiver_name_input, Name = input$station_name_input),global$signals))
        global$keepalives<-unique.data.frame(rbind(extract_keepalives(cbind(tmp,receiver = input$receiver_name_input, Name = input$station_name_input)), global$keepalives))
    }
    else {
        global$signals<-unique.data.frame(rbind(tmp, global$signals))
        for (file in input$SQLite_filepath[, "datapath"]) {
          con <- dbConnect(RSQLite::SQLite(), file)
          if (dbExistsTable(con, "rteu_calibrated")) {
            calibrated <- dbReadTable(con, "rteu_calibrated")
            global$calibrated <- (calibrated[1, 1] == 1)
          }
          # if (dbExistsTable(con, "rteu_keepalives")) {
          #   global$keepalives <- unique.data.frame(rbind(extract_keepalives(dbReadTable(con, "rteu_keepalives")), global$keepalives))
          # } else {
          #   global$keepalives<-unique.data.frame(rbind(extract_keepalives(tmp), global$keepalives))
          # }
          if (is.null(global$keepalives))
            global$keepalives<-data.frame(timestamp=as.POSIXct(character()), Name=character(), receiver=character(), Orientation=numeric(), td=numeric(), td_fctr=as.factor(character()))
          dbDisconnect(con)
        }
    }
  }
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
    input$clear_keepalive_data
    global$keepalives <- NULL
})

js$mark_invalid("Frequencies")
js$mark_invalid("Receivers")
js$mark_invalid("Connections")
js$mark_invalid("Logger data")
js$mark_invalid("Calibration")
js$mark_invalid("Map Markers")
js$mark_invalid("Keepalives")

update_single_tab_title_colour <- function(data, label) {
    if (!is.null(data)) {
        js$mark_valid(label)
    }
    else {
        js$mark_invalid(label)
    }
}

observe({update_single_tab_title_colour(global$signals, "Logger data")})
observe({update_single_tab_title_colour(global$receivers, "Antennas")})
observe({update_single_tab_title_colour(global$connections, "Connections")})
observe({update_single_tab_title_colour(global$frequencies, "Frequencies")})
observe({update_single_tab_title_colour(global$calibration, "Calibration")})
observe({update_single_tab_title_colour(global$map_markers, "Map Markers")})
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
    },
    "SQLite File" = {
      for (file in input$SQLite_filepath[, "datapath"]) {
        con <- dbConnect(RSQLite::SQLite(), file)
        if (dbExistsTable(con, "rteu_antenna")) {
          tmp_data <- dbReadTable(con, "rteu_antenna")
          tmp <- rbind(tmp, tmp_data)
        }
        dbDisconnect(con)
      }
      tmp <- unique(tmp)
      tmp
    },
    "Excel Files" = {
      if(input$excel_data_content=="Antennas"){
        if(is.null(input$excel_filepath_receivers))
          return(NULL)
        
        tmp<-safe_read_excel(input$excel_filepath_receivers$datapath)
      }
    }
  )
  if (!is.null(tmp)) {
    setDT(tmp)
    tmp[, Name:=trimws(Name, "right")]
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

gpx_data <- reactive({
  mytrack<-NULL
  switch(input$data_type_input,
         "Miscellaneous" = {
           if(input$misc_type_input == "GPX" && !is.null(input$coordinates_filepath)) {
             mygpx <- readGPX(input$coordinates_filepath$datapath, waypoints = FALSE)
             mytrack <- mygpx$tracks[[1]]$'NA'
             mytrack$timestamp<-as.POSIXct(mytrack$time,format="%Y-%m-%dT%H:%M:%S.000Z")
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

local_logger_data <- reactive({
    tmp <- NULL
    if (input$data_type_input == "Data folder") {
       tmp<-read_logger_folder()
    }
    return(tmp)
})

### read Signal data from files ###

get_signals <- reactive({
    switch (input$data_type_input,
            'Data folder' = {
                read_logger_folder()
            },
            'Logger Files' = {
              data <- NULL
              for (file in input$logger_filepath[, "datapath"]) {
                tmp <- read_logger_data(file)
                if(!is.null(tmp)){
                  data <- rbind(data, read_logger_data(file))
                }
              }
              data
            },
            'SQLite File' = {
                data <- NULL
                for (file in input$SQLite_filepath[, "datapath"]) {
                    con <- dbConnect(RSQLite::SQLite(), file)
                    if (dbExistsTable(con, "rteu_logger_data")) {
                        data <- rbindlist(list(data, dbReadTable(con, "rteu_logger_data")), fill=T)
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
                }
            )
        },
        "Logger Files" = {
            tmp <- get_signals()
        },
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

            logger_files <- get_logger_files()

            if (length(logger_files) > 0) {
                row <- c("Logger data", "yes", length(logger_files), "/data/logger/")
            }
            else {
                row <- c("Logger data", "no", 0, "/data/logger/")
            }

            tmp <- rbind(tmp, row)

            if (!is.null(tmp)) {
                colnames(tmp) <- c("Information type", "Found", "Count", "Filepath")
            }

            tmp
        },
        "Miscellaneous" = {
           tmp <- NULL
           if (any(input$misc_type_input == c("GPX","KML","KMZ"))) {
               tmp <- gpx_data()
           }
        })
})

### render Tables ###
output$data_tab_preview <- renderDataTable({
    validate(need(preview_content(), "Please select a file."))
    preview_content()
}, options = list(pageLength = 10), rownames=F)

output$data_tab_logger_table <- renderDataTable({
  validate(need(global$signals, "Please provide logger data file."))
  global$signals
}, options = list(pageLength = 10), colnames=c("antenna"="receiver"), rownames=F)

output$data_tab_freq_table <- renderDataTable({
  validate(need(global$frequencies, "Please provide frequency data file."))
  global$frequencies
}, options = list(pageLength = 10), rownames=F)

output$data_tab_calibration_table <- renderDataTable({
  validate(need(global$calibration, "Please provide calibration data file."))
  global$calibration
}, options = list(pageLength = 10), rownames=F)

output$data_tab_map_markers_table <- renderDataTable({
  validate(need(global$map_markers, "Please provide map markers data file."))
  global$map_markers
}, options = list(pageLength = 10), rownames=F)

output$data_tab_antennae_table <- renderDataTable({
  validate(need(global$receivers, "Please provide antenna data file."))
  global$receivers
}, options = list(pageLength = 10), rownames=F)

output$data_tab_remote_con_table <- renderDataTable({
  validate(need(global$connections, "Please provide remote connection data file."))
  global$connections
}, options = list(pageLength = 10), rownames=F)

output$data_tab_keepalive_table <- renderDataTable({
  validate(need(global$keepalives, "Please provide logger data file."))
  global$keepalives
}, options = list(pageLength = 10), rownames=F)

observeEvent(input$SQLite_filepath, ignoreNULL = T, {
  if (!is.null(input$SQLite_filepath$datapath))
    enable(id='add_data')
})

observeEvent(input$logger_filepath, ignoreNULL = T, {
  if (!is.null(input$logger_filepath$datapath))
    enable(id='add_data')
})