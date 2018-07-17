############ srvTabData.R ############

### define reactiveValues to store all data ###

global <- reactiveValues()

# maybe predefine content of tables

#global$connections to store information how to connect to dbs
#global$receivers to list all used receivers with the orientation and location
#global$frequencies to list the transmitters porperties like frequency, etc.
#global$calibration to list the sensitivy of each receiver
#global$signals is a the dataframe to store all received signals

### observe and add data ###

# Add Data Button is pressed
observeEvent(input$add_data,{
  global$connections<-unique.data.frame(rbind(remote_connections(),global$connections))
  global$receivers<-unique.data.frame(rbind(receiver_list(),global$receivers))
  global$frequencies<-unique.data.frame(rbind(frequencies_list(),global$frequencies))
  global$calibration <- unique.data.frame(rbind(calibration_list(), global$calibration))

  if(input$data_type_input == "Data folder" && !is.null(local_logger_data())) {
    global$signals<-unique.data.frame(rbind(local_logger_data(),global$signals))
    print(paste("Added",nrow(local_logger_data()),"points of data from local files."))
  }
  # add signal data if either SQLite or Logger Files has been selected
  if(input$data_type_input=="Logger Files"||input$data_type_input=="SQLite File"&&!input$data_type_input == "Data folder"){
    tmp<-get_signals()
    if(!is.null(tmp) && input$data_type_input != "SQLite File"){
        global$signals<-unique.data.frame(rbind(cbind(tmp,receiver = input$receiver_name_input, Name = input$station_name_input),global$signals))
    }
    else {
        global$signals<-unique.data.frame(rbind(tmp, global$signals))
    }
  }
})

js$mark_invalid("Frequencies")
js$mark_invalid("Receivers")
js$mark_invalid("Connections")
js$mark_invalid("Logger data")

update_single_tab_title_colour <- function(data, label) {
    if (!is.null(data)) {
        js$mark_valid(label)
    }
    else {
        js$mark_invalid(label)
    }
}

observe({update_single_tab_title_colour(global$signals, "Logger data")})
observe({update_single_tab_title_colour(global$receivers, "Receivers")})
observe({update_single_tab_title_colour(global$connections, "Connections")})
observe({update_single_tab_title_colour(global$frequencies, "Frequencies")})

### get data stored in the data folder ###

# get remote connection info
remote_connections <- reactive({
  tmp<-NULL
    switch(input$data_type_input,
           "Data folder" = {
                tmp<-safe_read_excel_silent("data/RemoteConnections.xlsx")
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
              tmp <- unique(tmp)
              tmp
           },
           "Excel Files" = {
             if(input$excel_data_content=="Connections") {
               if(is.null(input$excel_filepath_remote)) {
                 return(NULL)
               }
               tmp<-safe_read_excel(input$excel_filepath_remote$datapath)
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
        switch(input$data_type_input,
           "Data folder" = {
               tmp<-safe_read_excel_silent("data/Antennas.xlsx")
           },
           "SQLite File" = {
              for (file in input$SQLite_filepath[, "datapath"]) {
                con <- dbConnect(RSQLite::SQLite(), file)
                if (dbExistsTable(con, "rteu_antenna")) {
                    tmp <- rbind(tmp, dbReadTable(con, "rteu_antenna"))
                }
                dbDisconnect(con)
              }
              tmp <- unique(tmp)
              tmp
           },
           "Excel Files" = {
             if(input$excel_data_content=="Receivers"){
               if(is.null(input$excel_filepath_receivers))
                 return(NULL)

               tmp<-safe_read_excel(input$excel_filepath_receivers$datapath)
             }
           }
    )
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

local_logger_data <- reactive({
  tmp <- NULL
  if (input$data_type_input == "Data folder"){
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
              #data <- unique(data) - done twice... the add button does it again
              data
            },
            'SQLite File' = {
                data <- NULL
                for (file in input$SQLite_filepath[, "datapath"]) {
                    con <- dbConnect(RSQLite::SQLite(), file)
                    if (dbExistsTable(con, "rteu_logger_data")) {
                        data <- rbind(data, dbReadTable(con, "rteu_logger_data"))
                    }
                    dbDisconnect(con)
                }
                if (!is.null(data)) {
                    data <- unique(data)
                    data$timestamp <- as.POSIXct(data$timestamp, tz = "UTC", origin = "1970-01-01")
                }
                data
            }
    )
  data
})


### render Tables ###

output$data_tab_preview <- renderDataTable({
    switch(input$data_type_input,
        "Excel Files" = {
            switch(input$excel_data_content,
                Receivers = {
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

            files <- c("Antennas", "Frequencies", "RemoteConnections", "Calibration")

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

            if (!is.null(tmp)) {
                colnames(tmp) <- c("Information type", "Found", "Rows count", "Filepath")
            }

            tmp
        }
    )
    tmp
}, options = list(pageLength = 10))

output$data_tab_logger_table <- renderDataTable({
  validate(need(global$signals, "Please provide logger data file."))
  global$signals[c("Name", "timestamp", "samples", "duration", "signal_freq", "signal_bw", "max_signal")]
}, options = list(pageLength = 10))

output$data_tab_freq_table <- renderDataTable({
  validate(need(global$frequencies, "Please provide frequency data file."))
  global$frequencies
}, options = list(pageLength = 10))

output$data_tab_antennae_table <- renderDataTable({
  validate(need(global$receivers, "Please provide antenna data file."))
  global$receivers
}, options = list(pageLength = 10))

output$data_tab_remote_con_table <- renderDataTable({
  validate(need(global$connections, "Please provide remote connection data file."))
  global$connections
}, options = list(pageLength = 10))
