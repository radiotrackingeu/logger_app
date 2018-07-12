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
  global$calibration <- unique.data.frame(rbind(calibrations_list(), global$calibration))

  if(input$data_type_input == "Data folder" && !is.null(local_logger_data())) {
    global$signals<-unique.data.frame(rbind(local_logger_data(),global$signals))
    print(paste("Added",nrow(local_logger_data()),"points of data from local files."))
  }
  # add signal data if either SQLite or Logger Files has been selected
  if(input$data_type_input=="Logger Files"||input$data_type_input=="SQLite File"&&!input$data_type_input == "Data folder"){
    global$signals<-unique.data.frame(rbind(cbind(get_signals(),receiver = input$receiver_name_input, Name = input$station_name_input),global$signals))
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
                tmp<-safe_read_excel("data/RemoteConnections.xlsx")
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
                tmp<-safe_read_excel("data/Frequencies.xlsx")
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
               tmp<-safe_read_excel("data/Antennas.xlsx")
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

calibrations_list <- reactive({
    tmp <- NULL

        switch(input$data_type_input,
            "Data folder" = {
                tmp <- safe_read_excel("data/Calibrations.xlsx")
            },
            # TODO SQLite File
            "Excel Files" = {
                if (input$excel_data_content == "Calibrations" && !is.null(input$excel_filepath_calibrations)) {
                    tmp <- safe_read_excel(input$excel_filepath_calibrations$datapath)
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
                data <- rbind(data, read_logger_data(file))
              }
              data <- unique(data)
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
  if(input$data_type_input=="Excel Files"){
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
           Calibrations = {
             tmp <- calibrations_list()
           }
    )
  }
  if(input$data_type_input=="Logger Files"){
    tmp <- get_signals()
  }
  if(input$data_type_input=="SQLite File"){
    #overview of properties of file or sub tabs to show content?
    tmp <- get_signals()
  }
  if (input$data_type_input == "Data folder") {
    tmp <- receiver_list()
  }
  tmp
}, options = list(pageLength = 10))

output$data_tab_logger_table <- renderDataTable({
  validate(need(global$signals, "Please provide logger data file."))
  global$signals
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
