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
  # add connections
  global$connections<-unique.data.frame(rbind(remote_connections(),global$connections))
  # add receivers/antennas
  global$receivers<-unique.data.frame(rbind(receiver_list(),global$receivers))
  # add frequencies
  global$frequencies<-unique.data.frame(rbind(frequencies_list(),global$frequencies))
  # add signal data if either SQLite or Logger Files has been selected
  if(input$data_type_input=="Logger Files"||input$data_type_input=="SQLite File"&&!input$read_data_folder){
    global$signals<-unique.data.frame(rbind(cbind(get_signals(),receiver = input$receiver_name_input, Name = input$station_name_input),global$signals))
  }
})

### get data stored in the data folder ###

# get remote connection info
remote_connections <- reactive({
  tmp<-NULL
  if(input$read_data_folder){
    tmp<-read_excel("data/RemoteConnections.xlsx", sheet = 1)
  }else{
    switch(input$data_type_input,
           "SQLite File" = {
             if (is.null(input$SQLite_filepath))
               return(NULL)
             con <- dbConnect(RSQLite::SQLite(), input$SQLite_filepath$datapath)
             if (dbExistsTable(con, "rteu_connections")) {
               tmp <- dbReadTable(con, "rteu_connections")
             }
             dbDisconnect(con)
           },
           "Excel Files" = {
             if(input$excel_data_content=="Connections") {
               if(is.null(input$excel_filepath_remote))
                 return(NULL)
               tmp<-read_excel(input$excel_filepath_remote$datapath, sheet = 1)
             }
           }
    )
  }
  return(tmp)
})

frequencies_list <- reactive({
  tmp<-NULL
  if(input$read_data_folder){
    tmp<-read_excel("data/Frequencies.xlsx", sheet = 1)
  }else{
    switch(input$data_type_input,
           "SQLite File" = {
             if (is.null(input$SQLite_filepath))
               return(NULL)
             con <- dbConnect(RSQLite::SQLite(), input$SQLite_filepath$datapath)
             if (dbExistsTable(con, "rteu_freqs")) {
               tmp <- dbReadTable(con, "rteu_freqs")
             }
             dbDisconnect(con)
           },
           "Excel Files" = {
             if(input$excel_data_content=="Frequencies"){
               if(is.null(input$excel_filepath_frequencies))
                 return(NULL)
               tmp<-read_excel(input$excel_filepath_frequencies$datapath, sheet = 1)
             }
           }
    )
  }
  return(tmp)
})

receiver_list <- reactive({
    tmp<-NULL
    if(input$read_data_folder){
        tmp<-read_excel("data/Antennas.xlsx", sheet = 1)
    }
    else {
        switch(input$data_type_input,
           "SQLite File" = {
             if (is.null(input$SQLite_filepath))
               return(NULL)
             con <- dbConnect(RSQLite::SQLite(), input$SQLite_filepath$datapath)
             if (dbExistsTable(con, "rteu_antenna")) {
               tmp <- dbReadTable(con, "rteu_antenna")
             }
             dbDisconnect(con)
           },
           "Excel Files" = {
             if(input$excel_data_content=="Receivers"){
               print(input$excel_filepath_receivers$datapath)
               if(is.null(input$excel_filepath_receivers))
                 return(NULL)
               tmp<-read_excel(input$excel_filepath_receivers$datapath, sheet = 1)
             }
           }
    )
  }
  return(tmp)
})
### read Signal data from files ###

get_signals <- reactive({
  switch (input$data_type_input,
          'Logger Files' = {
            inFile <- input$logger_filepath
            if (is.null(inFile))
              return(NULL)
            data <- read_logger_data(inFile$datapath)
            if (is.null(data)) return(NULL)
          },
          'SQLite File' = {
            inFile <- input$SQLite_filepath
            if (is.null(inFile))
              return(NULL)
            # open db
            con <- dbConnect(RSQLite::SQLite(), inFile$datapath)
            if(!dbExistsTable(con, "rteu_logger_data")) {
              print("wrong sqlite db selected")
              dbDisconnect(con)
              return(NULL)
            }
            data <- dbReadTable(con, "rteu_logger_data")
            data$timestamp <- as.POSIXct(data$timestamp, tz = "UTC", origin = "1970-01-01")
            dbDisconnect(con)
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
           }
    )
  }
  if(input$data_type_input=="Logger Files"){
    tmp <- get_signals()
  }
  if(input$data_type_input=="SQLite File"){
    #overview of proterties of file or sub tabs to show content?
    tmp <- get_signals()
  }
  validate(need(tmp, "Please provide file"))
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
