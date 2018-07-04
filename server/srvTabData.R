############ srvTabData.R ############


### reactive UIs ###
data_tab_data_type <- reactive({
  if(input$data_type_input=="SQLite File"){
    forUI<-fileInput(
      "data_logger_input_sqlite",
      "Previous Session (SQLite)",
      multiple = FALSE,
      accept = NULL,
      width = NULL
    )
  }
  if(input$data_type_input=="Excel Files"){
    forUI<-tagList(
      radioButtons(
        "data_type_input",
        choices = c("Logger", "Receivers","Frequncies","Connections"),
        label = "Add the following data",
        selected = "Logger"
      ),
      fileInput(
        "data_logger_input_csv",
        "Select the according file",
        multiple = FALSE,
        accept = NULL,
        width = NULL
      )
    )
  }
  return(forUI)
})

output$data_tab_data_type <- renderUI({
  data_tab_data_type()
})

### get data stored in the data folder ### 

remote_connections <- reactive({
  tmp<-NULL
  if(input$read_data_folder){
    tmp<-read_excel("data/RemoteConnections.xlsx", sheet = 1)
  }
  return(tmp)
})

frequencies_list <- reactive({
  tmp<-NULL
  if(input$read_data_folder){
    tmp<-read_excel("data/Frequencies.xlsx", sheet = 1)
  }
  return(tmp)
})

antenna_list <- reactive({
  tmp<-NULL
  if(input$read_data_folder){
    tmp<-read_excel("data/Antennas.xlsx", sheet = 1)
  }
  return(tmp)
})

output$data_tab_logger_table <- renderDataTable({
  validate(need(logger_data(), "Please provide logger data file."))
  logger_data()
}, options = list(pageLength = 10))

output$data_tab_get_data_table <- renderDataTable({
  validate(need(get_data(), "Please provide logger data file."))
  get_data()
}, options = list(pageLength = 10))

output$data_tab_freq_table <- renderDataTable({
  validate(need(frequencies_list(), "Please provide frequency data file."))
  frequencies_list()
}, options = list(pageLength = 10))

output$data_tab_antennae_table <- renderDataTable({
  validate(need(antenna_list(), "Please provide antenna data file."))
  antenna_list()
}, options = list(pageLength = 10))

output$data_tab_remote_con_table <- renderDataTable({
  validate(need(remote_connections(), "Please provide remote connection data file."))
  remote_connections()
}, options = list(pageLength = 10))