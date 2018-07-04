############ tabData.R ############
tabPanel("File Input",
         sidebarLayout(
           sidebarPanel(
             checkboxInput("read_data_folder", "read data stored in the apps data-folder"),
             radioButtons(
               "data_type_input",
               choices = c("SQLite File", "Excel Files"),
               label = "Add static data using:",
               selected = "SQLite File"
             ),
             uiOutput('data_tab_data_type'),
             actionButton(
               "add_data", 
               h4("      Add Data      ")
             ),
             checkboxInput("show_whole_table","Show whole table")
           ),
           mainPanel(
             tabsetPanel(
             tabPanel("Preview of upload",
                      dataTableOutput("data_tab_get_data_table")),
             tabPanel("All Logger Data",
                      dataTableOutput("data_tab_logger_table")),
             tabPanel("Antennas and Receivers",
                      dataTableOutput("data_tab_antennae_table")),
             tabPanel("Remote Connections",
                      dataTableOutput("data_tab_remote_con_table")),
             tabPanel("Frequencies used",
                      dataTableOutput("data_tab_freq_table")),
             tabPanel("Help",
                      "1) First select the data source on the right",
                      br(),
                      "2) Check the preview window if it is the correct data"
                      )
           ))
         ))
