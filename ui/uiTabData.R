tabPanel("File Input",
         sidebarLayout(
           sidebarPanel(
             radioButtons(
               "data_type_input",
               choices = c("SQLite File", "Excel Files", "Logger Files", "Data folder", "Miscellaneous"),
               label = "Add data using:",
               selected = "SQLite File"
             ),
             conditionalPanel(
               condition = "input.data_type_input == 'SQLite File'",
               h6("Add data stored in previous sessions"),
               fileInput(
                 "SQLite_filepath",
                 "Upload data",
                 multiple = TRUE,
                 accept = NULL,
                 width = NULL
               )
             ),
             conditionalPanel(
               condition = "input.data_type_input == 'Logger Files'",
               h6("Add data stored in previous sessions"),
               fileInput(
                 "logger_filepath",
                 "Upload data",
                 multiple = TRUE,
                 accept = NULL,
                 width = NULL
               ),
               textInput("receiver_name_input","Please enter receiver name", value = "receiver_1"),
               textInput("station_name_input","Please enter receiver name", value = "station_1")
             ),
             conditionalPanel(
               condition = "input.data_type_input == 'Excel Files'",
               radioButtons("excel_data_content",
                            choices = c("Receivers", "Frequencies", "Connections", "Calibration", "Map Markers"),
                            label = "Add following data:"
               ),
               conditionalPanel(
                 condition = "input.excel_data_content == 'Receivers'",
                 h6("Information about each receiver used"),
                 fileInput(
                   "excel_filepath_receivers",
                   "",
                   multiple = FALSE,
                   accept = c(".xlsx", ".xls"),
                   width = NULL
                 )
               ),
               conditionalPanel(
                 condition = "input.excel_data_content == 'Frequencies'",
                 h6("Information about used frequencies"),
                 fileInput(
                   "excel_filepath_frequencies",
                   "",
                   multiple = FALSE,
                   accept = c(".xlsx", ".xls"),
                   width = NULL
                 )
               ),
               conditionalPanel(
                 condition = "input.excel_data_content == 'Connections'",
                 h6("Information about remote connections"),
                 fileInput(
                   "excel_filepath_remote",
                   "",
                   multiple = FALSE,
                   accept = c(".xlsx", ".xls"),
                   width = NULL
                 )
               ),
               conditionalPanel(
                 condition = "input.excel_data_content == 'Calibration'",
                 h6("Calibration of receiver's sensitivity"),
                 fileInput(
                   "excel_filepath_calibration",
                   "",
                   multiple = FALSE,
                   accept = c(".xlsx", ".xls"),
                   width = NULL
                 )
               ),
               conditionalPanel(
                 condition = "input.excel_data_content == 'Map Markers'",
                 h6("Map markers with comments"),
                 fileInput(
                   "excel_filepath_map_markers",
                   "",
                   multiple = FALSE,
                   accept = c(".xlsx", ".xls"),
                   width = NULL
                 )
               )
             ),
             conditionalPanel(
                condition = "input.data_type_input == 'Miscellaneous'",
                radioButtons(
                  "misc_type_input",
                  choices = c("GPX"),
                  label = "File type:"
                ),
                conditionalPanel(
                    condition = "input.misc_type_input == 'GPX'",
                    h6("GPX data to compare"),
                    fileInput(
                       "gpx_filepath",
                       "",
                       multiple = FALSE,
                       accept = c(".gpx"),
                       width = NULL
                    )
                )
            ),
             actionButton(
               "add_data",
               h4("    Add Data    ")
             )
           ),
           mainPanel(
             tabsetPanel(
             id = "data_tab_tabset",
             tabPanel("Preview of upload",
                      dataTableOutput("data_tab_preview")),
             tabPanel("All Logger Data",
                      dataTableOutput("data_tab_logger_table"),
                      actionButton("clear_logger_data", "Clear table")
             ),
             tabPanel("Antennas and Receivers",
                      dataTableOutput("data_tab_antennae_table"),
                      actionButton("clear_receivers_data", "Clear table")
             ),
             tabPanel("Remote Connections",
                      dataTableOutput("data_tab_remote_con_table"),
                      actionButton("clear_connections_data", "Clear table")
             ),
             tabPanel("Frequencies used",
                      dataTableOutput("data_tab_freq_table"),
                      actionButton("clear_frequencies_data", "Clear table")
             ),
             tabPanel("Calibration",
                      dataTableOutput("data_tab_calibration_table"),
                      actionButton("clear_calibration_data", "Clear table")
             ),
             tabPanel("Help",
                      "1) First select the data source on the right",
                      br(),
                      "2) Check the preview window if it is the correct data"
                      )
           ))
         ))
