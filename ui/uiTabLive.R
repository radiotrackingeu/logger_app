tabPanel("Live Data",
         sidebarLayout(
           sidebarPanel(
             uiOutput('con_tags'),
             numericInput("live_last_points", "Number of entries to read (0 to get all)", 50, min=0),
             airDatepickerInput("datetime_filter", label = "Load signals recorded after", timepicker=TRUE),
             actionButton("connect_mysql","Connect to DBs"),
             actionButton("load_mysql_data","Load Data"),
             checkboxInput("app_live_mode", "Live Mode"),
             conditionalPanel("input.app_live_mode",
                              numericInput("live_update_interval", "Data update interval:", 15, 2, 600)
                              ),
             tags$b("DBs-Filters:"),
             br(),
             checkboxInput("check_sql_duration", "Duration"),
             conditionalPanel("input.check_sql_duration",
                              sliderInput("query_filter_duration", "Duration",min=0.003,max = 0.04, value=c(0.01,0.025))
                              ),
             checkboxInput("check_sql_strength", "Strength"),
             conditionalPanel("input.check_sql_strength",
                              sliderInput("query_filter_strength", "Strength",min=0,max = 100, value=c(15,90))
             ),
             checkboxInput("query_filter_freq", "Frequencies"),
             conditionalPanel(cond = "input.query_filter_freq",
                radioButtons("query_filter_frequency_type",
                    choices = c("Multiple", "Single"),
                    label = "Frequencies selection: "
                ),
                conditionalPanel(cond = "input.query_filter_frequency_type == 'Single'",
                    numericInput("query_filter_single_frequency",
                        label = "kHz",
                        value = 150175,
                        min = 0
                    )
                )
             ),
             tags$b("Add new connection:"),
             br(),
             checkboxInput(
               "show_add_connection_panel", "Show manual connection input panel"
             ),
             conditionalPanel("input.show_add_connection_panel",
                  checkboxInput(
                    "global_db_hostname", "If a single global DB is used"
                  ),
                  textInput(
                    "MySQL_name",
                    "Connection name",
                    "Manual connection"
                  ),
                  textInput(
                    "MySQL_host",
                    "Enter Host Name",
                    "192.168.1.1"
                  ),
                  textInput(
                   "MySQL_table",
                   "Enter Table Name",
                   "signals"
                  ),
                  numericInput(
                    "MySQL_port",
                    "Enter Port",
                    3306
                  ),
                  textInput(
                    "MySQL_user",
                    "Enter User Name",
                    "rteu"
                  ),
                  passwordInput(
                    "MySQL_pw",
                    "Enter Password",
                    "rteuv2!"
                  ),
                  actionButton(
                    "add_manual_connection",
                    "Add remote connection"
                  ),
                  br(),
                  br()
             )
           ),
           mainPanel(
             tabsetPanel(
             id = "live_tab_tabset",
             tabPanel("List of Connections",
                      dataTableOutput("live_tab_remote_entries_table"),
                      actionButton("clear_connections_data_from_live", "Clear table")
              ),
             tabPanel("List of Data",
                      dataTableOutput("live_tab_mysql_data"),
                      actionButton("clear_logger_data_from_live", "Clear table")
             ),
             tabPanel("Plot of Keepalives",
                      plotOutput("live_tab_keepalive_plot")
             ),
             tabPanel("Help",
                      "1) First select the data source on the right",
                      br(),
                      "2) Check the preview window if it is the correct data"
                      )
           ))
         ))
