tabPanel("Live Data",
         sidebarLayout(
           sidebarPanel(
             uiOutput('con_tags'),
             numericInput("live_last_points", "Number of entries to read (0 to get all)", 50, min=0),
             airDatepickerInput("datetime_filter", label = "Load signals recorded after", timepicker=TRUE, addon="none"),
             actionButton("connect_mysql","Connect to DBs"),
             actionButton("load_mysql_data","Load Data"),
             checkboxInput("app_live_mode", "Live Mode"),
             conditionalPanel("input.app_live_mode",
                              numericInput("live_update_interval", "Data update interval:", 15, 2, 600)
                              ),
             tags$b("DBs-Filters:"),
             br(),
               # disabled(
                 checkboxInput("query_filter_freq", "Frequencies", value = FALSE#)
                ),
             conditionalPanel(cond = "input.query_filter_freq",
                radioButtons("query_filter_frequency_type",
                    choices = c("Single", "Multiple"),
                    label = "Frequencies selection: ",
                  selected = "Single"
                ),
               numericInput("query_filter_frequency_error", label="Max frequency deviation", value=5, min=0),
                conditionalPanel(cond = "input.query_filter_frequency_type == 'Single'",
                    numericInput("query_filter_single_frequency",
                        label = "kHz",
                        value = 150175,
                        min = 0
                    )
                ),
               conditionalPanel(cond = "input.query_filter_frequency_type == 'Multiple'",
                 # disabled(
                   selectizeInput("query_filter_multiple_frequency", 
                     multiple=TRUE, 
                     label=strong("Choose tags"), 
                     choices = NULL, 
                     selected = NULL
                    )
                 # )
               )
             ),
             # disabled(
               checkboxInput("check_sql_duration", "Duration", value = FALSE#)
             ),
             conditionalPanel("input.check_sql_duration",
                              sliderInput("query_filter_duration", "Duration [ms]",min=3,max = 40, value=c(1,25))
                              ),
             # disabled(
               checkboxInput("check_sql_strength", "Strength", value = FALSE#)
             ),
             conditionalPanel("input.check_sql_strength",
                              sliderInput("query_filter_strength", "Strength",min=-100,max = 0, value=c(-85,-10))
             ),
             disabled(checkboxInput("check_sql_tag", "Tags", value = FALSE)),
             conditionalPanel(cond = "input.check_sql_tag",
               selectInput("query_filter_tag", "Tags", choices=NULL),
             ),
             tags$b("Add new connection:"),
             br(),
             checkboxInput(
               "show_add_connection_panel", "Show manual connection input panel"
             ),
             conditionalPanel("input.show_add_connection_panel",
                  disabled(checkboxInput(
                    "global_db_hostname", "If a single global DB is used"
                    , value = FALSE
                  )),
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
                   "MySQL_db",
                   "Enter Database Name",
                   "rteu"
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
                      plotOutput("live_tab_keepalive_plot", height = "calc( 100vh - 120px)")
             ),
             tabPanel("Help",
                      "1) First select the data source on the right",
                      br(),
                      "2) Check the preview window if it is the correct data"
                      )
           ))
         ))
