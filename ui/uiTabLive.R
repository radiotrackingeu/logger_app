tabPanel("Live Data",
         sidebarLayout(
           sidebarPanel(
             radioButtons(
               "live_data_number",
               choices = c("Multiple", "Single"),
               label = "Add live data:",
               selected = "Multiple"
             ),
             conditionalPanel("input.live_data_number == 'Single'",
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
                    "connect_to_db",
                    "Add remote connection"
                  ),
                  br(),
                  br()
             ),
             conditionalPanel("input.live_data_number == 'Multiple'",
                 uiOutput('con_tags')
             ),
             numericInput("live_last_points", "Number of entries to read", 50, 1, 10000),
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
             checkboxInput("query_filter_freq", "Frequencies")
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
             tabPanel("List of Keepalives",
                      dataTableOutput("live_tab_keepalive")
             ),
             tabPanel("Help",
                      "1) First select the data source on the right",
                      br(),
                      "2) Check the preview window if it is the correct data"
                      )
           ))
         ))
