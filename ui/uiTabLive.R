############ tabData.R ############
tabPanel("Live Data",
         sidebarLayout(
           sidebarPanel(
             
             radioButtons(
               "live_data_number",
               choices = c("Multiple", "Single"),
               label = "Add live data:",
               selected = "Multiple"
             ),
             uiOutput('single_multiple_con_tags'),
             numericInput("live_last_points", "Number of entries to read", 50, 1, 10000),
             actionButton("load_mysql_data","Connect to DBs"),
             checkboxInput("app_live_mod", "Live Mode"),
             conditionalPanel("input.app_live_mod",
                              numericInput("live_update_intervall", "Data update intervall:", 5, 1, 60)
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
             tabPanel("List of Connections",
                      dataTableOutput("live_tab_remote_entries_table")
                      ),
             tabPanel("List of Data",
                      dataTableOutput("live_tab_mysql_data")
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
