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
             checkboxInput("app_live_mod", "Live Mode"),
             actionButton("load_mysql_data","Load once"),
             numericInput("live_update_intervall", "Data update intervall:", 5, 1, 60),
             numericInput("live_last_points", "Number of entries to read", 50, 1, 10000),
             h5("Filters:"),
             br(),
             br(),
             checkboxInput("query_filter_duration", "Duration"),
             sliderInput("filter_duration", "Duration",min=0.003,max = 0.04, value=c(0.01,0.025)),
             checkboxInput("query_filter_max_signal", "Signal Strength"),
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
             tabPanel("Help",
                      "1) First select the data source on the right",
                      br(),
                      "2) Check the preview window if it is the correct data"
                      )
           ))
         ))
