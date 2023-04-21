tabPanel("Command",
  sidebarLayout(
    sidebarPanel(
      uiOutput('stat_ui_select'),
      selectInput(inputId = "stat_select_service", label = "Select Service", choices = c("Loggers"="rteu", "WebRX"="owrx", "VPN"="vpn", "Database"="mariadb"), multiple = T),
      selectInput(inputId = "stat_select_cmd", label = "Select Command", choices = c("status", "restart", "start", "stop")),
      actionButton("stat_send_cmd","Send Command")#,
      # br(),
      # tags$b("Add new connection:"),
      # br(),
      # checkboxInput(
        # "stat_show_add_connection_panel", "Show manual connection input panel"
      # ),
      # conditionalPanel("input.stat_show_add_connection_panel",
      #   textInput(
      #     inputId="stat_MySQL_name",
      #     "Connection name",
      #     "Manual connection"
      #   ),
      #   textInput(
      #     inputId="stat_MySQL_host",
      #     "Enter Host Name",
      #     "192.168.1.1"
      #   ),
      #   textInput(
      #     inputId="stat_MySQL_db",
      #     "Enter Database Name",
      #     "rteu"
      #   ),
      #   textInput(
      #     inputId="stat_MySQL_table",
      #     "Enter Table Name",
      #     "signals"
      #   ),
      #   numericInput(
      #     inputId="stat_MySQL_port",
      #     "Enter Port",
      #     3306
      #   ),
      #   textInput(
      #     inputId="stat_MySQL_user",
      #     "Enter User Name",
      #     "rteu"
      #   ),
      #   passwordInput(
      #     inputId="stat_MySQL_pw",
      #     "Enter Password",
      #     "rteuv2!"
      #   ),
      #   actionButton(
      #     inputId="stat_add_manual_connection",
      #     "Add remote connection"
      #   ),
      #   br(),
      #   br()
      # )
    ),
    mainPanel(
      tabsetPanel(
        id = "status_overview",
        tabPanel("Overview",
          dataTableOutput("status_tab_overview"),
          # actionButton("clear_connections_data_from_live", "Clear table")
        ),
        # tabPanel("Status Details",
          # dataTableOutput("status_tab_details"),
          # actionButton("clear_logger_data_from_live", "Clear table")
        # ),
        # tabPanel("Plot of Keepalives",
        #   plotOutput("live_tab_keepalive_plot")
        # ),
        tabPanel("Help",
          "1) Select one or multiple connections.",
          br(),
          "2) Select the service you want to affect.",
          br(),
          "3) Select the command to execute. Only the status command can be executed for multiple services at once.",
          br(),
          "4) Double check your selections!",
          br(),
          "5) Hit 'Send Command'"
        )
      ))
  ))
