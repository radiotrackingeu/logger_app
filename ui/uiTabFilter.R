############ tabFilter.R ############
tabPanel("Filter",
  fluidRow(
    column(
      9,
      column(
        4,
        checkboxInput("filter_length", strong("Signal length"), value = FALSE),
        sliderInput(
          "signal_length",
          "",
          min = 0.0005,
          max = 0.050,
          value = c(0.01, 0.03)
        )
      ),
      column(
        4,
        checkboxInput("filter_strength", strong("Signal Strength"), value = FALSE),
        sliderInput(
          "signal_strength",
          "",
          min = 0,
          max = 100,
          value = c(15, 95)
        )
      ),
      column(
        4,
        checkboxInput("filter_bw", strong("Bandwidth"), value = FALSE),
        sliderInput(
          "signal_bw",
          "",
          min = 500,
          max = 50000,
          value = c(2000, 8000)
        )
      ),

      tabsetPanel(
        id = "filter_plotTabs",
        type = "tabs",
        tabPanel("Frequency",
          div(
            style="height: calc(100vh - 280px)",
            plotOutput("histo", hover = "plot_freq_hover", height="100%"))
        ),
        tabPanel("Duration",
          div(
            style="height: calc(100vh - 280px)",
            plotOutput("histo_length", hover = "histo_length_hover", height="100%")
          )
        ),
        tabPanel("Signal Strength",
          div(
            style="height: calc(100vh - 280px)",
            plotOutput("histo_strength", hover = "histo_strength_hover", height="100%")
          )
        ),
        tabPanel("Signal Bandwidth",
          div(
            style="height: calc(100vh - 280px)",
            plotOutput("histo_bandwidth", hover = "histo_bandwidth_hover", height="100%")
          )
        )
      )
    ),
    column(
      3,
      dateRangeInput("filter_for_dates", "Choose dates"),
      radioButtons(
        "filter_type",
        strong("Filter type:"),
        c("all", "Multiple frequencies", "Custom frequency")
      ),
      conditionalPanel(
        condition = 'input.filter_type == "Custom frequency"',
        numericInput("single_freq", "Enter a frequency", value = 150175),
        tags$span(style = "font-weight:bold", "For temperature curve a*e^b:"),
        splitLayout(
          numericInput("single_freq_temp_a", "Coefficient a", value = 20.307),
          numericInput("single_freq_temp_b", "Coefficient b", value = 0.0408)
        )
      ),
      conditionalPanel(condition = 'input.filter_type == "Multiple frequencies"',
        uiOutput("freq_tags")),
      sliderInput(
        "freq_error",
        "Frequency Error (kHz):",
        min = 0.1,
        max = 30,
        value = 2,
        step = 0.1
      ),
      checkboxInput("filter_interval", strong("Signal Interval Filter"), value = FALSE),
      sliderInput(
        "signal_interval",
        "Signal Period in sec:",
        min = 0.5,
        max = 8,
        value = c(0.8, 1.2)
      ),
      selectInput(
        "input_select_receiver",
        "Select Antenna(s)",
        choices = NULL,
        multiple = TRUE,
        selectize = TRUE
      ),
      selectInput(
        "input_select_station",
        "Select Station(s)",
        choices = NULL,
        multiple = TRUE,
        selectize = TRUE
      ),
      textOutput("total_counts"),
      h4(textOutput("freq_hover"))
    )
  ))
