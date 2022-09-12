############ tabFilters.R ############
tabPanel("Results",
  fluidRow(
    column(
      width=3,
      selectInput("choose_plot","Choose Plot",
        choices = c("Time-Strength-Antenna-Station",
                    "Time-Strength-Antenna-Station-Freq",
                    "Time-Freq-Strength-Station",
                    "Time-Strength-Frequency-Station",
                    "Time-Temperature-Station-Frequency",
                    "Time-TD-Station-Frequency"
        ),
        selected="Time-Strength-Antenna-Station"
      )
    ),
    column(
      width=4,
      conditionalPanel(
        condition = "input.choose_plot=='Time-TD-Station-Frequency'",
        sliderInput("results_slider_tdRange", "Set y-range limits", min=0, max=20, value=c(0,4.5), step=0.1, width="100%")
      )
    ),
    column(
      width=2,
      actionButton("minus_one_day","Minus one day"),
      actionButton("plus_one_day","Plus one day")
    ),
    column(
      width=3,
      br(),
      h4(textOutput("plot_x_y"))
    )
  ),
  div(
    plotOutput("facet",hover = "plot_hover", height="100%"),
    style="height: calc(100vh - 260px)"
  ),
  fluidRow(
    column(
      width=10,
      offset = 1,
      sliderInput(
        "slider_datetime", 
        "",
        min=as.POSIXlt("2017-09-24 00:00:00", "UTC"),
        max=as.POSIXlt("2017-09-29 23:59:59", "UTC"),
        value=c(
          as.POSIXlt("2017-09-24 00:00:00", "UTC"),
          as.POSIXlt("2017-09-29 23:59:59", "UTC")
        ),
        timezone = "UTC",
        dragRange = TRUE,
        animate = TRUE,
        width = "100%",
        timeFormat="%d-%m %H:%M:%S"
      )
    )
  )
)

