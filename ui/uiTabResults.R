############ tabFilters.R ############
tabPanel("Results",
         fluidRow(
           column(3,selectInput("choose_plot","Choose Plot", 
                                choices = c("Time-Strength-Receiver-Station",
                                            "Time-Temperature-Station-Frequency Tag",
                                            "Time-Strength-Frequency-Station"
                                ),
                                selected="Time-Strength-Receiver-Station")
                  ),
           column(3,offset = 6,
                  br(),
                  h4(textOutput("plot_x_y"))
                  )
         ),
                
                plotOutput("facet",hover = "plot_hover"),
         fluidRow(
           column(10,offset = 1,
                  sliderInput("slider_datetime", "",
                              min=as.POSIXlt("2017-09-24 00:00:00", "UTC"),
                              max=as.POSIXlt("2017-09-29 23:59:59", "UTC"),
                              value=c(as.POSIXlt("2017-09-24 00:00:00", "UTC"),
                                      as.POSIXlt("2017-09-29 23:59:59", "UTC")
                              ),
                              timezone = "UTC",
                              dragRange = TRUE,
                              animate = TRUE,
                              width = "100%",
                              timeFormat="%d-%m %H:%M:%S")
           )
         )
)

