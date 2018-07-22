############ tabFilters.R ############
tabPanel("Results",
         selectInput("choose_plot","Choose Plot", 
                     choices = c("Time-Strength-Receiver-Station",
                                 "Time-Temperature-Station-Frequency Tag",
                                 "Time-Strength-Frequency-Station"
                                 ),
                     selected="Time-Strength-Receiver-Station"),
         plotOutput("facet"),
         column(1),
         column(10,
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
         ),
         column(1)
)

