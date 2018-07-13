############ tabFilters.R ############
tabPanel("Results",
         column(3,
                selectInput("select_x","Choose x-Axis", choices = c("timestamp","max_signal","signal_freq","receiver","Name","freq_tag"),selected="timestamp"),
                sliderInput("spar_in","Spar for smooth",0,1,value=0.2)
                ),
         column(3,
                selectInput("select_y","Choose y-Axis", choices = c("timestamp","max_signal","signal_freq","receiver","Name","freq_tag"),selected="max_signal")
         ),
         column(3,
                selectInput("select_col","Choose Colour", choices = c("timestamp","max_signal","signal_freq","receiver","Name","freq_tag"),selected="receiver")
         ),
         column(3,
                selectInput("select_facet","Choose Facet", choices = c("timestamp","max_signal","signal_freq","receiver","Name","freq_tag"),selected="Name")
         ),
         plotOutput("facet"),
         br(),
         br(),
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
                            timeFormat="%d-%m <br> %H:%M:%S")
         ),
         column(1)
)

