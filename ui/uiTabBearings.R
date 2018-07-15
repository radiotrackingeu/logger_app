############ tabBearings.R ############
tabPanel("Bearings",
         tabsetPanel(
           tabPanel("Calibration",
                    actionButton("calibrate_signal_strength","Calibrate"),
                    checkboxInput("correct_signal_strength","Correct Signal Strengths"),
                    sliderInput("spar_in","Spar for smooth",0,1,value=0.2),
                    dataTableOutput("cal_factors"),
                    numericInput("dBLoss","dB between to neighbouring antennas",value=4),
                    numericInput("angle_sep","angle between to neighbouring antennas",value=90)
                    ),
           tabPanel("DoA Table 1",
                    dataTableOutput("doa")
                    ),
           tabPanel("DoA Plot 1",
                    plotOutput("doa_plot")
                    ),
           tabPanel("DoA Table 2",
                    dataTableOutput("doa2")
           ),
           tabPanel("DoA Plot 2",
                    plotOutput("doa_plot2")
           )
         )
)
