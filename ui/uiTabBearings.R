############ tabBearings.R ############
tabPanel("Bearings",
         tabsetPanel(
           tabPanel("Calibration",
                    actionButton("calibrate_signal_strength","Calibrate"),
                    checkboxInput("correct_signal_strength","Correct Signal Strengths"),
                    sliderInput("spar_in","Spar for smooth",0,1,value=0.1),
                    dataTableOutput("cal_factors"),
                    numericInput("dBLoss","dB between to neighbouring antennas",value=14),
                    numericInput("angle_sep","angle between to neighbouring antennas",value=90)
                    ),
           tabPanel("DoA Table 3",
                    dataTableOutput("doa3")
           ),
           tabPanel("DoA Plot 3",
                    plotOutput("doa_plot3")
           ),
           tabPanel("Smoothed",
                    plotOutput("smoothed_curves")
           )
         )
)
