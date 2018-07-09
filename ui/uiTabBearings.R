############ tabBearings.R ############
tabPanel("Bearings",
         tabsetPanel(
           tabPanel("Calibration",
                    checkboxInput("correct_signal_strength","Correct Signal Strengths"),
                    dataTableOutput("cal_factors"),
                    numericInput("dBLoss","dB between to neighbouring antennas",value=4),
                    numericInput("angle_sep","angle between to neighbouring antennas",value=90)
                    ),
           tabPanel("DoA Table",
                    dataTableOutput("doa")
                    ),
           tabPanel("DoA Table",
                    plotOutput("doa_plot")
                    )
         )
)
