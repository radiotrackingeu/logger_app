############ tabBearings.R ############
tabPanel("Bearings",
         tabsetPanel(
           tabPanel("Auto Calibration",
                    uiOutput("calibration_state_warning"),
                    actionButton("calibrate_signal_strength","Calibrate"),
                    checkboxInput("correct_signal_strength_auto","Correct Signal Strengths"),
                    sliderInput("spar_in","Spar for smooth",0,1,value=0.1),
                    dataTableOutput("cal_factors"),
                    numericInput("dBLoss","dB between two neighbouring antennas",value=14),
                    numericInput("angle_sep","angle between two neighbouring antennas",value=90)
                    ),
           tabPanel("Manual Calibration",
                    checkboxInput("correct_signal_strength_manu","Correct Signal Strengths"),
                    uiOutput("correction_list"),
                    actionButton("change_manu","Change manually")
           ),
           tabPanel("Life DoA",
                    plotOutput("polar_output")
           ),
           tabPanel("DoA Table",
                    dataTableOutput("doa")
           ),
           tabPanel("DoA Plot",
                    plotOutput("doa_plot"),
                    plotOutput("smoothed_curves")
           )
         )
)
