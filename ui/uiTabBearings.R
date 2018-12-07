############ tabBearings.R ############
tabPanel("Bearings",
         actionButton("start_doa","Calculate Bearings", style="margin-bottom:25px"),
         tabsetPanel(
           tabPanel("Settings",
                    selectInput("doa_option_approximation","Choose DoA Method",c("automatic","linear","arccos")),
                    selectInput("time_matching_method","Choose Time Match Method", c("tm","spline")),
                    sliderInput("spar_in","Spar for smooth",0,1,value=0.1),
                    numericInput("intra_station_time_error","Time Difference between the antennas",value=0.5),
                    numericInput("dBLoss","dB between two neighbouring antennas",value=14),
                    numericInput("angle_sep","angle between two neighbouring antennas",value=90),
                    sliderInput("min_doa_antennas","Choose minimum number of antennas",1,12,1)
           ),
           tabPanel("Auto Calibration",
                    uiOutput("calibration_state_warning"),
                    actionButton("calibrate_signal_strength","Calibrate",style="margin:10px"),
                    checkboxInput("correct_signal_strength_auto","Correct Signal Strengths"),
                    dataTableOutput("cal_factors")
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
