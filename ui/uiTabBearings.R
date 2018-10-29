############ tabBearings.R ############
tabPanel("Bearings",
         tabsetPanel(
           tabPanel("Auto Calibration",
                    actionButton("start_doa","Calcualte Bearings"),
                    sliderInput("slider_angles_allowed","Angles allowed",0,180,c(10,170)),
                    numericInput("time_error_inter_station","Time Error Inter Station",0.2,0,30, step = 0.05),
                    selectInput("doa_option_approximation","Choose DoA Method",c("automatic","linear","arccos")),
                    selectInput("time_matching_method","Choose Time Match Method", c("spline","tm")),
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
