############ tabFilter.R ############
tabPanel("Filter",
         fluidRow(
           column(9,
                  column(4,
                         checkboxInput("filter_length",strong("Signal length"),value = FALSE),
                         sliderInput("signal_length",
                                     "",
                                     min = 0.0005,
                                     max = 0.050,
                                     value = c(0.01,0.03))
                  ),
                  column(4,
                         checkboxInput("filter_strength",strong("Signal Strength"),value = FALSE),
                         sliderInput("signal_strength",
                                     "",
                                     min = 0,
                                     max = 100,
                                     value = c(15,95))
                  ),
                  column(4,
                         checkboxInput("filter_bw",strong("Bandwidth"),value = FALSE),
                         sliderInput("signal_bw",
                                     "",
                                     min = 500,
                                     max = 50000,
                                     value = c(2000,8000))
                  ),
                  tabsetPanel(type = "tabs",
                              tabPanel("Frequency",
                                       plotOutput("histo")
                              ),
                              tabPanel("Duration",
                                       plotOutput("histo_length")
                              ),
                              tabPanel("Signal Strength",
                                       plotOutput("histo_strength")
                              ),
                              tabPanel("Signal Bandwidth",
                                       plotOutput("histo_bandwidth")
                              )#,
                              #tabPanel("Results",plotOutput("facet")),
                              #tabPanel("DoA",dataTableOutput("angle")),
                              #tabPanel("DoA Plot",plotOutput("doa_plot")),
                              #tabPanel("Temperature",dataTableOutput("timediffs")),
                              #tabPanel("T Plot",{
                              #  plotOutput("timediffs_plot")
                              #})
                  )
           ),
           column(3,
                  checkboxInput("filter_one_freq",strong("Single Frequency kHz"),value = FALSE),
                  conditionalPanel(condition='input.filter_one_freq && !input.filter_freq',
                                   numericInput("single_freq", "", value = 150175)
                                   ),
                  checkboxInput("filter_freq",strong("Multiple Frequency Filter"),value = FALSE),
                  conditionalPanel(condition='!input.filter_one_freq && input.filter_freq',
                                   selectInput("choose_tag", "Choose Tag", choices = NULL)
                  ),
                  sliderInput("freq_error",
                              "Frequency Error:",
                              min = 1,
                              max = 30,
                              value = 5),
                  checkboxInput("filter_interval",strong("Signal Interval Filter"),value = FALSE),
                  sliderInput("signal_interval",
                              "Signal Period in sec:",
                              min = 0.5,
                              max = 8,
                              value = c(0.8,1.2)),
                  selectInput("input_select_receiver", "Select Receiver/s", choices = NULL, multiple = TRUE, selectize = TRUE),
                  selectInput("input_select_station", "Select Station/s", choices = NULL, multiple = TRUE, selectize = TRUE),
                  textOutput("total_counts")
                  #checkboxInput("correct_signal_strength",strong("Signal Strength Correction"),value = FALSE),
                  #uiOutput("correction_list"),
                  #numericInput("dBLoss","dB between Antennas", 13 , min = 0 ,max = 50),
                  #numericInput("angle_sep","Anlge between Antennas", 90 , min = 0 ,max = 359),
                  #h5("y=A*exp(B*Temperature)"),
                  #numericInput("temp_cal_a","Enter Coefficient A",value=20.307)
                  #numericInput("temp_cal_b","Enter Coefficient B",value=0.0408)
           )
         )
)
