tabPanel("Triangulation",
         actionButton("calc_triangulations","Calc Triangulations", style="margin-bottom:25px"),
         br(),
         tabsetPanel(
           tabPanel("Settings",
                    sliderInput("slider_angles_allowed","Angles allowed",0,180,c(10,170)),
                    numericInput("time_error_inter_station","Time Error Inter Station",0.6,0,30, step = 0.05),
                    selectInput("tri_option_dd","Choose Triangulation Method", choices = c("centroid","two_strongest"))
           ),
           tabPanel("Filters",
                    actionButton("filter_speed","Distance Filter"),
                    sliderInput("tri_speed_slider","Speeds allowed [m/s]",0,400,150),
                    actionButton("form_centroids","Centroid Filter")
                    ),
           tabPanel("Compare to one point",
                    numericInput("compare_single_x", "X to compare to",8.644599),
                    numericInput("compare_single_y", "X to compare to",50.841835),
                    textOutput("single_distance"),
                    plotOutput("one_distance")
           ),
           tabPanel("Distance and Speed",
                    plotOutput("distance_btw_points")
           ),
           tabPanel("Table",
                    dataTableOutput("triangulation_points")
           )
         )
)