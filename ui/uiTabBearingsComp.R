############ tabBearings.R ############
tabPanel("Bearings Compare",
  fluidRow(
    column(
      width=2,
      # settings
      selectInput(inputId = "input_bearings_comp_plot_select", label = "Select plot type", choices = c("Plot1", "Plot2")),
      selectInput(inputId = "input_bearings_comp_stat_select", label = "Select station", choices=NULL, multiple = F)
    ),
    column(
      width=10,
      tabsetPanel(
        tabPanel("Plot",
          div(
            plotOutput("bearings_comp_plot", height = "100%"),
            style="height: calc(100vh - 150px)"
          )
        ),
        tabPanel("Map",
          div(
            leafletOutput("bearings_comp_map", width = "100%", height = "100%"),
            style="height: calc(100vh - 150px)"
          )
        )
      )
    )
  )
)
