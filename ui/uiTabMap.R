############ tabMap.R ############
tabPanel("Map",
  div(class="outer",
    tags$head(
      tags$style(HTML("
                               div.outer {
                               
                               position: fixed;
                               top: 51px;
                               left: 0;
                               right: 0;
                               bottom: 0;
                               overflow: hidden;
                               padding: 0;
                               }


#controls {
  /* Appearance */
  background-color: white;
  padding: 0 20px 20px 20px;
  cursor: move;
  /* Fade out while not hovering */
  opacity: 0.65;
  zoom: 0.9;
  transition: opacity 500ms 1s;
}
#controls:hover {
  /* Fade in while hovering */
  opacity: 0.95;
  transition-delay: 0;
}
                               
                               "))
    ),
    leafletOutput("map", width="100%", height="100%"),
    
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
      draggable = F, top = 60, left = "auto", right = 20, bottom = "auto",
      width = 330, height = "auto",
      checkboxInput("select_offline_map","Online Map",value=TRUE),
      selectInput("map_choose", "Choose Map", choices = unlist(providers)),
      checkboxInput("map_show_antennae_outline", "Show antennae outlines", value=TRUE),
      checkboxInput("map_activate_single_data","Show Timeline",value = FALSE),
      conditionalPanel(condition="input.map_activate_single_data==true",
        sliderInput("map_choose_single_data_set","Data Steps", min=1, max =500,value = 1, animate = list(interval=400), step = 1),
        plotOutput("map_miniplot", height = "150px")
      ),
      htmlOutput("map_signal_select_prop")
    )
  )
)