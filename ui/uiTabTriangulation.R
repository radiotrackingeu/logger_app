tabPanel("Triangulation",
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
      
      
      .tri {
      /* Appearance */
      background-color: white;
      padding: 10px 20px 10px 20px;
      /*cursor: move;*/
      /* Fade out while not hovering */
      opacity: 0.65;
      zoom: 0.9;
      transition: opacity 500ms 1s;
      }
      .tri:hover {
      /* Fade in while hovering */
      opacity: 0.95;
      transition-delay: 0;
      }"
    ))
  ),
  # this combination of withSpinner and leaflet leads to a memory leak in RStudio Browser. However works fine in Opera 54.0, IE 11, Edge 42, Chromium 64
  #withSpinner(proxy.height = "80vh",
    div(class="outer", width="100%", height="100%",
      leafletOutput("tri_map", width="100%", height="100%"),
      absolutePanel( 
        class="panel panel-default tri",
        top=150,
        right=10,
        draggable=F,
        fixed=T,
        left="auto",
        bottom="auto",
        width="300px",
        height="auto",
        HTML('<b data-toggle="collapse" data-target="#tri_settings_content" style="cursor:pointer">Settings</b>'),
        div(id = 'tri_settings_content',  class="collapse in",
          selectizeInput("tri_frequency","Select Tag/Frequency", choices=c(""), options = list(placeholder="Please select a tag.")),
          sliderInput("tri_error","Triangulation Error",0,20,4, width="100%"),
          numericInput("tri_timestep", "Set time step in seconds",60)
        )
        
      ),
      absolutePanel(
        class="panel panel-default tri",
        bottom="150",
        right="10",
        draggable=F,
        fixed=T,
        left="auto",
        top="auto",
        width="500px",
        height="auto",
        HTML('<b data-toggle="collapse" data-target="#tri_signals_content" style="cursor:pointer">Signals</b>'),
        div(id = 'tri_signals_content',  class="collapse",
          dataTableOutput("tri_positions_and_angles")
        )
      ),
      absolutePanel(
        class="panel panel-default tri",
        bottom="10",
        right="10",
        draggable=F,
        fixed=T,
        left="auto",
        top="auto",
        width="100%",
        height="auto",
        HTML('<b data-toggle="collapse" data-target="#tri_timeline_content" style="cursor:pointer">Timeline</b>'),
        div(id = 'tri_timeline_content',  class="collapse in",
          uiOutput("tri_ui_timeline")
        )
      )
    )
  #)
)
