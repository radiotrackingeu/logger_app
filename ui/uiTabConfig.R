#uiTabconfings.R
tabPanel("Configure",
  sidebarLayout(
    sidebarPanel(
      h4("Load from ..."),
      uiOutput('conf_ui_select'),
      actionButton("conf_dload", "Dowload config"),
      h5("or"),
      fileInput("conf_file", "... local file"),
      hr(),
      h4("Save..."),
      fluidRow(
        actionButton("conf_uload", "Upload config(s)", style="display:inline-block;"),
        h5("or", style="display:inline-block; margin: 0 20px 0 20px;"),
        downloadButton("conf_dl", "Save locally", style="display:inline-block;")
      ),
    ),
    mainPanel(
      tabsetPanel(
        id = "tab_conf_editor",
        tabPanel("Editor",
          aceEditor(
            outputId = "conf_editor",
            value="",
            mode="json",
            theme = "dawn",
            height = "calc(100vh - 91px)",
            tabSize = 2,
            autoComplete = "disabled",
            placeholder = "Load config from device or copy into this area."
          )
        ),
        tabPanel("Help",
          tags$h3("Loading:"),
          "- Select ", tags$b("a single"), " connection to download the config from and hit the download button.",br(),
          "or",br(),
          "- Use the Browse button to load a file from your device.",
          tags$h3("Edit:"),
          "Just edit the file in the editor box on the right.",
          tags$h3("Saving:"),
          "- To save to one or multiple remote connection(s), select the connection(s) and hit the upload button.",br(),
          "or", br(),
          "- Use the save locally button to download the file unto your device.",br(),
          hr(),
          "It is recommended to load connections using a RemoteConnections Excel-file, however you can manually add connections using the Live Data Tab. Keep in mind, that you need to specify the MySQL port there."
        )
      )
    )
  )
)
