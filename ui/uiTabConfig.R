#uiTabconfings.R
tabPanel("Receiver configs",
  sidebarLayout(
    sidebarPanel(
      uiOutput('conf_ui_select'),
      actionButton("conf_dload", "Dowload config"),
      br(),
      br(),
      actionButton("conf_uload", "Upload config(s)")#,
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
          tags$h3("Download:"),
          "1) Select a single connection to download the config from.",
          br(),
          "2) Hit 'Download config'.",
          tags$h3("Edit:"),
          "1) Just edit the file in the editor box on the right.",
          tags$h3("Upload:"),
          "1) Select one or multiple connections to upload a config to.",
          br(),
          "2) Double check your input!",
          br(),
          "3) Hit 'Upload config(s)'"
        )
      )
    )
  )
)
