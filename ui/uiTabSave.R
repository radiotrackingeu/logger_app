############ tabSave.R ############
tabPanel("Save Data",
  fluidRow(
    column(
      width=4,
        h1("SQLite"),
        uiOutput("download_ui_tables"),
        downloadButton("filtered_data_sqlite", "Download SQLite"),
    ), column(
      width=4,
      h1("Excel"),
      downloadButton("download_excel_signals", "Download signals"), 
      tags$br(),
      downloadButton("download_excel_frequencies", "Download frequencies"),
      tags$br(),
      downloadButton("download_excel_remote_connections", "Download remote connections"),
      tags$br(),
      downloadButton("download_excel_calibrations", "Download calibrations"),
      tags$br(),
      downloadButton("download_excel_receivers", "Download antennas"),
      tags$br(),
      downloadButton("download_csv_temperature", "Download temperature data"),
      tags$br(),
      downloadButton("download_excel_map_markers", "Download map markers"),
      tags$br(),
      downloadButton("download_excel_man_points", "Download manual positions"),
      tags$br(),
      downloadButton("download_excel_bearings", "Download bearings"),
      tags$br(),
      downloadButton("download_tri_points", "Download triangulated points"),
    ),
    column(
      width=4,
      h1("CSV"),
      downloadButton("download_csv_signals", "Download filtered signals"),
      tags$br(),
      tags$span("(Use csv if there are too many signals for excel files.)")
    )
  )
)
