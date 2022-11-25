############ tabSave.R ############
tabPanel("Save Data",
    h1("SQLite"),
    downloadButton("filtered_data_sqlite", "Download SQLite"),
    h1("Excel"),
    downloadButton("download_excel_signals", "Download signals"),
    downloadButton("download_excel_frequencies", "Download frequencies"),
    downloadButton("download_excel_remote_connections", "Download remote connections"),
    downloadButton("download_excel_calibrations", "Download calibrations"),
    downloadButton("download_excel_receivers", "Download antennas"),
    downloadButton("download_csv_temperature", "Download temperature data"),
    downloadButton("download_excel_map_markers", "Download map markers"),
    downloadButton("download_excel_bearings", "Download bearings"),
    downloadButton("download_tri_points", "Download triangulated points"),
    h1("CSV"),
    downloadButton("download_csv_signals", "Download filtered signals"),
    tags$span("(Use csv if there are too many signals for excel files.)")
)
