############ tabSave.R ############
tabPanel("Save Data",
    h1("SQLite"),
    downloadButton("filtered_data_sqlite", "Download SQLite"),
    h1("Excel"),
    downloadButton("download_excel_signals", "Download signals"),
    downloadButton("download_excel_frequencies", "Download frequencies"),
    downloadButton("download_excel_remote_connections", "Download remote connections"),
    downloadButton("download_excel_calibrations", "Download calibrations"),
    downloadButton("download_excel_receivers", "Download receivers")
)
