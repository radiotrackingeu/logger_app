#srvTabSave.R

output$filtered_data_sqlite <- downloadHandler(
  filename = function() {
    "filtered_data.sqlite"
  },
  content = function(file) {
    con <- dbConnect(RSQLite::SQLite(), file)
    if(!is.null(filtered_data())){
      dbWriteTable(con,"rteu_logger_data",filtered_data(),overwrite=TRUE)
    }
    if(!is.null(global$frequencies)){
      dbWriteTable(con,"rteu_freqs",global$frequencies,overwrite=TRUE)
    }
    if(!is.null(global$receivers)){
      dbWriteTable(con,"rteu_antenna",global$receivers,overwrite=TRUE)
    }
    if(!is.null(global$connections)){
      dbWriteTable(con,"rteu_connections",global$connections,overwrite=TRUE)
    }
    if(!is.null(global$calibration)){
      dbWriteTable(con,"rteu_calibration",global$calibration,overwrite=TRUE)
    }
    if(!is.null(global$map_markers)){
      dbWriteTable(con,"rteu_map_markers",global$map_markers,overwrite=TRUE)
    }
    calibration_state <- data.frame(global$calibrated)
    print(calibration_state)
    dbWriteTable(con, "rteu_calibrated", calibration_state, overwrite=TRUE)
    
    dbDisconnect(con)
  }
)

output$download_excel_frequencies <- downloadHandler(
  filename = "Frequencies.xlsx",
  content = function(file) {
    if (!is.null(global$frequencies)) {
      write_xlsx(global$frequencies, file)
    }
    else {
      write_xlsx(data.frame(), file)
    }
  }
)

output$download_excel_calibrations <- downloadHandler(
  filename = "Calibration.xlsx",
  content = function(file) {
    if (!is.null(global$calibration)) {
      write_xlsx(global$calibration, file)
    }
    else {
      write_xlsx(data.frame(), file)
    }
  }
)

output$download_excel_signals <- downloadHandler(
  filename = "signals.xlsx",
  content = function(file) {
    if (!is.null(filtered_data_td())) {
      write_xlsx(filtered_data_td()[,!c("temperature", "samples", "keep")], file)
    } else if (!is.null(filtered_data())) {
      write_xlsx(filtered_data()[,!c("samples", "keep")], file)
    } else {
      write_xlsx(data.frame(), file)
    }
  }
)

output$download_csv_signals <- downloadHandler(
  filename = "filtered_data.csv",
  content = function(file) {
    if (!is.null(filtered_data_td())) {
      fwrite(filtered_data_td()[,!c("temperature", "samples", "keep")], file, sep = ";", dec = ",")
    } else if (!is.null(filtered_data())) {
      fwrite(filtered_data()[,!c("samples", "keep")], file, sep = ";", dec = ",")
    } else {
      fwrite(data.frame(), file, sep = ";", dec = ",")
    }
  }
)

output$download_excel_remote_connections <- downloadHandler(
  filename = "RemoteConnections.xlsx",
  content = function(file) {
    if (!is.null(global$connections)) {
      write_xlsx(global$connections, file)
    }
    else {
      write_xlsx(data.frame(), file)
    }
  }
)

output$download_excel_receivers <- downloadHandler(
  filename = "Antennas.xlsx",
  content = function(file) {
    if (!is.null(global$receivers)) {
      write_xlsx(global$receivers, file)
    }
    else {
      write_xlsx(data.frame(), file)
    }
  }
)

output$download_excel_bearings <- downloadHandler(
  filename = "bearings.xlsx",
  content = function(file) {
    if (!is.null(global$bearing)) {
      write_xlsx(global$bearing, file)
    }
    else {
      write_xlsx(data.frame(), file)
    }
  }
)

output$download_tri_points <- downloadHandler(
  filename = "Triangulations.xlsx",
  content = function(file) {
    if (!is.null(global$triangulation)) {
      write_xlsx(global$triangulation, file)
    }
    else {
      write_xlsx(data.frame(), file)
    }
  }
)



output$download_csv_temperature <- downloadHandler(
  filename = "TemperatureData.xlsx",
  content = function(file) {
    if (!is.null(filtered_data_td())) {
      write.csv2(filtered_data_td(), file)
    }
    else {
      write_xlsx(data.frame(), file)
    }
  }
)

output$download_excel_map_markers <- downloadHandler(
  filename = "MapMarkers.xlsx",
  content = function(file) {
    if (!is.null(global$map_markers)) {
      write_xlsx(global$map_markers, file)
    }
    else {
      write_xlsx(data.frame(), file)
    }
  }
)