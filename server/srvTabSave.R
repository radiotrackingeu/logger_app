#srvTabSave.R


sqlite_export_tables <- c(
  "Filtered Signals" = "rteu_logger_data", 
  "Frequencies" = "rteu_freqs", 
  "Receivers" = "rteu_antenna", 
  "Remote Connections" = "rteu_connections", 
  # "Calibration Data" = "rteu_calibration", 
  "Calculated Bearings" = "rteu_bearings", 
  "Calculated Triangulations" = "rteu_triangulations", 
  # "Manual Map Markers" = "rteu_manual_markers"
  "Manual Positions" = "rteu_man_points"
)

output$download_ui_tables <- renderUI(
  awesomeCheckboxGroup(
    inputId = "download_checkbox_tables",
    label = "Select tables to be included",
    choices = sqlite_export_tables,
    selected = NULL
  )
)

outputOptions(output, "download_ui_tables", suspendWhenHidden = FALSE)

observeEvent(input$navbar, {
  req(input$navbar == "Save Data")
  enabled <- NULL
  
  if ("rteu_logger_data" %in% sqlite_export_tables && !is.null(filtered_data()) && nrow(filtered_data()) > 0) {
    enabled <- c(enabled, "rteu_logger_data")
  }
  
  if ("rteu_freqs" %in% sqlite_export_tables && !is.null(global$frequencies) && nrow(global$frequencies) > 0) {
    enabled <- c(enabled, "rteu_freqs")
  }
  
  if ("rteu_antenna" %in% sqlite_export_tables && !is.null(global$receivers) && nrow(global$receivers) > 0) {
    enabled <- c(enabled, "rteu_antenna")
  }
  
  if ("rteu_connections" %in% sqlite_export_tables && !is.null(global$connections) && nrow(global$connections) > 0) {
    enabled <- c(enabled, "rteu_connections")
  }

  if ("rteu_calibration" %in% sqlite_export_tables && !is.null(global$calibration) && nrow(global$calibration) > 0) {
    enabled <- c(enabled, "rteu_calibration")
  }
  
  if ("rteu_bearings" %in% sqlite_export_tables && !is.null(global$bearing) && nrow(global$bearing) > 0) {
    enabled <- c(enabled, "rteu_bearings")
  }
  
  if ("rteu_triangulations" %in% sqlite_export_tables && !is.null(global$triangulation) && nrow(global$triangulation) > 0) {
    enabled <- c(enabled, "rteu_triangulations")
  }
  
  if ("rteu_manual_markers" %in% sqlite_export_tables && !is.null(global$map_markers) && nrow(global$map_markers) > 0) {
    enabled <- c(enabled, "rteu_manual_markers")
  }
  
  if ("rteu_man_points" %in% sqlite_export_tables && !is.null(global$man_points) && nrow(global$man_points) > 0) {
    enabled <- c(enabled, "rteu_man_points")
  }
  
  updateAwesomeCheckboxGroup(
    inputId = "download_checkbox_tables", 
    selected = enabled
  )
  
  alply(.data=sqlite_export_tables, .margins = 1, .expand = F, .fun = function(t) {
    if (t %in% enabled) {
      shinyjs::enable(selector = paste0("input[value = '", t, "']"))
    } else {
      shinyjs::disable(selector = paste0("input[value = '", t, "']"))
    }
  })
})

output$filtered_data_sqlite <- downloadHandler(
  filename = function() {
    "filtered_data.sqlite"
  },
  content = function(file) {
    con <- dbConnect(RSQLite::SQLite(), file)
    if("rteu_logger_data" %in% input$download_checkbox_tables && !is.null(filtered_data())){
      dbWriteTable(con,"rteu_logger_data",filtered_data(),overwrite=TRUE)
    }
    if("rteu_freqs" %in% input$download_checkbox_tables &&!is.null(global$frequencies)){
      dbWriteTable(con,"rteu_freqs",global$frequencies,overwrite=TRUE)
    }
    if("rteu_antenna" %in% input$download_checkbox_tables &&!is.null(global$receivers)){
      dbWriteTable(con,"rteu_antenna",global$receivers,overwrite=TRUE)
    }
    if("rteu_connections" %in% input$download_checkbox_tables &&!is.null(global$connections)){
      dbWriteTable(con,"rteu_connections",global$connections,overwrite=TRUE)
    }
    if("rteu_calibration" %in% input$download_checkbox_tables && !is.null(global$calibration)){
      dbWriteTable(con,"rteu_calibration",global$calibration,overwrite=TRUE)
    }
    if("rteu_bearings" %in% input$download_checkbox_tables && !is.null(global$bearing)){
      dbWriteTable(con,"rteu_bearings", global$bearing, overwrite=TRUE)
    }
    if("rteu_triangulations" %in% input$download_checkbox_tables && !is.null(global$triangulation)){
      dbWriteTable(con,"rteu_triangulations",global$triangulation,overwrite=TRUE)
    }
    if("rteu_manual_markers" %in% input$download_checkbox_tables && !is.null(global$map_markers)){
      dbWriteTable(con,"rteu_manual_markers",global$map_markers,overwrite=TRUE)
    }
    if("rteu_man_points" %in% input$download_checkbox_tables && !is.null(global$man_points)){
      dbWriteTable(con,"rteu_man_points",global$man_points,overwrite=TRUE)
    }
    
    calibration_state <- data.frame(global$calibrated)
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

output$download_excel_man_points <- downloadHandler(
  filename = "ManualPositions.xlsx",
  content = function(file) {
    if (!is.null(global$man_points)) {
      write_xlsx(as.data.table(global$man_points)[,.("Time"=timestamp, "Individual"=freq_tag, "Longitude"=longitude, "Latitude"=latitude)], file)
    }
    else {
      write_xlsx(data.frame(), file)
    }
  }
)