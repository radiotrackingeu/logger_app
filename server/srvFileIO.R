############ FileIO ############
# sets the maximal upload size to 3GB
options(shiny.maxRequestSize = 3000 * 1024 ^ 2)
# reads and reformats data from file. Adds col timestamp with posix-style readable time

read_logger_folder <-function(){
  path<-file.path("data","logger")
  list_of_stations<-list.dirs(path,full.names = FALSE, recursive =FALSE)
  tmp_data<-NULL

  receivers_count <- 0;
  for(i in list_of_stations){
    receivers_count <- receivers_count + length(list.dirs(file.path(path,i), full.names = FALSE, recursive = FALSE))
  }
  status_read<-0

  withProgress(
      for(i in list_of_stations){
        list_of_receivers<-list.dirs(file.path(path,i), full.names = FALSE, recursive = FALSE)
        for (j in list_of_receivers) {
          setProgress(detail=paste0(i, ", ", j))
          list_of_records <- list.files(file.path(path,i,j), no..=T)
          status_read<-status_read+1
          for (k in list_of_records) {
            p<-file.path(path,i,j,k)
            print(p)
            data<-read_logger_data(p)
            if(!is.null(data)){
              tmp_data<-rbind(cbind(data, receiver = j, Name = i),tmp_data)
            }
          }
          incProgress(amount=1)
        }
      },
      message = "Reading data from ",
      max = receivers_count,
      value = 0
  )
  return(tmp_data[, c("timestamp", "duration", "signal_freq", "Name", "receiver", "max_signal","signal_bw")])
}

get_logger_files <- function() {
  path<-file.path("data","logger")
  list_of_stations<-list.dirs(path,full.names = FALSE, recursive =FALSE)
  tmp_data<-NULL

  files <- c()

  for(i in list_of_stations){
    list_of_receivers<-list.dirs(file.path(path,i), full.names = FALSE, recursive = FALSE)
    for (j in list_of_receivers) {
      list_of_records <- list.files(file.path(path,i,j), no..=T)
      for (k in list_of_records) {
        p <- file.path(path,i,j,k)
        files <- c(files, p)
      }
    }
  }

  files
}

read_logger_data <- function(filepath){
  lines_to_skip <- findHeader(filepath) #skip meta data in files
  if (lines_to_skip < 0) return(NULL)

  mid_freq <- findMidFreq(filepath) # find center frequency of tuner
  if(mid_freq < 0) return(NULL)
  #data_in_file <- readLines(filepath) #reads two times... 
  #last_rows_skip<-0
  #if(grepl("total transforms",data_in_file[length(data_in_file)])){
  #  print("that recording crashed")
  #  last_rows_skip <- length(data_in_file)-3-lines_to_skip
  #  if(last_rows_skip<1) return(NULL)
  #}
  data <-
    read.csv2(
      filepath,
      skip = lines_to_skip,
      stringsAsFactors = FALSE,
      dec = ".",
      #nrows=last_rows_skip,
      fill=TRUE
    )
  data$max_signal[is.na(data$max_signal)]<-0
  data<-data[countCharOccurrences("[:-]",data$timestamp)==4,]
  data$timestamp <- as.POSIXct(data$timestamp, tz = "UTC")
  data$signal_freq <- (data$signal_freq + mid_freq) / 1000
  data$freq_tag<-NA
  return(data)
}

findHeader <- function(file) {
  n<--1
  tryCatch(
    {
      tmp <- readLines(file, n = 30, warn=F)
      n <- grep(pattern="timestamp;samples;duration;signal_freq;signal_bw;max_signal", x=tmp, fixed=T) - 1
      if (length(n)==0) n<--1
    }, warning = function(w) {
      print(w$message)
      n<--1
    }, error = function(e) {
      n<--1
      print(e$message)
    }, finally = {
      return(n)
    }
  )
}

findMidFreq <- function(file) {
  MidFreq<--1
  tryCatch({
    tmp <- readLines(file, n = 30, warn=F)
    n <- grep("Tuned to", tmp)
    if(length(n)>0){
      MidFreq <- as.numeric(gsub("[a-z,A-Z,., ]", "", tmp[n]))
    }
  }, warning = function(w) {
    print(w$message)
    MidFreq<--1
  }, error = function(e) {
    print(e$message)
    MidFreq<--1
  }, finally = {
    return(MidFreq)
  }
  )
}

filter_data_and_save <- function(filepath_db,filepath_filterSettings, receiver){
  # open db
  con <- dbConnect(RSQLite::SQLite(),filepath_db)
  # import data
  data<- dbReadTable(con,receiver)
  # read file with filter settings
  filterSettings<-read.csv2(filepath_filterSettings,dec=".", stringsAsFactors = FALSE, row.names = NULL)
  # filter for characteristic length
  data<-filter_data_length(data,filterSettings$duration)
  # filter for bandwith
  data<-filter_signal_bandwith(data,filterSettings$bandwith)
  # filter for signal strength
  data<-filter_signal_strength(data,filterSettings$strength)
  #disconnect
  dbDisconnect(con)
}



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
        if (!is.null(global$signals)) {
            write_xlsx(global$signals, file)
        }
        else {
            write_xlsx(data.frame(), file)
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

safe_read_excel <- function(filepath) {
    tryCatch({
            read_excel(filepath, sheet = 1)
        },
        warning = function(cond) {
            show_warning(paste("Warning while uploading ", filepath, ": ", cond[1]))
        },
        error = function(cond) {
            show_error(paste(filepath, " couldn't be uploaded: ", cond[1]))
        }
    )
}

safe_read_excel_silent <- function(filepath) {
    tryCatch({
            read_excel(filepath, sheet = 1)
        },
        warning = function(cond) {
        },
        error = function(cond) {
        }
    )
}

countCharOccurrences <- function(char, s) {
  s2 <- gsub(char,"",s)
  return (nchar(s) - nchar(s2))
}