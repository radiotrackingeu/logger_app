############ FileIO ############
# sets the maximal upload size to 3GB
options(shiny.maxRequestSize = 3000 * 1024 ^ 2)
# reads and reformats data from file. Adds col timestamp with posix-style readable time

read_logger_folder <-function(){
  path<-file.path("data","logger")
  list_of_stations<-list.dirs(path,full.names = FALSE, recursive =FALSE)
  tmp_data<-NULL
  for(i in list_of_stations){
    print(i)
    list_of_receivers<-list.dirs(file.path(path,i), full.names = FALSE, recursive = FALSE)
    for (j in list_of_receivers) {
      print(j)
      list_of_records <- list.files(file.path(path,i,j), no..=T)
      for (k in list_of_records) {
        print(k)
        p<-file.path(path,i,j,k)
        data<-read_logger_data(p)
        if(!is.null(data)){
          tmp_data<-rbind(cbind(data, receiver = j, Name = i),tmp_data)
        }
      }
    }
  }
  return(tmp_data)
}

read_logger_data <- function(filepath) {
  lines_to_skip <- findHeader(filepath) #skip meta data in files
  if (lines_to_skip < 0) return(NULL)

  mid_freq <- findMidFreq(filepath) # find center frequency of tuner
  if (mid_freq < 0) return(NULL)

  data <-
    read.csv2(
      filepath,
      skip = lines_to_skip,
      stringsAsFactors = FALSE,
      dec = "."
    )
  data$max_signal[is.na(data$max_signal)]<-0
  data$timestamp <-
    as.POSIXct(data$timestamp, tz = "UTC")
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
    MidFreq <- as.numeric(gsub("[a-z,A-Z,., ]", "", tmp[n]))
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
    dbDisconnect(con)
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
