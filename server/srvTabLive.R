############ srvTabLive.R ############

### render ui elements ###

observeEvent(input$connect_to_db, {
    global$connections <- rbind(global$connections,
      data.frame("Name"=input$MySQL_name, "Host"=input$MySQL_host, "Port"=input$MySQL_port,"User"=input$MySQL_user,"Password"=input$MySQL_pw,stringsAsFactors=F)
    )
})

output$con_tags <- renderUI({
    selectizeInput("select_connection", multiple=TRUE,selected=global$connections$Name,label="Please select connections",choices = global$connections$Name)
})

open_connections <- eventReactive(input$connect_mysql,{
  tmp_list<-list()
  if(!is.null(global$connections)){
    connect_to <- subset(global$connections,Name %in% input$select_connection)
    withProgress(
      expr = {
        for(i in 1:nrow(connect_to)){
          setProgress(detail=connect_to$Name[i])
          tmp_list[[connect_to$Name[i]]]<-tryCatch(
            dbConnect(
              drv=RMySQL::MySQL(),
              dbname = "rteu",
              host = connect_to$Host[i],
              port = connect_to$Port[i],
              username = connect_to$User[i],
              password = connect_to$Password[i]
            ),
            error = function(err){
              NULL
            },
            finally = {
              incProgress(amount=1)
            }
          )
        }
      },
      message = "Attempting connection: ",
      max = nrow(connect_to),
      value = 0
    )
  }else{
    tmp_list<-NULL
  }
  return(tmp_list)
})

get_info_of_entries <- reactive({
  tmp<-data.frame()
  if(!is.null(global$connections)){
    connect_to <- subset(global$connections,Name %in% input$select_connection)
    withProgress(
      expr = { for(i in connect_to$Name){
        setProgress(detail=i)
        if(is.null(open_connections()[[i]])){
          results<-data.frame(Name=i,id=NA,timestamp="unknown",size="unknown",running="unknown",time="unknown")
          tmp<-rbind(tmp,results)
          next
        }
        else{
          if(dbIsValid(open_connections()[[i]])) {
            results<-dbGetQuery(open_connections()[[i]],"SELECT id,timestamp FROM `signals` ORDER BY id DESC LIMIT 1;")
            results$size <- dbGetQuery(open_connections()[[i]], '
                                       SELECT ROUND(SUM(data_length + index_length) / 1024 / 1024, 1) "size"
                                       FROM information_schema.tables;
                                       ')$size
            results$time <- dbGetQuery(open_connections()[[i]], 'SELECT NOW();')$'NOW()'
            if(as.POSIXct(Sys.time(), tz="UTC")-as.POSIXct(results$timestamp, tz="UTC")<360){
              results$running<-"Yes"
            }else{
              results$running<-"No"
            }
            if(nrow(results)==0){
              results<-data.frame(id = 0 , timestamp="logger not running")
            }
            results$Name<-i
            tmp<-rbind(tmp,results)
          }else{
            results<-data.frame(Name=i,id=NA,timestamp="offline")
            tmp<-rbind(tmp,results)
          }
      }
        incProgress(amount=1)
      }
  },
  message = "Fetching additional informations: ",
  max = nrow(connect_to),
  value = 0
    )
      }else{
        tmp<-NULL
      }
  return(tmp)
})

global$live_mode = FALSE
global$mysql_data_invalidator = FALSE

observeEvent(input$load_mysql_data, {
  global$live_mode = input$app_live_mode
  if (global$live_mode) {
      global$live_update_interval = input$live_update_interval
  }
  else {
    global$mysql_data_invalidator = !global$mysql_data_invalidator
    signal_data()
  }
})

live_invalidator <- observe({
    if (global$live_mode) {
        global$mysql_data_invalidator = !isolate(global$mysql_data_invalidator)
        signal_data()
        invalidateLater(isolate(global$live_update_interval) * 1000)
    }
})

get_mysql_data <- eventReactive(global$mysql_data_invalidator, {
  if(!is.null(get_info_of_entries())){
    tmp<-data.frame()

    if(!is.null(get_info_of_entries())){
      withProgress(
        expr = {
          for(i in get_info_of_entries()[get_info_of_entries()$timestamp!="offline",]$Name) {
            setProgress(detail=i)
            if(is.null(open_connections()[[i]])) {
              next
            }
            else {
              if(dbIsValid(open_connections()[[i]])) {
                signals<-dbGetQuery(open_connections()[[i]], build_signals_query())
                mysql_query_runs<-paste("SELECT id, device, pos_x, pos_y, orientation, beam_width, center_freq FROM `runs` ORDER BY id DESC LIMIT",input$live_last_points,";")
                runs<-dbGetQuery(open_connections()[[i]],mysql_query_runs)
                if(nrow(signals)>0){
                  results<-merge(signals,runs,by.x="run",by.y="id")
                  results$run <- NULL
                  results$id <- NULL
                  results$Name<-i
                  tmp<-rbind(tmp,results)
                }
              }
              else{
                results<-data.frame(Name=i,id=NA,timestamp="offline")
                tmp<-rbind(tmp,results)
              }
            }
            incProgress(amount=1)
          }
        },
        message = "Loading data: ",
        max = nrow(get_info_of_entries()[get_info_of_entries()$timestamp!="offline",]),
        value = 0
      )
    }
    else{
      tmp<-NULL
    }
    global$tmp_data <- tmp
  }
})

keepalive_data <- reactive({
  if(!is.null(get_info_of_entries())){
    tmp<-data.frame()

    if(!is.null(get_info_of_entries())){
      withProgress(
        expr = {
          for(i in get_info_of_entries()[get_info_of_entries()$timestamp!="offline",]$Name) {
            setProgress(detail=i)
            if(is.null(open_connections()[[i]])) {
              next
            }
            else {
              if(dbIsValid(open_connections()[[i]])) {
                query <- paste("SELECT timestamp, device FROM `signals` s INNER JOIN runs r ON r.id = s.run WHERE max_signal = 0 LIMIT ", input$live_last_points, ";")
                results<-dbGetQuery(open_connections()[[i]], query)

                if(nrow(results)>0){
                  results$Name <- i
                  results$receiver <- substrLeft(results$device,17)
                  results$device <- NULL
                  tmp <- rbind(tmp,results)
                }
              }
              else{
                results <- data.frame(Name=i,id=NA,timestamp="offline")
                tmp <- rbind(tmp,results)
              }
            }
            incProgress(amount=1)
          }
        },
        message = "Loading data: ",
        max = nrow(get_info_of_entries()[get_info_of_entries()$timestamp!="offline",]),
        value = 0
      )
    }
    else{
      tmp<-NULL
    }
    tmp
  }
})

build_signals_query <- reactive({
    query_duration_filter<-""
    query_max_signal_filter<-""
    if(input$check_sql_duration){
      query_duration_filter<-paste("duration >",input$query_filter_duration[1],"AND duration <",input$query_filter_duration[2])
    }
    if(input$check_sql_strength){
      if(any(input$check_sql_duration)){
        and<-"AND"
      }else{
        and<-""
      }
      query_max_signal_filter<-paste(and,"max_signal >=",input$query_filter_strength[1],"AND max_signal <=",input$query_filter_strength[2])
    }

    query_freq_filter<-""
    inner_join <- ""
    if (input$query_filter_freq){
      error <- input$freq_error * 1000
      and<-""
      inner_join <- "INNER JOIN `runs` r ON s.run = r.id"
      if (input$query_filter_frequency_type == "Multiple") {
          for(k in global$frequencies$Frequency){
            if(any(input$check_sql_duration,input$check_sql_strength)) {
              and<-"AND("
            }
            if(nrow(global$frequencies)>1&&query_freq_filter!=""){
              and<-"OR"
            }
            query_freq_filter<-paste(query_freq_filter, and, "((signal_freq + center_freq + ",error,") >", k*1000, "  AND (signal_freq + center_freq - ",error,")  <", k*1000, ")")
          }
          if(any(input$check_sql_duration,input$check_sql_strength)){
            query_freq_filter<-paste(query_freq_filter,")")
          }
      }
      else if (input$query_filter_frequency_type == "Single") {
            k <- input$query_filter_single_frequency
            if(any(input$check_sql_duration,input$check_sql_strength)) {
              and<-"AND("
            }
            query_freq_filter<-paste(query_freq_filter, and, "((signal_freq + center_freq + ",error,") >", k*1000, "  AND (signal_freq + center_freq - ",error,")  <", k*1000, ")")
      }
    }
    where<-""
    if(any(input$check_sql_duration,input$check_sql_strength,input$query_filter_freq)){
      where<-"WHERE"
    }

    paste("SELECT timestamp, duration, signal_freq, run, max_signal FROM `signals` s", inner_join, where,query_duration_filter,query_max_signal_filter,query_freq_filter,"ORDER BY s.id DESC LIMIT",input$live_last_points,";")
})

signal_data<-function(){
  req(get_mysql_data())
  tmp<-get_mysql_data()

  if(is.null(tmp)) return(NULL)
  if(nrow(tmp)==0) return(NULL)
  #tmp<-subset(get_mysql_data(),signal_freq!=0)
  tmp$timestamp <- as.POSIXct(tmp$timestamp)
  tmp$signal_freq <- round((tmp$signal_freq+tmp$center_freq)/1000)
  tmp$receiver <- substrLeft(tmp$device,17)

  signal_info <- tmp[, c("timestamp", "duration", "signal_freq", "Name", "receiver", "max_signal")]
  global$signals<-unique.data.frame(rbind(isolate(global$signals), signal_info))

  receiver_info <- tmp[, c("receiver", "Name", "pos_x", "pos_y", "orientation", "beam_width")]
  names(receiver_info) <- c("Name", "Station","Latitude","Longitude", "Orientation", "Beam width")
  global$receivers<-unique.data.frame(rbind(isolate(global$receivers), receiver_info))
  tmp
}

output$live_tab_remote_entries_table <- renderDataTable({
  validate(need(get_info_of_entries(), "Please provide remote connection data file."))
  if (nrow(get_info_of_entries()) == 0) {
    return (NULL)
  }
  tmp <- get_info_of_entries()[, c("Name", "running", "timestamp", "size","time")]
  names(tmp) <- c("Name", "Reachable", "Latest timestamp", "Size (MB)","System Time")
  tmp
}, options = list(pageLength = 10))

output$live_tab_mysql_data <- renderDataTable({
  validate(need(global$signals, "Please load some data"))
  global$signals
}, options = list(pageLength = 10))

output$live_tab_keepalive<- renderDataTable({
  validate(need(keepalive_data(), "Please check connections first"))
  keepalive_data()
}, options = list(pageLength = 10))
