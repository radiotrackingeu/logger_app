############ srvTabLive.R ############

### render ui elements ###
observeEvent(input$add_manual_connection, {
    if (input$MySQL_name %in% global$connections$Name) {
        show_error("Could not add connection: name already in use")
    }
    else {
    global$connections <- rbind(global$connections,
      data.frame("Name"=input$MySQL_name, "Host"=input$MySQL_host, "Table"=input$MySQL_table, "Port"=input$MySQL_port,"User"=input$MySQL_user,"Password"=input$MySQL_pw,stringsAsFactors=F)
    )
    }
})

output$con_tags <- renderUI({
    selectizeInput("select_connection", multiple=TRUE,selected=global$connections$Name,label="Connections selection", choices = global$connections$Name)
})

open_connections <- eventReactive(input$connect_mysql,{
  tmp_list<-list()
  if (is.null(global$connections)) {
        show_error("Please select at least one connection")
        return (NULL)
  }
  else {
    connect_to <- subset(global$connections,Name %in% input$select_connection)
    if (nrow(connect_to) == 0) {
        show_error("Please select at least one connection")
        return (NULL)
    }
    withProgress(
      expr = {
        for(i in 1:nrow(connect_to)){
          setProgress(detail=connect_to$Name[i])
          table = connect_to$Table[i]
          table = ifelse(!is.character(table),"signals",table)
          tmp_list[[connect_to$Name[i]]] <- list("conn"=open_connection(connect_to[i, ]), "table"=table)
              incProgress(amount=1)
            }
      },
      message = "Attempting connection: ",
      max = nrow(connect_to),
      value = 0
    )
  }
  return(tmp_list)
})

open_connection <- function(connection_info) {
  tryCatch(
    dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "rteu",
      host = connection_info$Host,
      port = connection_info$Port,
      username = connection_info$User,
      password = connection_info$Password
    ),
    error = function(err){
      show_error(paste0("Could not connect to ", connection_info$Name, ": ", err[1]))
    }
  )
}

table_name <- reactive({
  if(is.character(input$MySQL_table) & input$MySQL_table!="")
    input$MySQL_table
  else
    "signals"
})

get_info_of_entries <- reactive({
  tmp<-data.frame()
  if(!is.null(global$connections)){
    connect_to <- subset(global$connections,Name %in% input$select_connection)
    withProgress(
      expr = {
        for(i in connect_to$Name){
        setProgress(detail=i)
            if (input$connect_mysql == 0 || is.null(open_connections()[[i]]$conn)) {
          results<-data.frame(Name=i,id=NA,timestamp="unknown",size="unknown",running="unknown",time="unknown",stringsAsFactors = FALSE)
          tmp<-rbind(tmp,results)
        }
            else {
          if(dbIsValid(open_connections()[[i]]$conn)) {
            results<-suppressWarnings(dbGetQuery(open_connections()[[i]]$conn,paste0("SELECT id,timestamp FROM `",open_connections()[[i]]$table,"` ORDER BY id DESC LIMIT 1;")))
            if(nrow(results)>0){
              results$size <- suppressWarnings(dbGetQuery(open_connections()[[i]]$conn, paste0('
                                       SELECT ROUND(SUM(data_length + index_length) / 1024 / 1024, 1) "size"
                                       FROM information_schema.tables WHERE table_schema = "rteu" AND table_name = "', open_connections()[[i]]$table, '";')
                                       )$size)
              results$time <- suppressWarnings(dbGetQuery(open_connections()[[i]]$conn, 'SELECT NOW();')$'NOW()')
              if(abs(difftime(as.POSIXct(Sys.time(), tz="UTC"),as.POSIXct(results$timestamp, tz="UTC"),units="mins"))<6){
                results$running<-"Recording"
              }else{
                results$running<-"Not recording"
              }
              }
            if(nrow(results)==0){
              results<-data.frame(timestamp="unknow",Name=i,id=NA,size="unknown",running="no data",time="unknown")
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
  if (input$connect_mysql == 0 || length(open_connections()) == 0) {
    show_error("No open connections found")
  }
  else if (!is.numeric(input$live_last_points)) {
    show_error("Malformed number of entries request")
  }
  else if (input$live_last_points < 0) {
    show_error("Can't request less than zero entries")
  }
  else if (input$app_live_mode && !is.numeric(input$live_update_interval)) {
    show_error("Malformed update interval input")
  }
  else if (input$app_live_mode && input$live_update_interval < 1) {
    show_error("The update interval has to be greater or equal to 1")
  }
  else {
  global$live_mode = input$app_live_mode
  if (global$live_mode) {
      global$live_update_interval = input$live_update_interval
  }
  else {
    global$mysql_data_invalidator = !global$mysql_data_invalidator
    signal_data()
    #    keepalive_data()
    fake_keepalives()
  }
  }
})

observeEvent(input$connect_mysql, {
    open_connections()
    get_info_of_entries()
})

live_invalidator <- observe({
    if (global$live_mode) {
        global$mysql_data_invalidator = !isolate(global$mysql_data_invalidator)
        signal_data()
        fake_keepalives()
        #keepalive_data()
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
            if(is.null(open_connections()[[i]]$conn)) {
              next
            }
            else {
              if(dbIsValid(open_connections()[[i]]$conn)) {
                signals<-suppressWarnings(dbGetQuery(open_connections()[[i]]$conn, build_signals_query(open_connections()[[i]]$table)))
                if(input$global_db_hostname){
                  mysql_query_runs<-paste("SELECT id, device, pos_x, pos_y, orientation, beam_width, center_freq, hostname FROM `runs`")
                }else{
                  mysql_query_runs<-paste("SELECT id, device, pos_x, pos_y, orientation, beam_width, center_freq FROM `runs`")
                }
                runs<-suppressWarnings(dbGetQuery(open_connections()[[i]]$conn,mysql_query_runs))
                if(nrow(signals)>0){
                  results<-merge(signals,runs,by.x="run",by.y="id")
                  results$run <- NULL
                  results$id <- NULL
                  if(input$global_db_hostname){
                    results$Name<-results$hostname
                  }else{
                    results$Name<-i
                  }
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
            if(is.null(open_connections()[[i]]$conn)) {
              next
            }
            else {
              if(dbIsValid(open_connections()[[i]]$conn)) {
                query <- paste0("SELECT timestamp, device FROM `",open_connections()[[i]]$table,"` s INNER JOIN runs r ON r.id = s.run WHERE max_signal = 0 LIMIT ", input$live_last_points, ";")
                results<-suppressWarnings(dbGetQuery(open_connections()[[i]]$conn, query))

                if(nrow(results)>0){
                  results$Name <- i
                  results$receiver <- substrLeft(results$device,17)
                  results$device <- NULL
                  results$timestamp <- as.POSIXct(results$timestamp, tz="UTC")
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
        message = "Loading keepalives: ",
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

# Stopgap measure to allow current plotting to work with data without the proper keepalive structure.
# To be removed once keepalives works the way it does for data folder
fake_keepalives <- function() {
  global$keepalives<-unique(rbind(global$keepalives, data.frame(timestamp=as.POSIXct(character()), Name=character(), receiver=character(), Orientation=numeric(), td=numeric(), td_fctr=as.factor(character()))))
}

build_signals_query <- function(table) {
    query_duration_filter<-""
    query_max_signal_filter<-""
    if(input$check_sql_duration){
      query_duration_filter<-paste(" duration >",input$query_filter_duration[1],"AND duration <",input$query_filter_duration[2])
    }
    if(input$check_sql_strength){
      if(any(input$check_sql_duration)){
        and<-" AND"
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
      inner_join <- " INNER JOIN `runs` r ON s.run = r.id"
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
            query_freq_filter<-paste("",query_freq_filter,")")
          }
      }
      else if (input$query_filter_frequency_type == "Single") {
            k <- input$query_filter_single_frequency
            if(any(input$check_sql_duration,input$check_sql_strength)) {
              and<-"AND("
            }
            query_freq_filter<-paste(query_freq_filter, and, "((signal_freq + center_freq + ",error,") >", k*1000, "  AND (signal_freq + center_freq - ",error,")  <", k*1000, ")")
            if(any(input$check_sql_duration,input$check_sql_strength)){
              query_freq_filter<-paste("",query_freq_filter,")")
            }
      }
    }
    
    where<-""
    
    if (!is.null(input$datetime_filter)) {
      where <- paste0("  WHERE timestamp >= '", input$datetime_filter, "' ")
    }
    
    if(any(input$check_sql_duration,input$check_sql_strength,input$query_filter_freq)){
      if (where!="")
        where<-paste(where, " AND ")
      else
        where<-" WHERE "
    }
    
    #keepalive_filter <- paste(and, "max_signal != 0")
    
    print(paste0("SELECT timestamp, duration, signal_freq, run, max_signal, signal_bw FROM `",table,"` s", inner_join, where, query_duration_filter,query_max_signal_filter,query_freq_filter, " ORDER BY s.id DESC", ifelse(input$live_last_points == 0,"", paste0(" LIMIT ",input$live_last_points)),";"))
}

signal_data<-function(){
  req(get_mysql_data())
  tmp<-get_mysql_data()

  if(is.null(tmp)) return(NULL)
  if(nrow(tmp)==0) return(NULL)
  #tmp<-subset(get_mysql_data(),signal_freq!=0)
  tmp$timestamp <- as.POSIXct(tmp$timestamp,tz="UTC")
  tmp$signal_freq <- round((tmp$signal_freq+tmp$center_freq)/1000, 2)
  tmp$receiver <- substrLeft(tmp$device,17)

  signal_info <- tmp[, c("timestamp", "duration", "signal_freq", "Name", "receiver", "max_signal", "signal_bw")]
  global$signals<-unique.data.frame(rbind(isolate(global$signals), signal_info))

  receiver_info <- tmp[, c("receiver", "Name", "pos_x", "pos_y", "orientation", "beam_width")]
  names(receiver_info) <- c("Name", "Station","Latitude","Longitude", "Orientation", "Beam width")
  global$receivers<-unique.data.frame(rbind(isolate(global$receivers), receiver_info))
  tmp
}

output$live_tab_remote_entries_table <- renderDataTable({
  validate(need(get_info_of_entries(), "No known connections."))
  validate(need(nrow(get_info_of_entries()) > 0, "No connections selected."))
  if (nrow(get_info_of_entries()) == 0) {
    return (NULL)
  }
  tmp <- get_info_of_entries()[, c("Name", "running", "timestamp", "size","time")]
  names(tmp) <- c("Name", "Reachable", "Latest timestamp", "Size (MB)","System Time")
  tmp
}, options = list(pageLength = 10), rownames=F)

output$live_tab_mysql_data <- renderDataTable({
  validate(need(global$signals, "Please load some data"))
  global$signals
}, options = list(pageLength = 10), rownames=F)

output$live_tab_keepalive_plot <- renderPlot({
    validate(need(global$signals, "Please load some data"))
    validate(need(keepalive_data(), "No keepalives found."))
    validate(need(nrow(keepalive_data()) > 0, "No keepalives found."))
    ggplot(keepalive_data()) +
    geom_point(aes(x=timestamp, y=receiver, color=receiver)) +
    labs(x = "Time", y = "Antenna") +
    theme(axis.text.x=element_text(angle = 60, hjust = 1)) +
    scale_x_datetime(labels = function(x) format(x, "%d-%m \n %H:%M:%S"))
})
