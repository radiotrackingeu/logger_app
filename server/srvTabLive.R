############ srvTabLive.R ############

### render ui elements ###
observeEvent(input$add_manual_connection, {
    if (input$MySQL_name %in% global$connections$Name) {
        show_error("Could not add connection: name already in use")
    }
    else {
    global$connections <- rbind(global$connections,
      data.frame("Name"=input$MySQL_name, "Host"=input$MySQL_host, "Database"=input$MySQL_db, "Table"=input$MySQL_table, "Port"=input$MySQL_port,"User"=input$MySQL_user,"Password"=input$MySQL_pw,stringsAsFactors=F)
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
          database = connect_to$Database[i]
          database = ifelse(!is.character(database),"rteu",database)
          tmp_list[[connect_to$Name[i]]] <- list("conn"=open_connection(connect_to[i, ]), "table"=table, "database"=database)
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
      drv = RMariaDB::MariaDB(),
      dbname = ifelse(!is.null(connection_info$Database), connection_info$Database, "rteu"),
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
            results<-suppressWarnings(dbGetQuery(open_connections()[[i]]$conn,paste0("SELECT timestamp FROM `",open_connections()[[i]]$table,"` ORDER BY timestamp DESC LIMIT 1;")))
            results$timestamp <- as.character(results$timestamp)
            if(nrow(results)>0){
              results$size <- suppressWarnings(dbGetQuery(open_connections()[[i]]$conn, paste0('
                                       SELECT ROUND(SUM(data_length + index_length) / 1024 / 1024, 1) "size"
                                       FROM information_schema.tables WHERE table_schema = "', open_connections()[[i]]$database, '" AND table_name = "', open_connections()[[i]]$table, '";')
                                       )$size)
              results$time <- suppressWarnings(dbGetQuery(open_connections()[[i]]$conn, 'SELECT NOW();')$'NOW()')
              if(abs(difftime(as.POSIXct(Sys.time(), tz="UTC"),as.POSIXct(results$timestamp, tz="UTC"),units="mins"))<6){
                results$running<-"Recording"
              }else{
                results$running<-"Not recording"
              }
              }
            if(nrow(results)==0){
              results<-data.frame(timestamp="unknown",Name=i,id=NA,size="unknown",running="no data",time="unknown")
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
    keepalive_data()
    # fake_keepalives()
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
        # fake_keepalives()
        keepalive_data()
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
                tryCatch({
                  signals<-RMariaDB::dbGetQuery(open_connections()[[i]]$conn, build_signals_query(open_connections()[[i]]$table))
                  signals<- signals %>% filter(signal_freq!=0)
                  if(input$global_db_hostname){
                    mysql_query_runs<-paste("SELECT id, device, latitude, longitude, orientation, center_freq, hostname FROM `runs`")
                  }else{
                    mysql_query_runs<-paste("SELECT id, device, latitude, longitude, orientation, center_freq FROM `runs`")
                  }
                  runs<-dbGetQuery(open_connections()[[i]]$conn,mysql_query_runs)
                },
                error = function(err) {
                  showNotification(session, HTML("Error getting data:<br>", err[1]), type = "error")
                }
              )
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
    # global$tmp_data <- tmp
    tmp
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
                query <- paste0("SELECT k.timestamp, device FROM `keepalives` k INNER JOIN runs r ON r.id = k.run ")
                if (!is.null(input$datetime_filter)) {
                  query <- paste0(query, "WHERE k.timestamp >= '", input$datetime_filter, "' ")
                }
                if (!input$live_last_points==0)
                  query <- paste0(query, "LIMIT ", input$live_last_points, ";")
                else 
                  query <- paste0(query,";")
                print(query)
                results<-suppressWarnings(dbGetQuery(open_connections()[[i]]$conn, query))

                if(nrow(results)>0){
                  results$Name <- i
                  results$receiver <- results$device#substrLeft(results$device,17)
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
    query_tag_filter<-""
    if(input$check_sql_duration){
      query_duration_filter<-paste(" duration >",input$query_filter_duration[1],"AND duration <",input$query_filter_duration[2])
    }
    if(input$check_sql_strength){
      if(any(input$check_sql_duration)){
        and<-" AND"
      }else{
        and<-""
      }
      query_max_signal_filter<-paste(and, "max_signal >=",input$query_filter_strength[1],"AND max_signal <=",input$query_filter_strength[2])
    }
    
    if(input$check_sql_tag) {
      and<-""
      if(any(input$check_sql_duration,input$check_sql_strength)) {
        and<-" AND "
      }
      query_tag_filter <- paste0(and, " freq_tag IN ('", paste0(input$query_filter_tag, collapse = "', '"), "') ")
    }

    query_freq_filter<-""
    inner_join <- ""
    if (input$query_filter_freq){
      error <- input$freq_error
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
            query_freq_filter<-paste(query_freq_filter, and, "(signal_freq between (", k,"-", error,") AND (", k, "+", error, "))")
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
            query_freq_filter<-paste(query_freq_filter, and, "(signal_freq between (", k,"-", error,") AND (", k, "+", error, "))")
            if(any(input$check_sql_duration,input$check_sql_strength)){
              query_freq_filter<-paste("",query_freq_filter,")")
            }
      }
    }
    
    where<-""
    
    if (!is.null(input$datetime_filter)) {
      where <- paste0("  WHERE timestamp >= '", input$datetime_filter, "' ")
    }
    
    if(any(input$check_sql_duration,input$check_sql_strength,input$query_filter_freq, input$check_sql_tag)){
      if (where!="")
        where<-paste(where, " AND ")
      else
        where<-" WHERE "
    }
    
    #keepalive_filter <- paste(and, "max_signal != 0")
    
    print(paste0("SELECT timestamp, duration, ", ifelse(input$check_sql_tag, "freq_tag, ", ""), "signal_freq, run, max_signal, signal_bw FROM `",table,"` s", inner_join, where, query_duration_filter,query_max_signal_filter,query_freq_filter, query_tag_filter," ORDER BY s.timestamp DESC", ifelse(input$live_last_points == 0,"", paste0(" LIMIT ",input$live_last_points)),";"))
}

signal_data<-function(){
  req(get_mysql_data())
  tmp<-get_mysql_data()

  if(is.null(tmp)) return(NULL)
  if(nrow(tmp)==0) return(NULL)
  #tmp<-subset(get_mysql_data(),signal_freq!=0)
  tmp$timestamp <- as.POSIXct(tmp$timestamp,tz="UTC")
  # tmp$signal_freq <- round((tmp$signal_freq), 2)
  tmp$receiver <- tmp$device#substrLeft(tmp$device,17)
  if(nrow(tmp)>0){
    #### Steinkauz ####
    setDT(tmp)
    if ("freq_tag" %in% names(tmp))
      tmp[, freq_tag:=NULL]
    tmp[Name %in% c("rteu-50","rteu-51"), Name:="rteu-50/51"]
    tmp[Name %in% c("rteu-52","rteu-53"), Name:="rteu-52/53"]
  }
  signal_info <- tmp[, c("timestamp", "duration", "signal_freq", "Name", "receiver", "max_signal", "signal_bw")]
  global$signals<-unique.data.frame(rbind(isolate(global$signals), signal_info))

  receiver_info <- tmp[, c("receiver", "Name", "latitude", "longitude", "orientation")]
  names(receiver_info) <- c("Name", "Station","Latitude","Longitude", "Orientation")
  global$receivers<-unique.data.frame(rbind(isolate(global$receivers), receiver_info, fill=T))
  tmp
}

output$live_tab_remote_entries_table <- renderDataTable({
  shiny::validate(need(get_info_of_entries(), "No known connections."))
  shiny::validate(need(nrow(get_info_of_entries()) > 0, "No connections selected."))
  if (nrow(get_info_of_entries()) == 0) {
    return (NULL)
  }
  tmp <- get_info_of_entries()[, c("Name", "running", "timestamp", "size","time")]
  names(tmp) <- c("Name", "Reachable", "Latest timestamp", "Size (MB)","System Time")
  tmp
}, options = list(pageLength = 10), rownames=F)

output$live_tab_mysql_data <- renderDataTable({
  shiny::validate(need(global$signals, "Please load some data"))
  global$signals
}, options = list(pageLength = 10), rownames=F)

output$live_tab_keepalive_plot <- renderPlot({
    shiny::validate(need(global$signals, "Please load some data"))
    shiny::validate(need(keepalive_data(), "No keepalives found."))
    shiny::validate(need(nrow(keepalive_data()) > 0, "No keepalives found."))
    ggplot(keepalive_data()) +
    geom_point(aes(x=timestamp, y=receiver, color=receiver), size=0.2) +
    labs(x = "Time", y = "Antenna") +
    theme(axis.text.x=element_text(angle = 60, hjust = 1), legend.position = "none") +
    scale_x_datetime(labels = function(x) format(x, "%d-%m \n %H:%M:%S"))
})

observeEvent(global$frequencies, {
  updateSelectInput(session=session, "query_filter_tag", choices = global$frequencies$Name)
})
