############ srvTabLive.R ############

### render ui elements ###

observeEvent(input$connect_to_db, {
    new_connection <- data.frame(input$MySQL_name, input$MySQL_host, input$MySQL_port, input$MySQL_user, input$MySQL_pw)
    names(new_connection) <- c("Name", "Host", "Port", "User", "Password")
    global$connections <- rbind(global$connections, new_connection)
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
              show_error(err[1]);
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
          results<-data.frame(Name=i,id=NA,timestamp="unknown",size="unknown",running="unknown")
          tmp<-rbind(tmp,results)
          next
        }
        else {
          if(dbIsValid(open_connections()[[i]])) {
            results<-dbGetQuery(open_connections()[[i]],"SELECT id,timestamp FROM `signals` ORDER BY id DESC LIMIT 1;")
            results$size <- dbGetQuery(open_connections()[[i]], '
                                       SELECT ROUND(SUM(data_length + index_length) / 1024 / 1024, 1) "size"
                                       FROM information_schema.tables;
                                       ')$size
            if(as.POSIXlt(Sys.time(), tz="UTC")-as.POSIXlt(results$timestamp, tz="UTC")<360){
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

observe({
  input$load_mysql_data
  signal_data()
})

get_mysql_data <- eventReactive(input$load_mysql_data,{
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

build_signals_query <- reactive({
    query_duration_filter<-""
    query_max_signal_filter<-""
    if(input$check_sql_duration){
      query_duration_filter<-paste("duration >",input$query_filter_duration[1],"AND duration <",input$query_filter_duration[2])
    }
    if(input$check_sql_strength){
      if(any(input$check_sql_duration)){
        and<-"AND"
      }
      query_max_signal_filter<-paste(and,"max_signal >",input$query_filter_strength[1],"AND max_signal <",input$query_filter_strength[2])
    }

    query_freq_filter<-""
    inner_join <- ""
    if(input$query_filter_freq){
      for(k in global$frequencies$Frequency){
        error<-2000
        and<-""
        if(any(input$check_sql_duration,input$check_sql_strength)) {
          and<-"AND("
        }
        if(nrow(global$frequencies)>1&&query_freq_filter!=""){
          and<-"OR"
        }
        inner_join <- "INNER JOIN `runs` r ON s.run = r.id"
        query_freq_filter<-paste(query_freq_filter, and, "((signal_freq + center_freq + ",error,") >", k*1000, "  AND (signal_freq + center_freq - ",error,")  <", k*1000, ")")
      }
      if(any(input$check_sql_duration,input$check_sql_strength)){
        query_freq_filter<-paste(query_freq_filter,")")
      }
    }
    where<-""
    if(any(input$check_sql_duration,input$check_sql_strength,input$query_filter_freq)){
      where<-"WHERE"
    }

    paste("SELECT timestamp, duration, signal_freq, run, max_signal FROM `signals` s", inner_join, where,query_duration_filter,query_max_signal_filter,query_freq_filter,"ORDER BY s.id DESC LIMIT",input$live_last_points,";")
})

signal_data<-reactive({
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
})

keepalive_data<-reactive({
  tmp<-get_mysql_data()
  if(is.null(tmp)) return(NULL)
  if(nrow(tmp)==0) return(NULL)
  tmp<-subset(tmp, signal_freq==0)
  tmp$timestamp<-as.POSIXct(tmp$timestamp)
  data.frame(Timestamp = tmp$timestamp,Station = tmp$Name, Receiver = substrLeft(tmp$device,17))
})

output$live_tab_remote_entries_table <- renderDataTable({
  validate(need(get_info_of_entries(), "Please provide remote connection data file."))
  if(is.null(get_info_of_entries())) return(NULL)
  tmp <- get_info_of_entries()[, c("Name", "running", "timestamp", "size")]
  names(tmp) <- c("Name", "Reachable", "Latest timestamp", "Size (MB)")
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
