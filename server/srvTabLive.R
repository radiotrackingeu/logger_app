############ srvTabLive.R ############

### render ui elements ###

single_multiple_con_tags <- reactive({
  if(input$live_data_number=="Single"){
    forUI<-tagList(
      textInput(
        "MySQL_host",
        "Enter Host Name",
        "192.168.1.1"
      ),
      numericInput(
        "MySQL_port",
        "Enter Port",
        3306
      ),
      textInput(
        "MySQL_user",
        "Enter User Name",
        "rteu"
      ),
      passwordInput(
        "MySQL_pw",
        "Enter Password",
        "rteuv2!"
      ),
      actionButton(
        "connect_to_db",
        "Load Data from DB"
      ),
      br(),
      br()
    )
  }
  if(input$live_data_number=="Multiple"){
    forUI<-tagList(
      selectizeInput("select_connection", multiple=TRUE,selected="all",label="Please select connections",choices = c("all",global$connections$Name))
    )
  }
  return(forUI)
})

output$single_multiple_con_tags <- renderUI({
  single_multiple_con_tags()
})

current_connections_ids <- reactive({
    requested_connections <- input$select_connection
    if ("all" %in% requested_connections) {
        requested_connections <- global$connections$Name
    }
    requested_connections_id = c(1:nrow(global$connections))[global$connections$Name %in% requested_connections]
})

are_current_connections_open <- function() {
    connections_ids <- current_connections_ids()

    for (i in 1:length(connections_ids)) {
        current_connection <- connections_ids[i]
        if (is.null(open_connections()[current_connection])) {
            return (FALSE)
        }
    }

    TRUE
}

open_connections <- reactive({
  close_all_dbs()
  input$connect_mysql
  tmp_list<-list()

  connections_ids = isolate(current_connections_ids())
  connections_count = length(connections_ids)

  if(connections_count > 0 && input$connect_mysql){
    withProgress(
      expr = {
        for(i in 1:connections_count){
          current_connection = global$connections[connections_ids[i], ]
          setProgress(detail=current_connection$Name)
          tmp_list[[current_connection$Name]]<-tryCatch(
            dbConnect(
              drv=RMySQL::MySQL(),
              dbname = "rteu",
              host = current_connection$Host,
              port = current_connection$Port,
              username = current_connection$User,
              password = current_connection$Password
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
      max = connections_count,
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
    withProgress(
        expr = { for(i in global$connections$Name){
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
      max = nrow(global$connections),
      value = 0
    )
  }else{
    tmp<-NULL
  }
  return(tmp)
})

observe({
    if (input$load_mysql_data) {
        if (!are_current_connections_open()) {
            show_error("you need to connect to the selected databases first.")
            return (NULL)
        }

        updateTabsetPanel(session, "live_tab_tabset", selected = "List of Data")
    }
})

get_mysql_data <- reactive({
    if (!are_current_connections_open()) {
        return (NULL)
    }

  if(!is.null(get_info_of_entries())&&input$load_mysql_data){
    tmp<-data.frame()
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

    if(!is.null(get_info_of_entries())){
        connections <- current_connections_ids()
        connections_count <- length(connections)

        withProgress(
            expr = {
                for(i in connections) {
                    connection = open_connections()[[i]]
                    connection_info = get_info_of_entries()[i, ]

                    setProgress(detail = connection_info$Name)
                            query_freq_filter<-""
                            if(input$query_filter_freq){
                                for(k in global$frequencies$Frequency){
                                    error<-2000
                                    center<-150120000
                                    and<-""
                                    if(any(input$check_sql_duration,input$check_sql_strength)) {
                                        and<-"AND("
                                    }
                                    if(nrow(global$frequencies)>1&&query_freq_filter!=""){
                                        and<-"OR"
                                    }
                                    query_freq_filter<-paste(query_freq_filter,and,"(signal_freq >",k*1000-error-center,"AND signal_freq <",k*1000+error-center,")")
                                }
                                if(any(input$check_sql_duration,input$check_sql_strength)){
                                    query_freq_filter<-paste(query_freq_filter,")")
                                }
                            }
                            where<-""
                            if(any(input$check_sql_duration,input$check_sql_strength,input$query_filter_freq)){
                                where<-"WHERE"
                            }
                            mysql_query_signals <- paste("SELECT * FROM `signals`",where,query_duration_filter,query_max_signal_filter,query_freq_filter,"ORDER BY id DESC LIMIT",input$live_last_points,";")

                            signals <- dbGetQuery(connection ,mysql_query_signals)
                            mysql_query_runs<-paste("SELECT * FROM `runs` ORDER BY id DESC LIMIT",input$live_last_points,";")

                            runs<-dbGetQuery(connection, mysql_query_runs)

                            if(nrow(signals)>0){
                                results<-merge(signals,runs,by.x="run",by.y="id")
                                results$Name<-connection_info$Name
                                tmp<-rbind(tmp,results)
                            }
                        }
                    incProgress(amount=1)
                },
            message = "Loading data: ",
            max = connections_count,
            value = 0
        )
    }
    else{
      tmp<-NULL
    }
    global$tmp_data <- tmp
  }
})

signal_data<-reactive({
  tmp<-get_mysql_data()
  if(is.null(tmp)) return(NULL)
  if(nrow(tmp)==0) return(NULL)
  #tmp<-subset(get_mysql_data(),signal_freq!=0)
  tmp$timestamp<-as.POSIXct(tmp$timestamp)
  tmp$signal_freq<-round((tmp$signal_freq+tmp$center_freq)/1000)
  global$signals<-unique.data.frame(rbind(cbind(tmp,receiver = substrLeft(tmp$device,17)),global$signals))
  tmp
})

keepalive_data<-reactive({
  tmp<-get_mysql_data()
  if(is.null(tmp)) return(NULL)
  if(nrow(tmp)==0) return(NULL)
  tmp<-subset(get_mysql_data, signal_freq==0)
  tmp$timestamp<-as.POSIXct(tmp$timestamp)
  data.frame(Timestamp = tmp$timestamp,Station = tmp$Name, Receiver = substrLeft(tmp$device,17))
})

output$live_tab_remote_entries_table <- renderDataTable({
  validate(need(get_info_of_entries(), "Please provide remote connection data file."))
  tmp <- get_info_of_entries()[, c("Name", "running", "timestamp", "size")]
  names(tmp) <- c("Name", "Reachable", "Latest timestamp", "Size (MB)")
  tmp
}, options = list(pageLength = 10))

output$live_tab_mysql_data <- renderDataTable({
  validate(need(signal_data(), "Please check connections first"))
  signal_data()[c("Name", "timestamp", "samples", "duration", "signal_freq", "signal_bw", "max_signal")]
}, options = list(pageLength = 10))

output$live_tab_keepalive<- renderDataTable({
  validate(need(keepalive_data(), "Please check connections first"))
  keepalive_data()
}, options = list(pageLength = 10))
