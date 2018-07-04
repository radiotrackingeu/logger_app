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
      selectizeInput("select_connection", multiple=TRUE,selected="all",label="Please select connections",choices = c("all",remote_connections()$Name))
    )
  }
  return(forUI)
})

output$single_multiple_con_tags <- renderUI({
  single_multiple_con_tags()
})

open_connections <- reactive({
  tmp_list<-list()
  if(!is.null(remote_connections())&&input$load_mysql_data){
    withProgress(
      expr = {
        for(i in 1:nrow(remote_connections())){
          setProgress(detail=remote_connections()$Name[i])
          tmp_list[[remote_connections()$Name[i]]]<-tryCatch(
            dbConnect(
              drv=RMySQL::MySQL(),
              dbname = "rteu",
              host = remote_connections()$Host[i],
              port = remote_connections()$Port[i],
              username = remote_connections()$User[i],
              password = remote_connections()$Password[i]
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
      max = nrow(remote_connections()),
      value = 0
    )
  }else{
    tmp_list<-NULL
  }
  return(tmp_list)
})

get_info_of_entries <- reactive({
  tmp<-data.frame()
  if(!is.null(remote_connections())){
    for(i in remote_connections()$Name){
      if(is.null(open_connections()[[i]])){
        results<-data.frame(Name=i,id=NA,timestamp="offline")
        tmp<-rbind(tmp,results)
        next
      }else{
        if(dbIsValid(open_connections()[[i]])){
          results<-dbGetQuery(open_connections()[[i]],"SELECT id,timestamp FROM `signals` ORDER BY id DESC LIMIT 1;")
          if(nrow(results)==0){
            results<-data.frame(id = 0 , timestamp="logger not running")
          }
          print(results)
          results$Name<-i
          tmp<-rbind(tmp,results)
        }else{
          results<-data.frame(Name=i,id=NA,timestamp="offline")
          tmp<-rbind(tmp,results)
        }
      }
    }
  }else{
    tmp<-NULL
  }
  return(tmp)
})

get_mysql_data <- reactive({
  tmp<-data.frame()
  query_duration_filter<-""
  query_max_signal_filter<-""
  if(input$query_filter_duration){
    query_duration_filter<-paste("duration >",input$filter_duration[1],"AND duration <",input$filter_duration[2])
  }
  if(input$query_filter_max_signal){
    if(any(input$query_filter_duration)){
      and<-"AND"
    }
    query_max_signal_filter<-paste(and,"max_signal >",input$filter_max_signal[1],"AND max_signal <",input$filter_max_signal[2])
  }
  
  if(!is.null(get_info_of_entries())){
    for(i in get_info_of_entries()[get_info_of_entries()$timestamp!="offline",]$Name){
      if(is.null(open_connections()[[i]])){
        results<-data.frame(Name=i,id=NA,timestamp="offline")
        tmp<-rbind(tmp,results)
        next
      }else{
        if(dbIsValid(open_connections()[[i]])){
          query_freq_filter<-""
          if(input$query_filter_freq){
            for(k in frequencies_list()$Frequency){
              error<-2000
              center<-150120000
              and<-""
              if(any(input$query_filter_duration,input$query_filter_max_signal)){
                and<-"AND("
              }
              if(nrow(frequencies_list()>1)&&query_freq_filter!=""){
                and<-"OR"
              }
              query_freq_filter<-paste(query_freq_filter,and,"(signal_freq >",k*1000-error-center,"AND signal_freq <",k*1000+error-center,")")
            }
            if(any(input$query_filter_duration,input$query_filter_max_signal)){
              query_freq_filter<-paste(query_freq_filter,")")
            }
          }
          where<-""
          if(any(input$query_filter_duration,input$query_filter_max_signal,input$query_filter_freq)){
            where<-"WHERE"
          }
          mysql_query_signals<-paste("SELECT * FROM `signals`",where,query_duration_filter,query_max_signal_filter,query_freq_filter,"ORDER BY id DESC LIMIT",input$live_last_points,";")
          signals<-dbGetQuery(open_connections()[[i]],mysql_query_signals)
          mysql_query_runs<-paste("SELECT * FROM `runs` ORDER BY id DESC LIMIT",input$live_last_points,";")
          runs<-dbGetQuery(open_connections()[[i]],mysql_query_runs)
          if(nrow(signals)>0){
            results<-merge(signals,runs,by.x="run",by.y="id")
            results$Name<-i
            tmp<-rbind(tmp,results)
          }
        }else{
          results<-data.frame(Name=i,id=NA,timestamp="offline")
          tmp<-rbind(tmp,results)
        }
      }
    }
  }else{
    tmp<-NULL
  }
  return(tmp)
})

signal_data<-reactive({
  tmp<-get_mysql_data()
  tmp<-subset(get_mysql_data(),signal_freq!=0)
  tmp$timestamp<-as.POSIXct(tmp$timestamp)
  data.frame(Timestamp = tmp$timestamp, Frequency = round((tmp$signal_freq+tmp$center_freq)/1000), Station = tmp$Name, Strength = tmp$max_signal, Duration = tmp$duration, Bandwidth = tmp$signal_bw, Longitude = tmp$pos_x, Latitude = tmp$pos_y, Orientation = tmp$orientation, Receiver = substrLeft(tmp$device,17))
})

keepalive_data<-reactive({
  tmp<-get_mysql_data()
  tmp<-subset(get_mysql_data(),signal_freq==0)
  tmp$timestamp<-as.POSIXct(tmp$timestamp)
  data.frame(Timestamp = tmp$timestamp,Station = tmp$Name, Receiver = substrLeft(tmp$device,17))
})

output$live_tab_remote_entries_table <- renderDataTable({
  validate(need(get_info_of_entries(), "Please provide remote connection data file."))
  get_info_of_entries()
}, options = list(pageLength = 10))

output$live_tab_mysql_data <- renderDataTable({
  validate(need(signal_data(), "Please check connections first"))
  signal_data()
}, options = list(pageLength = 10))

output$live_tab_keepalive<- renderDataTable({
  validate(need(keepalive_data(), "Please check connections first"))
  keepalive_data()
}, options = list(pageLength = 10))
