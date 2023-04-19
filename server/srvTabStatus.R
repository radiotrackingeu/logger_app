#srvTabStatus.R
# observeEvent(input$stat_add_manual_connection, {
#   if (input$stat_MySQL_name %in% global$connections$Name) {
#     show_error("Could not add connection: name already in use")
#   }
#   else {
#     global$connections <- rbind(global$connections,
#       data.frame(
#         "Name"=input$stat_MySQL_name, 
#         "Host"=input$stat_MySQL_host, 
#         "Database"=input$stat_MySQL_db, 
#         "Table"=input$stat_MySQL_table, 
#         "Port"=input$stat_MySQL_port,
#         "User"=input$stat_MySQL_user,
#         "Password"=input$stat_MySQL_pw,
#         stringsAsFactors=F
#       )
#     )
#   }
# })

output$stat_ui_select <- renderUI({
  selectizeInput(
    "stat_select_connection", 
    multiple=TRUE,
    selected=global$connections$Name,
    label="Connections selection", 
    choices = global$connections$Name
  )
})

stat_response <- eventReactive(input$stat_send_cmd, ignoreInit = T, {
  cmd <- input$stat_select_cmd
  serv <- input$stat_select_service
  recs <- subset(global$connections,Name %in% input$stat_select_connection)
  resp <- NULL
  
  if (!cmd=="status" && length(serv)>1) {
    showNotification("Only command 'status' can be executed for multiple services.", type = "error")
  } else {
    # print(global$connections$Port-16)
    # print(serv)
    # print(cmd)

    resp<-sysdweb_ctr2(
      ports=recs$Port-16,
      services=serv,
      cmd=cmd
    )
  }
  # print(resp)
  return(resp)
})

observeEvent({input$stat_select_cmd; input$stat_select_service; input$stat_select_connection}, ignoreInit = FALSE, ignoreNULL = FALSE, {
  if(any(c(
    is.null(input$stat_select_cmd),
    is.null(input$stat_select_service),
    is.null(input$stat_select_connection)
  ))){
    disable("stat_send_cmd")
  } else {
    enable("stat_send_cmd")
    }
})

output$status_tab_overview <- renderDataTable({
  req(stat_response(), cancelOutput = T)
  stat_response()
})

## TODO change to accept lines out of remoteconnections.xslx
## TODO allow different hosts per port
## nested foreach for one thread per request when making multiple calls to one device
sysdweb_ctr2 <- function(
  host="http://vpn.rteu.me",
  ports=seq(2982,3122,20),
  services="rteu",
  cmd="status",
  user="pi",
  pw="CxmNpiPLqz"){
  # start_time <- Sys.time()
  cl<-parallel::makeCluster(4)
  doParallel::registerDoParallel(cl)
  tmp<- foreach(i = ports, .packages = c("data.table")) %dopar% {
    h <- curl::new_handle()
    curl::handle_setopt(
      handle = h,
      httpauth = 1,
      userpwd = paste0(user,":",pw)
    )
    l<-data.table("cmd"=cmd, "port"=i)
    for (s in services) {
      l[, (s):=tryCatch({
        resp <- curl::curl_fetch_memory(url=paste0(host,":",i,"/api/v1/",s,"/",cmd), handle = h)
        jsonlite::fromJSON(rawToChar(resp$content))[[1]]
      })]
    }
    l
  }
  parallel::stopCluster(cl)
  tmp<-rbindlist(tmp, fill=T)
  # end_time <- Sys.time()
  # print(end_time - start_time)
  return(tmp)
}