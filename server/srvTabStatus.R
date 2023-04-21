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
  recs <- subset(global$connections, Name %in% input$stat_select_connection)
  resp <- NULL
  
  if (!cmd=="status" && length(serv)>1) {
    showNotification("Only command 'status' can be executed for multiple services.", type = "error")
  } else {
    # print(recs$Name)
    # print(serv)
    # print(cmd)

    resp <- poll_sysdbweb(
      hosts = recs,
      services=serv,
      cmd=cmd,
      user = "pi",
      password = "CxmNpiPLqz"
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

poll_sysdbweb <- function(hosts, services, cmd, user, password) {
  dests<-adply(.data = services, .margins = 1, .expand = F, .id = NULL, .fun = function(serv) {
    cbind(hosts, "service"=serv)
  })
  # start_time <- Sys.time()
  cl<-parallel::makeCluster(min(length(dests), detectCores()))
  doParallel::registerDoParallel(cl)
  tmp<-foreach(dest = iter(dests, by="row")) %dopar% {
    h <- curl::new_handle()
    curl::handle_setopt(
      handle = h,
      httpauth = 1,
      userpwd = paste0(user,":",password)
    )
    ret<-tryCatch({
      resp <- curl::curl_fetch_memory(url=paste0(dest$Host,":",dest$Port-16,"/api/v1/",dest$service,"/",cmd), handle = h)
      jsonlite::fromJSON(rawToChar(resp$content))[[1]]
    })
    cbind(dest,"response"=ret)
  }
  tmp<-rbindlist(tmp, fill=T)
  parallel::stopCluster(cl)
  # end_time <- Sys.time()
  # print(end_time - start_time)
  return(tmp)
}
