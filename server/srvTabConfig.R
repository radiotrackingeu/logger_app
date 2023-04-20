#srvTabConfig.R

output$conf_ui_select <- renderUI({
  selectizeInput(
    "conf_select_connection", 
    multiple=TRUE,
    selected=global$connections$Name,
    label="Connections selection", 
    choices = global$connections$Name
  )
})

observeEvent(input$conf_select_connection, ignoreInit = F, ignoreNULL = F, {
  if(is.null(input$conf_select_connection) || length(input$conf_select_connection)>1)
    disable("conf_dload")
  else
    enable("conf_dload")
  if(is.null(input$conf_select_connection))
    disable("conf_uload")
  else
    enable("conf_uload")
})

observeEvent(input$conf_dload, ignoreNULL = T, ignoreInit = T, {
  recs <- subset(global$connections,Name %in% input$conf_select_connection)
  
  if (nrow(recs)>1) {
    showNotification("Can only download from one device at a time.", type = "error")
  } else {
    global$config <- RCurl::getURL(
      url = paste0("sftp://", recs$Host, "/opt/rteu.json"), 
      port = recs$Port+1, 
      ssh.private.keyfile = "./sftp_rsa", 
      ssh.public.keyfile = "./sftp_rsa.pub", 
      username = "sftp", 
      verbose = FALSE
    )    
  }
})

observeEvent(global$config, ignoreInit = T, {
  updateAceEditor(session = session, editorId = "conf_editor", value = global$config)
})

