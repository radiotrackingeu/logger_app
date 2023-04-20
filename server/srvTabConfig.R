#srvTabConfig.R

output$conf_ui_select <- renderUI({
  selectizeInput(
    "conf_select_connection", 
    multiple=TRUE,
    selected=global$connections$Name,
    label="... remote device", 
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
    ret<-tryCatch({
      RCurl::scp(
        host = "sftp@vpn.rteu.me:2779", 
        path="/opt/rteu.json", 
        key = c("./sftp_rsa.pub","./sftp_rsa"), 
        user="sftp", 
        verbose=T, 
        binary = FALSE
      )
    })
    updateAceEditor(session, "conf_editor", value=ret)
  }
})

observeEvent(input$conf_editor, {
  global$config<-input$conf_editor
})

observeEvent(input$conf_uload, ignoreInit = T, {
  tfile<-tempfile(pattern=paste0("rteu_", format(Sys.time(), "%FT%Hh%Mm"),"_"), fileext = ".json")
  fileConn<-file(tfile)
  writeLines(text = input$conf_editor, con = fileConn, sep = "")
  close(fileConn)
  
  recs <- subset(global$connections,Name %in% input$conf_select_connection)
  cl<-parallel::makeCluster(min(detectCores(),nrow(recs)))
  doParallel::registerDoParallel(cl)
  tmp<- foreach(i = iter(recs, by="row"), .export = c("session")) %dopar% {
    ret<-tryCatch({
      RCurl::ftpUpload(
        what = tfile,
        to = paste0("sftp://sftp@", i$Host, ":", i$Port+1, "/home/sftp/rteu.json"),
        verbose = TRUE,
        .opts = list(
          ssh.private.keyfile = "./sftp_rsa", 
          ssh.public.keyfile = "./sftp_rsa.pub"
        ) 
      )
    })
    if (ret>0) {
      showNotification(session = session, ui = HTML("Upload to ", i$Name ," failed. Please download to see current state and try again."), type="error")
    } else {
      showNotification(session = session, ui = HTML("Upload to ", i$Name ," successful."), type="message")
    }
  }
  parallel::stopCluster(cl)
})

output$conf_dl <- downloadHandler(
  filename = function() {
    paste0("rteu_", format(Sys.time(), "%FT%Hh%Mm"), ".json")
  },
  content = function(file) {
    writeLines(text = global$config, con = file, sep = "" )
  },
  contentType = "application/json"
)

observeEvent(input$conf_file, ignoreNULL = T, ignoreInit = T, {
  req(input$conf_file)
  content<-paste0(readLines(input$conf_file$datapath), collapse = "\n")
  updateAceEditor(session, editorId = "conf_editor", value = content)
})