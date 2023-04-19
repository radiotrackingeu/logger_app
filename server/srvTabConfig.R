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