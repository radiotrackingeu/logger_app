############ srvFunctions.R ############

substrLeft <- function(x, n){
  sapply(x, function(xx)
    substr(xx, 0, nchar(xx)-n)
  )
}

close_all_dbs <- function(){
    all_con <- dbListConnections(MySQL())

    for (con in all_con) {
        dbDisconnect(con)
    }
}

global$patients_first_passage <- list()
global$patients_list <- list()
global$patients_next_id <- 1
global$patients_call_next_id <- 1

patientReactive <- function(event, delay, update_function) {
    id <- reactive(global$patients_next_id)
    global$patients_next_id <- reactive(global$patients_next_id + 1)

    observeEvent(event, {
        patientReactive_register(toString(id), delay, update_function)
    })
}

patientReactive_register <- function(id, delay, update_function) {
    time <- Sys.time()
    global$patients_list[[id]] <- time
    uid <- global$patients_call_next_id
    global$patients_call_next_id <- global$patients_call_next_id + 1

    patientReactive_update(id, delay, update_function, toString(uid))
}

patientReactive_update <- function(id, delay, update_function, uid) {
    global$patients_first_passage[[uid]] <- TRUE
    val <- observe({
        if (!is.null(isolate(global$patients_first_passage)[[uid]])) {
            isolate(global$patients_first_passage[[uid]] <- NULL)
            invalidateLater(delay)
        }
        else if (isolate(as.double(Sys.time() - global$patients_list[[id]]) * 1000) >= delay) {
            global$patients_list[[id]] <- Sys.time()
            update_function()
        }
    })
    val <- NULL
}

show_error <- function(message) {
    show_notification(message, "error")
}

show_warning <- function(message) {
    show_notification(message, "warning")
}

show_message <- function(message) {
    show_notification(message, "message")
}

show_notification <- function(message, type) {
    showNotification(message, type = type, duration = 3)
    NULL
}
