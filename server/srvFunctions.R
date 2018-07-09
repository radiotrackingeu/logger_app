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
