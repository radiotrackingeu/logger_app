############ srvFunctions.R ############

substrLeft <- function(x, n){
  sapply(x, function(xx)
    substr(xx, 0, nchar(xx)-n)
  )
}

close_all_dbs<- function(){
  to_close<-dbListConnections(RMySQL::MySQL())
  for(i in 1:length(to_close)){
    dbDisconnect(to_close[[i]])
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
