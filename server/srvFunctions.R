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
