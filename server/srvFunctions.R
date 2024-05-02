############ srvFunctions.R ############

substrLeft <- function(x, n){
  sapply(x, function(xx)
    substr(xx, 0, nchar(xx)-n)
  )
}
# 
# close_all_dbs <- function(){
#     all_con <- dbListConnections(MySQL())
# 
#     for (con in all_con) {
#         dbDisconnect(con)
#     }
# }

global$patients_next_id <- 1
global$patients_list <- list()
global$patients_first_passage <- list()

patientReactive <- function(id, delay, update_function) {
    time <- Sys.time()
    global$patients_list[[id]] <- time
    uid <- toString(global$patients_next_id)
    global$patients_next_id <- global$patients_next_id + 1
    global$patients_first_passage[[uid]] <- TRUE

    observer <- observe({
        if (!is.null(isolate(global$patients_first_passage)[[uid]])) {
            isolate(global$patients_first_passage[[uid]] <- NULL)
            invalidateLater(delay)
        }
        else if (isolate(as.double(Sys.time() - global$patients_list[[id]]) * 1000) >= delay) {
            global$patients_list[[id]] <- Sys.time()
            update_function()
        }
        else {
            observer$destroy()
        }
    })
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

#' Kilo, Mega, Giga
#'
#' Convert numbers to SI or IEC format.
#' 
#' @param x numeric
#' @param standard one of "SI" (1000-based) or "IEC" (1024)
#' @param digits number of significant digits to round to
#' @param sep character, what to put between the number and the letter
#' @param suffix character what to put immediately after the K/M/G
#'   (e.g., "b" or "B")
#' @return character
#' @export
KMG <- function(x, standard = "SI", digits = 1L, sep = "", suffix = "") {
  known_bases <- c(legacy = 1024, IEC = 1024, SI = 1000)
  known_units <- list(SI = c("", "k", "M", "G", "T", "P", 
    "E", "Z", "Y"), IEC = c("", "Ki", "Mi", "Gi", 
      "Ti", "Pi", "Ei", "Zi", "Yi"), legacy = c("", "K", 
        "M", "G", "T", "P"))
  standard <- match.arg(standard, c("auto", names(known_bases)))
  if (is.null(digits)) 
    digits <- 1L
  base <- known_bases[[standard]]
  units_map <- known_units[[standard]]
  powers <- rep(0L, length(x))
  powers[x > 0] <- pmin(as.integer(log(x[x > 0], base = base)), length(units_map) - 1L)
  units <- paste0(units_map[powers + 1L], suffix)
  paste(round(x/base^powers, digits = digits), units, sep = sep)
}
