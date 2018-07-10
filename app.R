required_packages <- c("shiny", 
                       "shinyjs", 
                       "readxl", 
                       "ggplot2", 
                       "DBI", 
                       "RSQLite", 
                       "RMySQL")

# try to load packages and install missing ones
for (package in required_packages) {
    # require tries to load a package, and returns a boolean indicating success
    if (!require(package, character.only = TRUE)) {
        install.packages(package , dependencies = TRUE)
        require(package, character.only = TRUE)
    }
}

#timestamps to be shown with 3 digits
options(digits.secs = 3)

ui <- tagList(
  useShinyjs(),
  includeCSS("style.css"),
  extendShinyjs("script.js"),
  navbarPage(id = "navbar", "rteu-logger-app v1.1",
    source("ui/uiTabData.R")$value,
    source("ui/uiTabLive.R")$value,
    source("ui/uiTabFilter.R")$value,
    source("ui/uiTabResults.R")$value,
    source("ui/uiTabBearings.R")$value,
    source("ui/uiTabTriangulation.R")$value,
    source("ui/uiTabMap.R")$value,
    source("ui/uiTabSave.R")$value
  )
)

server <- function(input, output, session) {
  source("server/srvTabData.R", local = TRUE)$value
  source("server/srvTabLive.R", local = TRUE)$value
  source("server/srvFileIO.R", local = TRUE)$value
  source("server/srvFilters.R", local = TRUE)$value
  source("server/srvTabFilter.R", local = TRUE)$value
  source("server/srvTabResults.R", local = TRUE)$value
  source("server/srvDoA.R", local = TRUE)$value
  source("server/srvTabBearings.R", local = TRUE)$value
  source("server/srvFunctions.R", local = TRUE)$value
  #source("server/srvTabMap.R", local = TRUE)$value

  onStop(function() {
    close_all_dbs()
    print("DB Connections closed")
  })
}

shinyApp(ui = ui, server = server)
