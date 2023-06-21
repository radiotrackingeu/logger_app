##### Logger-App A33#####
#########################

# attach packages defined in renv.lock
required_packages <- sort(names(jsonlite::fromJSON("renv.lock")$Packages))
for (package in required_packages) {
  require(package, character.only = TRUE)
}


#timestamps to be shown with 3 digits
options(digits.secs = 3)
options(shiny.maxRequestSize=400000*1024^2)

ui <- tagList(
  useShinyjs(),
  includeCSS("style.css"),
  includeCSS("tooltip.css"),
  extendShinyjs("script.js",functions=c("mark_valid",
                                        "mark_invalid",
                                        "disableTab",
                                        "enableTab",
                                        "disableButton")),
  add_busy_spinner(spin="circle", height = "30px", width = "30px"),
  navbarPage(id = "navbar", "rteu-logger-app A33",
    source("ui/uiTabData.R")$value,
    # source("ui/uiTabStatus.R")$value,
    # source("ui/uiTabConfig.R")$value,
    source("ui/uiTabReceivers.R")$value,
    source("ui/uiTabLive.R")$value,
    source("ui/uiTabFilter.R")$value,
    source("ui/uiTabResults.R")$value,
    source("ui/uiTabBearings.R", encoding = "UTF-8")$value,
    source("ui/uiTabTriangulation.R")$value,
    source("ui/uiTabMap.R")$value,
    source("ui/uiTabSave.R")$value
  )
)

server <- function(input, output, session) {
  ### define reactiveValues to store all data ###
  global <- reactiveValues()

  source("server/srvFunctions.R", local = TRUE)$value
  source("server/srvTabConfig.R", local = TRUE)$value
  source("server/srvTabData.R", local = TRUE)$value
  source("server/srvTabStatus.R", local = TRUE)$value
  source("server/srvTabLive.R", local = TRUE)$value
  source("server/srvFileIO.R", local = TRUE)$value
  source("server/srvFilters.R", local = TRUE)$value
  source("server/srvTabFilter.R", local = TRUE)$value
  source("server/srvTabResults.R", local = TRUE)$value
  source("server/srvResults.R", local = TRUE)$value
  source("server/srvDoA.R", local = TRUE)$value
  source("server/srvTabBearings.R", local = TRUE)$value
  source("server/srvTabTriangulation.R",local = TRUE)$value
  source("server/srvMapFuncs.R",local=TRUE, encoding = "UTF-8")$value
  source("server/srvTriangulation.R",local=TRUE)$value
  source("server/srvTabMap.R", local = TRUE)$value

  onStop(function() {
    close_all_dbs()
    if (exists("cl"))
      stopCluster(cl)
  })
}

shinyApp(ui = ui, server = server)
