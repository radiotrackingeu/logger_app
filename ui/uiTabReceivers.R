#uiTabReceivers.R
tabPanel("Receivers",
  tabsetPanel(
    source("ui/uiTabStatus.R")$value,
    source("ui/uiTabConfig.R")$value
  )
)