############ tabBearings.R ############
tabPanel("Bearings",
         sidebarLayout(
           sidebarPanel(
             ),
           mainPanel(
             dataTableOutput("angle"),
             plotOutput("doa_plot")
           )
         ))
