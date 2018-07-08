############ tabFilters.R ############
tabPanel("Results",
         column(3,
                selectInput("select_x","Choose x-Achis", choices = c("timestamp","max_signal","signal_freq","receiver","Name","freq_tag"),selected="timestamp")
                ),
         column(3,
                selectInput("select_y","Choose y-Achis", choices = c("timestamp","max_signal","signal_freq","receiver","Name","freq_tag"),selected="max_signal")
         ),
         column(3,
                selectInput("select_col","Choose Colour", choices = c("timestamp","max_signal","signal_freq","receiver","Name","freq_tag"),selected="receiver")
         ),
         column(3,
                selectInput("select_facet","Choose Facet", choices = c("timestamp","max_signal","signal_freq","receiver","Name","freq_tag"),selected="Name")
         ),
         plotOutput("facet")
         )
