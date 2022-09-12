
# 
# observeEvent(input$slider_datetime, {
#     patientReactive("slider_datetime", 1000, redraw_results_plot)
# })
# 
# global$invalidate_filtered_data <- 0

filtered_data_td <- reactive({
  validate(need(input$filter_type!="all", "Please use 'Filter' tab to specify one or more frequencies, by using either of the 'Multiple frequencies' or 'Custom frequency' options."))
  return(calculate_delta_T(filtered_data()))
})

output$plot_x_y <- renderText({
  if(is.null(input$plot_hover$x)) return(NULL)
  return(paste0("Time: ", round(as.POSIXct(input$plot_hover$x,origin="1970-01-01", "UTC")),"\n Strength: ",input$plot_hover$y))
})

#Temperature would be great here

output$facet <- renderPlot({
  validate(need(filtered_data(), "No data loaded"))
  validate(need(nrow(filtered_data())>0, "Oh no, there is no data to plot! Did you filter it all out?"))
  switch(input$choose_plot,
         'Time-Strength-Antenna-Station'={
           get_base_plot(filtered_data(), style="white") +
           # ggplot(filtered_data()) +
           geom_point(aes(x=as.POSIXct(timestamp, "UTC"), y=max_signal, color=receiver)) +
           labs(x="Date and Time in UTC", y = "Signal Strength") +
           scale_x_datetime(labels = function(x) format(x, "%d-%m \n %H:%M:%S"))+
           facet_wrap(~Name) +
           guides(color=guide_legend(override.aes = list(size=3, alpha=1)))
         },
         'Time-Strength-Antenna-Station-Freq'={
           # ggplot(filtered_data()) +
           get_base_plot(filtered_data_td(), style="white") +
             geom_point(aes(x=as.POSIXct(timestamp, "UTC"), y=max_signal, color=receiver,group=freq_tag,shape=freq_tag)) +
             labs(x="Date and Time in UTC", y = "Signal Strength") +
             scale_x_datetime(labels = function(x) format(x, "%d-%m \n %H:%M:%S"))+
             facet_wrap(~Name) +
             guides(color=guide_legend(override.aes = list(size=3, alpha=1)), shape=guide_legend(override.aes = list(size=3)))
         },
         'Time-Freq-Strength-Station'={
           # ggplot(filtered_data()) +
           get_base_plot(filtered_data(), style="white") +
             geom_point(aes(x=timestamp, y=signal_freq, color=max_signal)) +
             labs(x="Date and Time in UTC", y = "Frequency") +
             scale_x_datetime(labels = function(x) format(x, "%d-%m \n %H:%M:%S"))+
             facet_wrap(~Name)
         },
         'Time-Strength-Frequency-Station'={
           # ggplot(filtered_data()) +
           get_base_plot(filtered_data_td(), style="white") +
             geom_point(aes(x=as.POSIXct(timestamp, "UTC"), y=max_signal, color=freq_tag)) +
             labs(x="Date and Time in UTC", y = "Signal Strength") +
             scale_x_datetime(labels = function(x) format(x, "%d-%m \n %H:%M:%S"))+
             facet_wrap(~Name) +
             guides(color=guide_legend(override.aes = list(size=3, alpha=1)))
         },
         'Time-Temperature-Station-Frequency'={
           ggplot(filtered_data_td())+
             geom_point(aes(x=as.POSIXct(timestamp, "UTC"), y=temperature,color=Name))+
             labs(x="Date and Time in UTC", y = "Temperature [C]") +
             ylim(27.4,40)+
             facet_wrap(~freq_tag) +
             guides(color=guide_legend(override.aes = list(size=3, alpha=1)))
         },
         'Time-TD-Station-Frequency'={
           ggplot(filtered_data_td())+
             geom_point(aes(x=as.POSIXct(timestamp, "UTC"), y=td,color=freq_tag))+
             ylab("Time difference [s]")+
             xlab("Date and Time in UTC")+
             scale_x_datetime(labels = function(x) format(x, "%d-%m \n %H:%M:%S"))+
             facet_wrap(~Name)+
             ylim(0.0,4.5) +
             guides(color=guide_legend(override.aes = list(size=3, alpha=1)))
         }
         )
})