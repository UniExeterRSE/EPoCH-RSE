source("plot.R")

## when the user clicks "visualise results"...
observeEvent(input$plot_data,{
  ## if the data hasn't been loaded, they get an error message...
#  if(exists("global_data$data")==F){
#    showModal(modalDialog("No data has been loaded."))
#    reset("plot_data")
#  }
  ## but if it HAS been loaded, they get a message saying that the visualisations are being generated...
    #showModal(modalDialog("Generating visualisations"))
  ## then the visualisations appear...
    ## a manhattan plot

output$exposureManhattanPlot <- renderPlotly({
    model <- global_data$df_models$shortname[global_data$df_models$name == input$model_choice]
    dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]
    print(dat)
    exp_df <- create_exposure_dfs(tolower(input$exposure_choice),dat)
    #print(exp_df$person_exposed)
    create_manhattan_plot(exp_df)
  })
    ## and a volcano plot
output$exposureVolcanoPlot <- renderPlotly({
    model <- global_data$df_models$shortname[global_data$df_models$name == input$model_choice]
    dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]
    print(dat)
    exp_df <- create_exposure_volcano_dfs(tolower(input$exposure_choice),dat)
    create_exposure_volcano_plot(exp_df)
})

})