
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

print(global_data$df_models)

output$exposureManhattanPlot <- renderPlotly({
    model <- global_data$df_models$shortname[global_data$df_models$name == input$model]
    model = "model1a"
    print("dat")
    print(global_data$data$all_res[which(global_data$data$all_res$model==model),])
    dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]

    print(dat)
    exp_class="smoking"

    df <- create_exposure_manhattan_dfs(exp_class,dat)
    print(df$person_exposed)
    create_exposure_manhattan_plot(df)
  })
    ## and a volcano plot
output$exposureVolcanoPlot <- renderPlotly({
  #model <- global_data$df_models$shortname[global_data$df_models$name == input$model]
  #dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]
  #df <- create_exposure_volcano_dfs(input$exp_class,dat)
  #create_exposure_volcano_plot(df)
})

})