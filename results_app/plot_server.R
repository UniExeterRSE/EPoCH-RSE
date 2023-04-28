source("plot.R")

## When the user clicks "visualise results"...
observeEvent(input$plot_data,{
  ## If the data hasn't been loaded, they get an error message...
  if (global_data$data_is_loaded == FALSE) {
    showModal(modalDialog("No data has been loaded."))
  }
  else
  {

  output$exposureManhattanPlot <- renderPlotly({
    model <- global_data$df_models$shortname[global_data$df_models$name == input$model_choice]
    dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]

    exp_df <- create_exposure_dfs(tolower(input$exposure_choice),dat)
    filtered_df <- create_outcome_dfs(tolower(input$outcome_choice),exp_df)
    if (input$exposure_choice == "All" && input$outcome_choice == "All") {
      create_hl_exposure_manhattan_plotly(filtered_df, input$dimension[2]-110)
    } else {
      create_exposure_manhattan_plotly(filtered_df, input$dimension[2]-110)
    }
    })

  output$outcomeManhattanPlot <- renderPlotly({
    model <- global_data$df_models$shortname[global_data$df_models$name == input$model_choice]
    dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]

    outc_df <- create_outcome_dfs(tolower(input$outcome_choice),dat)
    filtered_df <- create_exposure_dfs(tolower(input$exposure_choice),outc_df)
    if (input$outcome_choice == "All" && input$exposure_choice == "All") {
      print(filtered_df)
      print(input$dimension[2]-110)
      create_hl_outcome_manhattan_plotly(filtered_df, input$dimension[2]-110)
    } else {
      create_outcome_manhattan_plotly(filtered_df, input$dimension[2]-110)
    }
    })

  output$exposureVolcanoPlot <- renderPlotly({
    model <- global_data$df_models$shortname[global_data$df_models$name == input$model_choice]
    dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]
    exp_df <- create_exposure_dfs(tolower(input$exposure_choice),dat)
    filtered_df <- create_outcome_dfs(tolower(input$outcome_choice),exp_df)
    create_volcano_plot(filtered_df)
    })

  output$outcomeVolcanoPlot <- renderPlotly({
    model <- global_data$df_models$shortname[global_data$df_models$name == input$model_choice]
    dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]
    outc_df <- create_outcome_dfs(tolower(input$outcome_choice),dat)
    filtered_df <- create_exposure_dfs(tolower(input$exposure_choice),outc_df)
    create_volcano_plot(filtered_df)
    })

  }

})