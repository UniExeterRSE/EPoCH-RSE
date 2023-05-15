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
    if (input$exposure_choice == "All") {
      create_manhattan_plot(filtered_df, input$dimension[2]-110,
                            "exposure_class", "Exposure class")
    } else {
      create_manhattan_plot(filtered_df, input$dimension[2]-110,
                            "exposure_subclass_time_dose", "Exposure type")
    }
    })

  output$outcomeManhattanPlot <- renderPlotly({
    model <- global_data$df_models$shortname[global_data$df_models$name == input$model_choice]
    dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]

    outc_df <- create_outcome_dfs(tolower(input$outcome_choice),dat)
    filtered_df <- create_exposure_dfs(tolower(input$exposure_choice),outc_df)
    if (input$outcome_choice == "All") {
      create_manhattan_plot(filtered_df, input$dimension[2]-110,
                            "outcome_class", "Outcome class")
    } else {
      create_manhattan_plot(filtered_df, input$dimension[2]-110,
                            "outcome_subclass_time", "Outcome type")
    }
    })

  output$exposureVolcanoPlot <- renderPlotly({
    model <- global_data$df_models$shortname[global_data$df_models$name == input$model_choice]
    dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]
    exp_df <- create_exposure_dfs(tolower(input$exposure_choice),dat)
    filtered_df <- create_outcome_dfs(tolower(input$outcome_choice),exp_df)
    p_m <- create_volcano_plot(filter(filtered_df, person_exposed=="mother"))
    p_f <- create_volcano_plot(filter(filtered_df, person_exposed=="father"))
    subplot(p_m, p_f, shareY = TRUE, titleX = TRUE)%>%
      layout(xaxis = list(title = "Standardised effect estimate",
                         range = list(-0.75, 0.75)),
             yaxis = list(title = "Ranked -log10(P)"))
    })

  output$outcomeVolcanoPlot <- renderPlotly({
    model <- global_data$df_models$shortname[global_data$df_models$name == input$model_choice]
    dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]
    outc_df <- create_outcome_dfs(tolower(input$outcome_choice),dat)
    filtered_df <- create_exposure_dfs(tolower(input$exposure_choice),outc_df)
    p_m <- create_volcano_plot(filter(filtered_df, person_exposed=="mother"))
    p_f <- create_volcano_plot(filter(filtered_df, person_exposed=="father"))
    subplot(p_m, p_f, shareY = TRUE, titleX = TRUE)%>%
      layout(xaxis = list(title = "Standardised effect estimate",
                         range = list(-0.75, 0.75)),
             yaxis = list(title = "Ranked -log10(P)"))
    })

  }

})