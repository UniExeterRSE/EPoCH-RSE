source("data.R")

# when the user clicks the button to load the data...
loaded_data <- observeEvent(input$load_results,{
  # the data will load from dropbox (with a message explaining what's happening)
  showModal(modalDialog("Loading data"))
  loaded_data <- import_data_local(global_data)

  #Generate a dataframe matching the model names to their IDs (essentially "Model 1a" to "model1a" etc)
  model_n = 1 # was 4
  model_l = 2 # was 3
  global_data$df_models <- data.frame(name=paste0("Model ",as.vector(outer(1:model_n, letters[1:model_l], paste0))),
                        shortname=paste0("model",as.vector(outer(1:model_n, letters[1:model_l], paste0)))
                                     )

  updateSelectizeInput(inputId = "exposure_choice", selected = 'All',
                       choices = str_to_sentence(append(global_data$exp_classes, 'All', after=0)))

  updateSelectizeInput(inputId = "outcome_choice", selected = 'All',
                       choices = str_to_sentence(append(global_data$out_classes, 'All', after=0)))

  updateSelectizeInput(inputId = "model_choice", choices = global_data$df_models$name,
                       selected = global_data$df_models$name[1])

  # And the rest of the input options will be updated based on the data
  updateRadioButtons(session, "exp_class",
                     choices = loaded_data$exp_classes)
  updateRadioButtons(session, "out_class",
                     choices = loaded_data$out_classes)
  updateRadioButtons(session, "n_comparisons",
                     choices = 1:4)

  # The message about loading the data will be removed
  removeModal()

  # And loaded_data will be returned for use elsewhere in the app
  global_data$data_is_loaded = TRUE
  global_data$data <- loaded_data
})
