source("data.R")

# when the user clicks the button to load the data...
loaded_data <- observeEvent(input$load_results,{
  # the data will load from dropbox (with a message explaining what's happening)
  showModal(modalDialog("Loading data"))
  loaded_data <- import_data_local(global_data)

  # And the rest of the input options will be updated based on the data
  updateRadioButtons(session, "exp_class",
                     choices = loaded_data$exp_classes)
  updateRadioButtons(session, "out_class",
                     choices = loaded_data$out_classes)
  updateRadioButtons(session, "n_comparisons",
                     choices = 1:4)

  # The message about loading the data will be removed
  removeModal()

  #Generate a dataframe matching the model names to their IDs (essentially "Model 1a" to "model1a" etc)
  global_data$df_models <- data.frame(name=paste0("Model ",as.vector(outer(1:4, letters[1:3], paste0))),
                        shortname=paste0("model",as.vector(outer(1:4, letters[1:3], paste0)))
                                     )
  # And loaded_data will be returned for use elsewhere in the app
  global_data$data <- loaded_data
})
