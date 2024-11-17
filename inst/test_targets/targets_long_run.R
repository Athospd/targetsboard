library(targets)
library(tidymodels)
library(tarchetypes)

# Load required packages
tar_option_set(packages = c("tidymodels", "dplyr"))

list(

  # Data Preparation
  tar_target(data_raw, {
    Sys.sleep(4)
    # Replace with your actual data loading code
    data <- iris
    data
  }),
  
  # Data Splitting
  tar_target(data_split, {
    Sys.sleep(4)
    initial_split(data_raw, prop = 0.75)
  }),
  
  # Training and Testing Data
  tar_target(train_data, training(data_split)),
  tar_target(test_data, testing(data_split)),
  
  # Recipe
  tar_target(recipe, {
    Sys.sleep(6)
    recipe(Species ~ ., data = train_data) %>%
      step_normalize(all_predictors())
  }),
  
  # Model Specification
  tar_target(model_spec, {
    Sys.sleep(5)
    rand_forest(mtry = 2, trees = 100) %>%
      set_engine("ranger") %>%
      set_mode("classification")
  }),
  
  # Workflow
  tar_target(workflow, {
    Sys.sleep(4)
    workflow() %>%
      add_recipe(recipe) %>%
      add_model(model_spec)
  }),
  
  # Training the Model
  tar_target(trained_model, {
    Sys.sleep(4)
    fit(workflow, data = train_data)
  }),
  
  # Predictions
  tar_target(predictions, {
    Sys.sleep(6)
    predict(trained_model, test_data) %>%
      bind_cols(test_data)
  }),
  
  # Model Evaluation
  tar_target(evaluation, {
    Sys.sleep(8)
    dplyr::bind_rows(predictions, predictions) %>%
      metrics(truth = Species, estimate = .pred_class)
  })
)
