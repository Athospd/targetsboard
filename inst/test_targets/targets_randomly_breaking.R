library(targets)
library(tarchetypes)
library(crew)
suppressPackageStartupMessages(library(tidymodels))

# Load required packages
tar_option_set(
  packages = c("tidymodels", "targetsboard"),
  error = "trim",
  controller = crew_controller_local(workers = 2)
)

probability_of_throwing_an_error <- function(p = 0.1) {
  if(runif(1) < p & as.integer(Sys.time()) %% 2 == 0) stop("Random Error occurred.")
}
p_error = 0.1
t = 2

targets <- list(

  # Data Preparation
  tar_target(data_raw, {
    Sys.sleep(2*t)
    probability_of_throwing_an_error(p_error)
    # Replace with your actual data loading code
    data <- iris
    data
  }),
  
  # Data Splitting
  tar_target(data_split, {
    Sys.sleep(2*t)
    probability_of_throwing_an_error(p_error)
    initial_split(data_raw, prop = 0.75)
  }),
  
  # Training and Testing Data
  tar_target(train_data, {Sys.sleep(2.5*t);training(data_split)}),
  tar_target(test_data, {Sys.sleep(1.5*t);testing(data_split)}),
  
  tar_target(train_test_ggplot, train_test_ggplot(train_data, test_data)),
  tar_target(train_test_plotly, train_test_plotly(train_data, test_data)),
  tar_quarto(train_test_report_qmd, "inst/test_quarto/train_test_report.qmd"),

  # Recipe
  tar_target(recipe, {
    Sys.sleep(2*t)
    # probability_of_throwing_an_error(p_error)
    # stop("Forced Error")
    recipe(Species ~ ., data = train_data) %>%
      step_normalize(all_numeric())
  }),
  
  # Model Specification
  tar_target(model_spec, {
    Sys.sleep(1*t)
    # probability_of_throwing_an_error(p_error)
    rand_forest(mtry = 2, trees = 50) %>%
      set_engine("ranger") %>%
      set_mode("classification")
  }),
  
  # Workflow
  tar_target(workflow, {
    Sys.sleep(2*t)
    # probability_of_throwing_an_error(p_error)
    workflow() %>%
      add_recipe(recipe) %>%
      add_model(model_spec)
  }),
  
  # Training the Model
  tar_target(trained_model, {
    Sys.sleep(2*t)
    # probability_of_throwing_an_error(p_error)
    fit(workflow, data = train_data)
  }),
  
  # Predictions
  tar_target(predictions, {
    Sys.sleep(3*t)
    # probability_of_throwing_an_error(p_error)
    predict(trained_model, test_data) %>%
      bind_cols(test_data)
  }),
  
  # Model Evaluation
  tar_target(evaluation, {
    Sys.sleep(2*t)
    # probability_of_throwing_an_error(p_error)
    dplyr::bind_rows(predictions, predictions) %>%
      metrics(truth = Species, estimate = .pred_class)
  })
)





tarchetypes::tar_hook_outer(targets,hook = suppressWarnings(.x))
