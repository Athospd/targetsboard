library(targets)
library(tarchetypes)
library(crew)
suppressPackageStartupMessages(library(tidymodels))
f <- glue::glue

# Load required packages
tar_option_set(
  packages = c("targetsboard", "rlang"),
  error = "trim",
  controller = crew_controller_local(workers = 2)
)


foo_input <- function(id, inputs) {
  n_zeros <- stringr::str_length(as.character(inputs))
  input_nums <- stringr::str_pad(1:inputs, n_zeros, "left", "0")

  nodes = data.frame(
    id = f("{id}_w{input_nums}")
  )

  edges <- data.frame(
    from = "root", 
    to = nodes$id, 
    arrow = "to",
    stringsAsFactors = FALSE
  )

  current = list(nodes = nodes, edges = edges)
  return(list(current = current, cummulative = current))
}

foo <- function(source, id, outputs) {
  n_zeros <- stringr::str_length(as.character(outputs))
  output_nums <- stringr::str_pad(1:outputs, n_zeros, "left", "0")

  nodes  = data.frame(
    id = f("{id}_w{output_nums}")
  )

  edges <- expand.grid(
    from = source$current$nodes$id, 
    to = nodes$id, 
    arrow = "to",
    stringsAsFactors = FALSE
  ) 

  current = list(nodes = nodes, edges = edges)
  cummulative <- list(
    nodes = dplyr::bind_rows(source$cummulative$nodes, nodes),
    edges = dplyr::bind_rows(source$cummulative$edges, edges)
  )
  
  return(list(current = current, cummulative = cummulative))
}






library(magrittr)
fully_connected <- foo_input("a", inputs = 5) %>%
  foo("b", outputs = 4) %>%
  foo("h3", outputs = 4) %>%
  foo("h4", outputs = 4) %>%
  foo("h5", outputs = 4) %>%
  foo("h6", outputs = 4) %>%
  foo("h7", outputs = 4) %>%
  foo("hd", outputs = 4) %>%
  foo("hf", outputs = 4) %>%
  foo("hg", outputs = 4) %>%
  foo("hx", outputs = 4) %>%
  foo("hc", outputs = 4) %>%
  foo("hv", outputs = 4) %>%
  foo("ib", outputs = 1)

gera_target <- function(from, to) {
  from <- purrr::map(from, as.symbol)
  expr(targets::tar_target(name = !!to, command = dummy_command(!!!from)))
}

dummy_command <- function(...) {
  Sys.sleep(1 + rexp(1, 2))
  dots <- enquos(...)
  lapply(dots, quo_text)
}

target_list <- fully_connected$cummulative$edges %>% 
  group_by(to) %>%
    reframe(
    from = list(from)
  ) %>%
  mutate(
    target = purrr::map2(from, to, gera_target)
  )

target_list$target %>% map(eval) 
