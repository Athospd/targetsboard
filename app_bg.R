library(targetsboard)
script = "inst/test_targets/targets_randomly_breaking.R"

# app running in background
app_bg <- targetsboard::tar_board(script, targets_only = TRUE)
app_bg$is_alive() 

# R is free to run tar_* as we like
targets::tar_destroy(ask = FALSE)
targets::tar_make(script = script)


# desliga o app
app_bg$kill()
