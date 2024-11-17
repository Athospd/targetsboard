library(targetsboard)
# inicia o servico em plano de fundo
app_bg <- tar_board()
app_bg$is_alive() 

 

# roda o pipeline do targets a vontade
targets::tar_destroy(ask = FALSE)
targets::tar_make(script = "inst/test_targets/targets_randomly_breaking.R")


# desliga o app
app_bg$kill()
