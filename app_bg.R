library(targetsboard)
script = "inst/test_targets/targets_randomly_breaking.R"
# metadata <- targets::tar_visnetwork(targets_only = TRUE, script = script)
# saveRDS(metadata, "metadata.rds")
metadata <- readRDS("metadata.rds")$x

# inicia o servico em plano de fundo
app_bg <- targetsboard::tar_board(metadata = metadata, script = script, port = 9999)
app_bg$is_alive() 

# roda o pipeline do targets a vontade
targets::tar_destroy(ask = FALSE)
targets::tar_make(script = script)


# desliga o app
app_bg$kill()
