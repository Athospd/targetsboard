# torch values (#228): tensors and nn_module models stored with
# tarchetypes::tar_torch() (format = "torch"). A small classifier trained on
# two spirals, one model per learning rate, and tensors of each kind the
# Value pane shows: a scalar, ints, a bool mask, half precision, an image
# batch, and one too big to list.
#
# Needs torch, with its libraries (torch::install_torch(); CUDA = "cpu" for
# a CPU-only build). Everything runs on the CPU, in a few seconds.
#
#   Sys.setenv(TAR_CONFIG = "inst/examples/pipelines/_targets.yaml", TAR_PROJECT = "torch")
#   targets::tar_make()
#   targetsboard::tar_board()

library(targets)
library(tarchetypes)
library(torch) # nn_module() below

tar_option_set(packages = "torch")

# Two interleaved spirals, n points each: x is n*2 x 2, y their class, 1 or 2.
spirals <- function(n, noise = 0.15, seed = 42) {
  torch_manual_seed(seed) # the same points for x and y
  t <- torch_linspace(0.5, 3 * pi, n)
  arm <- function(sign) torch_stack(list(sign * t * torch_cos(t), sign * t * torch_sin(t)), dim = 2)
  x <- torch_cat(list(arm(1), arm(-1))) + noise * torch_randn(2 * n, 2)
  y <- torch_cat(list(torch_ones(n, dtype = torch_long()), torch_full(n, 2L, dtype = torch_long())))
  list(x = x, y = y)
}

mlp <- nn_module(
  "mlp",
  initialize = function(hidden = 32) {
    self$body <- nn_sequential(
      nn_linear(2, hidden), nn_relu(),
      nn_linear(hidden, hidden), nn_relu()
    )
    self$head <- nn_linear(hidden, 2)
  },
  forward = function(x) self$head(self$body(x))
)

train <- function(data, lr, epochs = 200, seed = 1) {
  torch_manual_seed(seed)
  model <- mlp()
  opt <- optim_adam(model$parameters, lr = lr)
  for (epoch in seq_len(epochs)) {
    opt$zero_grad()
    loss <- nnf_cross_entropy(model(data$x), data$y)
    loss$backward()
    opt$step()
  }
  model$eval()
  model
}

accuracy <- function(model, data) {
  with_no_grad(as.numeric((model(data$x)$argmax(dim = 2) == data$y)$to(dtype = torch_float())$mean()))
}

list(
  tar_torch(x, spirals(200)$x),
  tar_torch(y, spirals(200)$y),

  # One model per learning rate: a pattern of nn_modules.
  tar_target(lr, c(0.001, 0.01, 0.05)),
  tar_torch(models, train(list(x = x, y = y), lr), pattern = map(lr), iteration = "list"),
  tar_target(scores, data.frame(lr = lr, accuracy = accuracy(models, list(x = x, y = y))), pattern = map(lr, models)),

  # The best model, and its predictions as a tensor.
  tar_torch(best, models[[which.max(scores$accuracy)]]),
  tar_torch(predictions, with_no_grad(nnf_softmax(best(x), dim = 2))),

  # A model never trained, with more kinds of layers.
  tar_torch(convnet, nn_sequential(
    nn_conv2d(3, 16, kernel_size = 3, padding = 1), nn_batch_norm2d(16), nn_relu(), nn_max_pool2d(2),
    nn_conv2d(16, 32, kernel_size = 3, padding = 1), nn_relu(), nn_adaptive_avg_pool2d(1),
    nn_flatten(), nn_dropout(0.2), nn_linear(32, 10)
  )),

  # Tensors of each kind.
  tar_torch(loss_scalar, torch_tensor(0.3125)),
  tar_torch(class_counts, torch_bincount(y)),
  tar_torch(mask, x[, 1] > 0),
  tar_torch(half, torch_randn(4, 4)$to(dtype = torch_float16())),
  tar_torch(images, torch_rand(8, 3, 32, 32)),
  tar_torch(big, torch_randn(1000, 1000))
)
