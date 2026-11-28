# torch values, from tar_torch() (#228): a module's layers and parameters,
# a tensor's shape and first values. torch isn't a dependency: these fakes
# carry the methods the views call, as torch's R6 objects do. The views
# were checked against real torch by hand (see the PR).

fake_tensor <- function(shape, values = seq_len(prod(shape)) / 10, dtype = "Float", grad = TRUE) {
  x <- new.env()
  x$shape <- as.integer(shape)
  x$dtype <- dtype
  x$device <- list(type = "cpu")
  x$requires_grad <- grad
  x$numel <- function() prod(shape)
  x$cpu <- function() list(flatten = function() values)
  class(x) <- c("torch_tensor", "R7")
  x
}

fake_module <- function(class, parameters = list(), children = list()) {
  m <- new.env()
  # As torch: a module's parameters are its own and its children's.
  own <- parameters
  for (child in names(children)) {
    p <- children[[child]]$parameters
    if (length(p)) own[paste0(child, ".", names(p))] <- p
  }
  m$parameters <- own
  m$children <- children
  class(m) <- c(class, "nn_module")
  m
}

test_that("a tensor shows its shape, dtype, device and first values", {
  views <- tar_board_preview(fake_tensor(c(3, 4)), NULL)
  html <- as.character(views$Tensor)
  expect_match(html, "3 × 4", fixed = TRUE)
  expect_match(html, "Float", fixed = TRUE)
  expect_match(html, "cpu", fixed = TRUE)
  expect_match(html, "12 elements", fixed = TRUE)
  expect_match(html, "0.1, 0.2, 0.3", fixed = TRUE)
  # A big one: its first values only.
  html <- as.character(tar_board_preview(fake_tensor(c(1000, 1000), values = rep(1, 1e6)), NULL)$Tensor)
  expect_match(html, "The first 20 of 1,000,000 values", fixed = TRUE)
})

test_that("a module shows its layers as a tree, and its parameters", {
  net <- fake_module("net", children = list(
    fc1 = fake_module("nn_linear", list(weight = fake_tensor(c(8, 4)), bias = fake_tensor(8))),
    act = fake_module("nn_relu"),
    fc2 = fake_module("nn_linear", list(weight = fake_tensor(c(1, 8)), bias = fake_tensor(1, grad = FALSE)))
  ))
  views <- tar_board_preview(net, NULL)
  expect_named(views, c("Module", "Parameters"))
  html <- as.character(views$Module)
  expect_match(html, "net", fixed = TRUE)
  expect_match(html, "49 parameters, 48 trainable", fixed = TRUE)
  expect_match(html, '<code class="tb-tree-name">fc1</code>', fixed = TRUE)
  expect_match(html, '<span class="tb-tree-type">nn_relu</span>', fixed = TRUE)
  expect_match(html, "40 parameters", fixed = TRUE) # fc1's
  html <- htmltools::renderTags(views$Parameters)$html
  expect_match(html, "fc1.weight", fixed = TRUE)
  expect_match(html, "8 × 4", fixed = TRUE)
})

test_that("a value whose format's package isn't installed says which to install", {
  local_mocked_bindings(is_installed = function(pkg, ...) FALSE, .package = "rlang")
  expect_match(missing_format_package("torch"), "torch model or tensor: install the torch package", fixed = TRUE)
  expect_match(missing_format_package("keras"), "install the keras package", fixed = TRUE)
  expect_match(missing_format_package("parquet"), "install the arrow package", fixed = TRUE)
  expect_match(missing_format_package("fst_tbl"), "install the tibble and fst packages", fixed = TRUE)
  expect_null(missing_format_package("rds"))
  expect_null(missing_format_package(NULL))
  local_mocked_bindings(is_installed = function(pkg, ...) TRUE, .package = "rlang")
  expect_null(missing_format_package("torch"))
})

test_that("torch and keras targets are kinds with their own line", {
  kinds <- target_kinds(data.frame(name = c("m", "k", "t"), format = c("torch", "keras", "rds"), command = ""))
  expect_equal(kinds, c(m = "format:torch", k = "format:keras"))
  expect_match(kind_hint("format:torch"), "torch model or tensor (tar_torch())", fixed = TRUE)
  expect_match(kind_hint("format:keras"), "keras model (tar_keras())", fixed = TRUE)
})

test_that("the missing package's card keeps the target's metadata", {
  target <- list(name = "model", format = "torch", bytes = 378400, time = Sys.time())
  html <- as.character(missing_package_card(target, "install the torch package"))
  expect_match(html, "install the torch package", fixed = TRUE)
  expect_match(html, "torch format", fixed = TRUE)
  expect_match(html, "378.4 kB", fixed = TRUE)
})
