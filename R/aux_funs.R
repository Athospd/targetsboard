#' @export
train_test_ggplot <- function(train_data, test_data) {
  dplyr::bind_rows(
    train_data |> dplyr::mutate(set = "train"),
    test_data |> dplyr::mutate(set = "test")
  ) |> 
    ggplot2::ggplot(ggplot2::aes(colour = set, fill = set, group = set)) +
      ggplot2::geom_density(ggplot2::aes(x = Sepal.Length), alpha = .6)
}
 
#' @export
train_test_plotly <- function(train_data, test_data) {
  p <- dplyr::bind_rows(
    train_data |> dplyr::mutate(set = "train"),
    test_data |> dplyr::mutate(set = "test")
  ) |>
    ggplot2::ggplot(ggplot2::aes(colour = Species, fill = Species)) +
      ggplot2::geom_point(ggplot2::aes(x = Sepal.Length, y = Sepal.Width), size = 3) +
      ggplot2::geom_smooth(ggplot2::aes(x = Sepal.Length, y = Sepal.Width), se = FALSE)
  
  plotly::ggplotly(p)
}