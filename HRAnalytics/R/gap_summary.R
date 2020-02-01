#' Get mean, median, and count where a column has values,
#' split by a grouping factor
#'
#' @param df Table to be analysed
#' @param group Column to be grouped
#' @param column Column to be aggregated
#'
#' @return tibble Summarising results
#' @export
#'
#' @examples
#' gap_summary(iris, Species, Sepal.Width)
gap_summary <- function(df, group, column) {
  col = rlang::ensym(column)
  g = rlang::ensym(group)
  df %>%
    dplyr::filter(!is.na(!!col)) %>%
    dplyr::group_by(!!g) %>%
    dplyr::summarise(
      mean = mean(!!col),
      median = median(!!col) ,
      count = dplyr::n()
    )
}
