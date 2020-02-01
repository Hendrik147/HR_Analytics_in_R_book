#' Predicted vs actuals charts
#'
#' @param df data.frame containing actual, predicted, and gender
#'
#' @return chart
#' @export
#'
#' @examples
#' pred_vs_actuals(data.frame(actual=1, predicted=1, gender="Female"))
pred_vs_actuals <- function(df) {
  df %>%
    ggplot2::ggplot() +
    ggplot2::aes(x=actual, y=predicted, colour=gender) +
    ggplot2::geom_abline(slope=1,intercept=0, colour="blue") +
    ggplot2::geom_point(alpha=.7) +
    ggplot2::facet_wrap(~gender) +
    ggplot2::theme_minimal()
}
